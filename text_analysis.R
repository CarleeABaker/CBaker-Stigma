#SCOTT NOTES
test <- all_results |> 
  mutate(discipline = case_when(
    journal %in% c("Emergencias", "World Journal of Emergency Surgery", "Annals of Emergency Medicine", "Resuscitation", "European Journal of Emergency Medicine", "Academic Emergency Medicine", "World Journal of Emergency Medicine", "Scandinavian Journal of Trauma Resuscitation & Emergency Medicine", "Burns & Trauma", "Emergency Medicine Journal", "BMC Emergency Medicine", "Prehospital and Disaster Medicine", "Resuscitation Plus", "Journal of Emergency Medicine", "Clinical and Experimental Emergency Medicine", "Turkish Journal of Emergency Medicine") ~ "EMS",
    .default = "Other"
  ))

journal_lookup <- all_journals |> 
  select(journal_title, category)

test_join <- all_results |> 
  left_join(journal_lookup, by = join_by (tolower(journal) == tolower(journal_title)))
#-------------------------------------------------------------------------------------------------

#preflight
library(tidyverse)
library(xml2)
library(tidytext)
library(dplyr)
library(textstem)
library(stringr)
library(readr)

#fix all of the weird issues with sentence case coming from PubMed. remove ampersands, white space, weird punctuation and lower everything in both dfs
clarivate <- read_csv("all_journals.csv")
pubmed_results <- read.csv("all_results.csv")

fix_journal_title <- function(x) {
  x |> 
    str_to_lower() |> 
    str_replace_all("&", " and ") |> 
    str_replace_all("[[:punct:]]", " ") |> 
    str_squish()
}

clarivate <- clarivate |> 
  mutate(join_key = fix_journal_title(journal_title))
pubmed_results<- pubmed_results |> 
  mutate(join_key = fix_journal_title(journal))

joined_df <- pubmed_results |> 
  left_join(
    clarivate |> 
      select(join_key, category),
    by = "join_key"
  ) |> 
  select(journal, abstract, category)

#PART 2: Text Analytics
all_results <- read.csv("all_results.csv")

#read in keywords
#import list of substance terms
drug_terms <- read_csv("drug_keywords.csv")

#paste into a regex query
drug_terms_regex <- paste0(drug_terms$term,collapse = "|")

#create regixify function that stems and lemmatizes the terms and then creates regex query
regexify <- function(x){
  stems <- stem_words(x)
  lemmas <- lemmatize_words(x)
  c(stems,lemmas,x) %>% unique %>% 
    paste0(.,collapse = "|") %>% 
    tolower()
}

#then apply the regex query to the substance terms
drug_terms_better_regex <- regexify(drug_terms$term)

#trigram analysis with substance keyterms
sub_keyword_framegrams <- all_results %>%
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram, drug_terms_better_regex)) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(sub_keyword_framegrams, "sub_keyword_framegrams.csv")

#same thing for rural keywords, but with raw count instead of trigrams
rural_terms <- read_csv("rural_keywords.csv")

rural_terms_regex <- paste0(rural_terms$term,collapse = "|")

rural_terms_better_regex <- regexify(rural_terms$term)

#raw count with keyterms
rural_keyword_counts <- joined_df_standard %>%
  unnest_tokens(word, abstract) %>%
  filter(str_detect(word, rural_terms_better_regex)) %>%
  separate_rows(category, sep = "\\s*[,]\\s*") |> 
  mutate(category = str_squish(category)) |> 
  filter(category != "") |> 
  count(category, word, sort = TRUE) %>%
  group_by(category) |> 
  mutate(word = reorder_within(word, n, category)) |> 
  ungroup()

write.csv(rural_keyword_raw, "rural_keyword_raw.csv")

# View most common words excluding stop words
raw_count <- all_results %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

write.csv(raw_count, "raw_count.csv")

#framegrams testing
raw_framegrams <- all_results %>%
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(raw_framegrams, "raw_framegrams.csv")

#analysis according to clarivate categories. I'm sure there's a more elegant way to do this but i automated the formatting in excel and it took almost no time to just copy and paste so ¯\_(ツ)_/¯

#EMERGENCY MEDICINE: weird as f
emerg_med_results <- joined_df |> 
  select(abstract, category) |> 
  filter(str_detect(category, "Emergency Medicine"))

collapsed_ems_abstracts <- paste(joined_df$abstract, collapse = " ")
    
ems_trigram_df <- tibble(abstract = collapsed_ems_abstracts) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n = 3) |>
  filter(str_detect(trigram, drug_terms_better_regex)) |>
  count(trigram, sort = TRUE)

write.csv(ems_trigram_df, "ems_trigram_keywords.csv")

ems_tf_idf <- ems_trigram_df |> 
  bind_tf_idf(trigram, trigram, n)

#without keywords
emerg_med_results <- all_results |> 
  filter(journal %in% c("Emergencias", "World Journal of Emergency Surgery", "Annals of Emergency Medicine", "Resuscitation", "European Journal of Emergency Medicine", "Academic Emergency Medicine", "World Journal of Emergency Medicine", "Scandinavian Journal of Trauma Resuscitation & Emergency Medicine", "Burns & Trauma", "Emergency Medicine Journal", "BMC Emergency Medicine", "Prehospital and Disaster Medicine", "Resuscitation Plus", "Journal of Emergency Medicine", "Clinical and Experimental Emergency Medicine", "Turkish Journal of Emergency Medicine")) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(emerg_med_results, "emerg_med_frames.csv")

#BEHAVIORAL HEALTH
behav_health_results_keyword <- all_results |> 
  filter(journal %in% c("Trends in Cognitive Sciences",
"Neuroscience and Biobehavioral Reviews",
"Behavioral and Brain Sciences",
"Current Opinion in Behavioral Sciences",
"Appetite",
"Autism Research",
"Stress-The International Journal on the Biology of Stress",
"Cognitive Affective & Behavioral Neuroscience",
"Human Factors",
"Frontiers in Behavioral Neuroscience",
"Biological Psychology",
"Current Research in Behavioral Sciences",
"Cortex",
"Frontiers in Integrative Neuroscience"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(behav_health_results_keyword, "behav_health_frames_keyword.csv")

behav_health_results <- all_results |> 
  filter(journal %in% c("Trends in Cognitive Sciences",
                        "Neuroscience and Biobehavioral Reviews",
                        "Behavioral and Brain Sciences",
                        "Current Opinion in Behavioral Sciences",
                        "Appetite",
                        "Autism Research",
                        "Stress-The International Journal on the Biology of Stress",
                        "Cognitive Affective & Behavioral Neuroscience",
                        "Human Factors",
                        "Frontiers in Behavioral Neuroscience",
                        "Biological Psychology",
                        "Current Research in Behavioral Sciences",
                        "Cortex",
                        "Frontiers in Integrative Neuroscience"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(behav_health_results, "behav_health_results.csv")

#CLINICAL NEUROLOGY
clin_neuro_results_keyword <- all_results |> 
  filter(journal %in% c("Lancet Neurology",
"Nature Reviews Neurology",
"JAMA Neurology",
"Neuro-Oncology",
"Brain",
"Alzheimers & Dementia",
"Sleep Medicine Reviews",
"Acta Neuropathologica",
"Neurology",
"Stroke",
"International Journal of Stroke",
"Journal of Stroke",
"Neurology-Neuroimmunology & Neuroinflammation",
"Brain Stimulation",
"Journal of Headache and Pain",
"Journal of Neurology Neurosurgery and Psychiatry",
"JPAD-Journal of Prevention of Alzheimers Disease",
"Annals of Neurology",
"Alzheimers Research & Therapy",
"Movement Disorders",
"CNS Drugs",
"Neurotherapeutics",
"Alzheimers & Dementia-Translational Research & Clinical Interventions",
"European Neuropsychopharmacology",
"Epilepsia",
"Epilsepsy Currents",
"Brain Pathology",
"Psychiatry and Clinical Neurosciences",
"Pain",
"Journal of Psychopharmacology",
"Current Neurology and Neuroscience Reports",
"Multiple Sclerosis Journal",
"Journal of Affective Disorders",
"Sleep",
"Stroke and Vascular Neurology",
"Sleep Medicine Clinics",
"Neurology and Therapy",
"Brain Circulation",
"Spine Journal",
"Cephalagia", 
"Journal of Neurology",
"European Stroke Journal",
"Brain Communications",
"Bipolar Disorders",
"Alzheimer's & Dementia: Diagnosis, Assessment, & Disease Monitoring",
"Current Opinion on Neurology",
"Developmental Medicine and Child Neurology",
"Translational Stroke Research",
"Neruology-Genetics",
"Journal of Neurologic Physical Therapy",
"Therapeutic Advances in Neurological Disorders",
"Neuro-Oncology Advances",
"CNS Spectrums",
"Headache",
"Journal of Pain",
"Journal of Neurodevelopmental Disorders",
"Neuroepidemiology",
"Journal of Neural Transmission",
"Progress in Neuro-Psychopharmacology & Biological Psychiatry",
"Neurosurgery",
"Annals of Clinical and Translational Neurology",
"European Journal of Neurology",
"Journal of Sleep Research",
"Neuroscientist",
"Journal of Neurotrauma",
"Neurorehabilitation and Neural Repair",
"International Journal of Neuropsychopharmacology",
"American Journal of Neuroradiology",
"European Archives of Psychiatry and Clinical Neuroscience",
"Journal of Neurosurgery",
"Neurospine",
"Clinical Neurophysiology",
"Neurocritical Care")) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(clin_neuro_results_keyword, "clin_neuro_frames_keyword.csv")

clin_neuro_results <- all_results |> 
  filter(journal %in% c("Lancet Neurology",
                        "Nature Reviews Neurology",
                        "JAMA Neurology",
                        "Neuro-Oncology",
                        "Brain",
                        "Alzheimers & Dementia",
                        "Sleep Medicine Reviews",
                        "Acta Neuropathologica",
                        "Neurology",
                        "Stroke",
                        "International Journal of Stroke",
                        "Journal of Stroke",
                        "Neurology-Neuroimmunology & Neuroinflammation",
                        "Brain Stimulation",
                        "Journal of Headache and Pain",
                        "Journal of Neurology Neurosurgery and Psychiatry",
                        "JPAD-Journal of Prevention of Alzheimers Disease",
                        "Annals of Neurology",
                        "Alzheimers Research & Therapy",
                        "Movement Disorders",
                        "CNS Drugs",
                        "Neurotherapeutics",
                        "Alzheimers & Dementia-Translational Research & Clinical Interventions",
                        "European Neuropsychopharmacology",
                        "Epilepsia",
                        "Epilsepsy Currents",
                        "Brain Pathology",
                        "Psychiatry and Clinical Neurosciences",
                        "Pain",
                        "Journal of Psychopharmacology",
                        "Current Neurology and Neuroscience Reports",
                        "Multiple Sclerosis Journal",
                        "Journal of Affective Disorders",
                        "Sleep",
                        "Stroke and Vascular Neurology",
                        "Sleep Medicine Clinics",
                        "Neurology and Therapy",
                        "Brain Circulation",
                        "Spine Journal",
                        "Cephalagia", 
                        "Journal of Neurology",
                        "European Stroke Journal",
                        "Brain Communications",
                        "Bipolar Disorders",
                        "Alzheimer's & Dementia: Diagnosis, Assessment, & Disease Monitoring",
                        "Current Opinion on Neurology",
                        "Developmental Medicine and Child Neurology",
                        "Translational Stroke Research",
                        "Neruology-Genetics",
                        "Journal of Neurologic Physical Therapy",
                        "Therapeutic Advances in Neurological Disorders",
                        "Neuro-Oncology Advances",
                        "CNS Spectrums",
                        "Headache",
                        "Journal of Pain",
                        "Journal of Neurodevelopmental Disorders",
                        "Neuroepidemiology",
                        "Journal of Neural Transmission",
                        "Progress in Neuro-Psychopharmacology & Biological Psychiatry",
                        "Neurosurgery",
                        "Annals of Clinical and Translational Neurology",
                        "European Journal of Neurology",
                        "Journal of Sleep Research",
                        "Neuroscientist",
                        "Journal of Neurotrauma",
                        "Neurorehabilitation and Neural Repair",
                        "International Journal of Neuropsychopharmacology",
                        "American Journal of Neuroradiology",
                        "European Archives of Psychiatry and Clinical Neuroscience",
                        "Journal of Neurosurgery",
                        "Neurospine",
                        "Clinical Neurophysiology",
                        "Neurocritical Care")) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(clin_neuro_results, "clin_neuro_results.csv")

#CRITICAL CARE
crit_care_results_keyword <- all_results |> 
  filter(journal %in% c("Lancet Respiratory Medicine",
"Intensive Care Medicine",
"American Journal of Respiratory and Critical Care Medicine",
"Critical Care",
"Chest",
"Critical Care Medicine",
"Annals of Intensive Care",
"Intensive and Critical Care Nursing",
"Anaesthesia Critical Care & Pain Medicine",
"Journal of Intensive Care",
"Pediatric Critical Care Medicine",
"Journal of Trauma and Acute Care Surgery")) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(crit_care_results_keyword, "crit_care_frames_keyword.csv")

crit_care_results <- all_results |> 
  filter(journal %in% c("Lancet Respiratory Medicine",
                        "Intensive Care Medicine",
                        "American Journal of Respiratory and Critical Care Medicine",
                        "Critical Care",
                        "Chest",
                        "Critical Care Medicine",
                        "Annals of Intensive Care",
                        "Intensive and Critical Care Nursing",
                        "Anaesthesia Critical Care & Pain Medicine",
                        "Journal of Intensive Care",
                        "Pediatric Critical Care Medicine",
                        "Journal of Trauma and Acute Care Surgery")) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(crit_care_results, "crit_care_results.csv")

#ENDOCRINOLOGY AND METABOLISM
endo_met_results_keyword <- all_results |> 
  filter(journal %in% c("Nature Reviews Endocrinology",
"Lancet Diabetes & Endocrinology",
"Cell Metabolism",
"Endocrine Reviews",
"Nature Metabolism",
"Diabetes Care",
"Endocrine Pathology",
"Trends in Endocrinology and Metabolism",
"Metabolism-Clinical and Experimental",
"Current Obesity Reports",
"Cardiovascular Diabetology",
"Diabetologia",
"Diabetes & Metabolism Journal",
"Free Radical Biology and Medicine",
"Reviews in Endocrine & Metabolic Disorders",
"Journal of Obesity & Metabolic Syndrome",
"Diabetes",
"Diabetes Research and Clinical Practice",
"Obesity Reviews",
"Thyroid",
"Frontiers in Neuroendocrinology",
"Molecular Metabolism",
"Current Diabetes Reports",
"Journal of Pineal Research",
"Diabetes Technology & Therapeutics",
"Best Practice & Research Clinical Endocrinology & Metabolism",
"Antioxidants & Redox Signaling",
"Diabetes-Metabolism Research and Reviews",
"Journal of Bone and Mineral Research",
"Diabetes Obesity & Metabolism",
"Pediatric Diabetes",
"Osteoporosis International",
"Current Osteoporosis Reports",
"European Journal of Endocrinology",
"Nutrition & Diabetes",
"Journal of Clinical Endocrinology & Metabolism",
"Biology of Sex Differences",
"Biofactors",
"Diabetes & Metabolism",
"Reproductive Biology and Endocrinology",
"Obesity",
"Obesity Facts",
"Endocrine Practice",
"Frontiers in Endocrinology",
"Endocrine-Related Cancer",
"Therapeutic Advances in Endocrinology and Metabolism",
"World Journal of Diabetes",
"Current Opinion in Lipidology")) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(endo_met_results_keyword, "endo_met_frames_keyword.csv")

endo_met_results <- all_results |> 
  filter(journal %in% c("Nature Reviews Endocrinology",
                        "Lancet Diabetes & Endocrinology",
                        "Cell Metabolism",
                        "Endocrine Reviews",
                        "Nature Metabolism",
                        "Diabetes Care",
                        "Endocrine Pathology",
                        "Trends in Endocrinology and Metabolism",
                        "Metabolism-Clinical and Experimental",
                        "Current Obesity Reports",
                        "Cardiovascular Diabetology",
                        "Diabetologia",
                        "Diabetes & Metabolism Journal",
                        "Free Radical Biology and Medicine",
                        "Reviews in Endocrine & Metabolic Disorders",
                        "Journal of Obesity & Metabolic Syndrome",
                        "Diabetes",
                        "Diabetes Research and Clinical Practice",
                        "Obesity Reviews",
                        "Thyroid",
                        "Frontiers in Neuroendocrinology",
                        "Molecular Metabolism",
                        "Current Diabetes Reports",
                        "Journal of Pineal Research",
                        "Diabetes Technology & Therapeutics",
                        "Best Practice & Research Clinical Endocrinology & Metabolism",
                        "Antioxidants & Redox Signaling",
                        "Diabetes-Metabolism Research and Reviews",
                        "Journal of Bone and Mineral Research",
                        "Diabetes Obesity & Metabolism",
                        "Pediatric Diabetes",
                        "Osteoporosis International",
                        "Current Osteoporosis Reports",
                        "European Journal of Endocrinology",
                        "Nutrition & Diabetes",
                        "Journal of Clinical Endocrinology & Metabolism",
                        "Biology of Sex Differences",
                        "Biofactors",
                        "Diabetes & Metabolism",
                        "Reproductive Biology and Endocrinology",
                        "Obesity",
                        "Obesity Facts",
                        "Endocrine Practice",
                        "Frontiers in Endocrinology",
                        "Endocrine-Related Cancer",
                        "Therapeutic Advances in Endocrinology and Metabolism",
                        "World Journal of Diabetes",
                        "Current Opinion in Lipidology")) |> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(endo_met_results, "endo_met_results.csv")

#GENETICS AND HEREDITY
gene_here_results_keyword <- all_results |> 
  filter(journal %in% c("Nature Reviews Genetics",
                        "Nature Genetics",
                        "Trends in Ecology & Evolution",
                        "Trends in Genetics",
                        "Molecular Therapy",
                        "Genome Medicine",
                        "Genome Biology",
                        "Genes & Diseases",
                        "Cell Genomics",
                        "Annual Review of Genetics",
                        "Horticulture Research",
                        "American Journal of Human Genetics",
                        "Genomics Proteomics & Bioinformatics",
                        "Annual Review of Genomics and Human Genetics",
                        "Genes & Development",
                        "Oncogene",
                        "Journal of Genetics and Genomics",
                        "Genetics in Medicine",
                        "Phenomics",
                        "Molecular Autism",
                        "Genome Research",
                        "Circulation-Genomic and Precision Medicine",
                        "Environmental Microbiome",
                        "Molecular Biology and Evolution",
                        "Genetics",
                        "Cancer Gene Therapy",
                        "Genes and Nutrition",
                        "npj Genomic Medicine",
                        "European Journal of Human Genetics",
                        "Gene Therapy",
                        "Genes and Immunity",
                        "Mitochondrion",
                        "Clinical Epigenetics",
                        "Molecular Diagnosis & Therapy",
                        "Frontiers in Genome Editing",
                        "American Journal of Medical Genetics Part C-Seminars in Medical Genetics",
                        "Human Genomics",
                        "Neurology-Genetics",
                        "Mutagenesis",
                        "Theoretical and Applied Genetics",
                        "Mutation Research-Reviews in Mutation Research",
                        "Journal of Molecular Medicine-JMM",
                        "International Journal of Neonatal Screening",
                        "Microbial Genomics",
                        "CRISPR Journal",
                        "Human Gene Therapy",
                        "Heredity"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(gene_here_results_keyword, "gene_here_frames_keyword.csv")

gene_here_results <- all_results |> 
  filter(journal %in% c("Nature Reviews Genetics",
                        "Nature Genetics",
                        "Trends in Ecology & Evolution",
                        "Trends in Genetics",
                        "Molecular Therapy",
                        "Genome Medicine",
                        "Genome Biology",
                        "Genes & Diseases",
                        "Cell Genomics",
                        "Annual Review of Genetics",
                        "Horticulture Research",
                        "American Journal of Human Genetics",
                        "Genomics Proteomics & Bioinformatics",
                        "Annual Review of Genomics and Human Genetics",
                        "Genes & Development",
                        "Oncogene",
                        "Journal of Genetics and Genomics",
                        "Genetics in Medicine",
                        "Phenomics",
                        "Molecular Autism",
                        "Genome Research",
                        "Circulation-Genomic and Precision Medicine",
                        "Environmental Microbiome",
                        "Molecular Biology and Evolution",
                        "Genetics",
                        "Cancer Gene Therapy",
                        "Genes and Nutrition",
                        "npj Genomic Medicine",
                        "European Journal of Human Genetics",
                        "Gene Therapy",
                        "Genes and Immunity",
                        "Mitochondrion",
                        "Clinical Epigenetics",
                        "Molecular Diagnosis & Therapy",
                        "Frontiers in Genome Editing",
                        "American Journal of Medical Genetics Part C-Seminars in Medical Genetics",
                        "Human Genomics",
                        "Neurology-Genetics",
                        "Mutagenesis",
                        "Theoretical and Applied Genetics",
                        "Mutation Research-Reviews in Mutation Research",
                        "Journal of Molecular Medicine-JMM",
                        "International Journal of Neonatal Screening",
                        "Microbial Genomics",
                        "CRISPR Journal",
                        "Human Gene Therapy",
                        "Heredity"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(gene_here_results, "gene_here_frames.csv")

#INTEGRATIVE AND COMPLEMENTARY MEDICINE 
int_comp_results_keyword <- all_results |> 
  filter(journal %in% c("Phytomedicine",
                        "Chinese Medicine",
                        "Journal of Ginseng Research",
                        "American Journal of Chinese Medicine",
                        "Journal of Ethnopharmacology",
                        "Chinese Journal of Natural Medicines",
                        "Acupuncture and Herbal Medicine",
                        "Journal of Integreative Medicine-JIM",
                        "Journal of Evidence-Based Integrative Medicine",
                        "Complementary Therapies in Medicine",
                        "BMC Complementary Medicine and Therapies"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(int_comp_results_keyword, "int_comp_frames_keyword.csv")

int_comp_results <- all_results |> 
  filter(journal %in% c("Phytomedicine",
                        "Chinese Medicine",
                        "Journal of Ginseng Research",
                        "American Journal of Chinese Medicine",
                        "Journal of Ethnopharmacology",
                        "Chinese Journal of Natural Medicines",
                        "Acupuncture and Herbal Medicine",
                        "Journal of Integreative Medicine-JIM",
                        "Journal of Evidence-Based Integrative Medicine",
                        "Complementary Therapies in Medicine",
                        "BMC Complementary Medicine and Therapies"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(int_comp_results, "int_comp_frames.csv")

#MEDICAL INFORMATICS
med_info_results_keyword <- all_results |> 
  filter(journal %in% c("Lancet Digital Health",
                        "npj Digital Medicine",
                        "ACM Transaction on Computing for Healthcare",
                        "PLOS Digital Health",
                        "Intelligent Medicine",
                        "IEEE Journal of Biomedical and Health Informatics",
                        "JMIR mHealth and uHealth",
                        "Artificial Intelligence in Medicine",
                        "Journal of Medical Internet Research",
                        "Journal of Medical Systems",
                        "JMIR Aging",
                        "Computer Methods and Programs in Biomedicine"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(med_info_results_keyword, "med_info_frames_keyword.csv")

med_info_results <- all_results |> 
  filter(journal %in% c("Lancet Digital Health",
                        "npj Digital Medicine",
                        "ACM Transaction on Computing for Healthcare",
                        "PLOS Digital Health",
                        "Intelligent Medicine",
                        "IEEE Journal of Biomedical and Health Informatics",
                        "JMIR mHealth and uHealth",
                        "Artificial Intelligence in Medicine",
                        "Journal of Medical Internet Research",
                        "Journal of Medical Systems",
                        "JMIR Aging",
                        "Computer Methods and Programs in Biomedicine"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(med_info_results, "med_info_frames.csv")

#GENERAL INTERNAL MEDICINE
gen_int_med_results_keyword <- all_results |> 
  filter(journal %in% c("Lancet",
"New England Journal of Medicine",
"Nature Reviews Disease Primers",
"JAMA-Journal of the American Medical Association",
"BMJ-British Medical Journal",
"JAMA Internal Medicine",
"Military Medical Research",
"Annals of Internal Medicine",
"Canadian Medical Association Journal",
"BMJ Evidence-Based Medicine",
"Journal of the Royal Society of Medicine",
"BMJ Medicine",
"EClinicalMedicine",
"PLOS Medicine",
"JAMA Network Open",
"Cochrane Database of Systematic Reviews",
"Journal of Internal Medicine",
"Journal of Cachexia Sarcopenia and Muscle",
"Medical Journal of Australia",
"BMC Medicine",
"Journal of Translational Internal Medicine",
"Amyloid-Journal of Protein Folding Disorders",
"Chinese Medical Journal",
"Deutsches Arzteblatt International",
"Mayo Clinic Proceedings",
"QJM-An International Journal of Medicine",
"Journal of Travel Medicine",
"European Journal of Internal Medicine",
"Translational Research",
"British Medical Bulletin",
"American Journal of Medicine",
"British Journal of General Practice",
"Annals of Family Medicine",
"Polish Archives of Internal Medicine-Polskie Archiwum Medycyny Wewnetrznej",
"American Journal of Preventative Medicine",
"Medical Sciences",
"Primary Care",
"DM Disease-A-Month",
"Annals of Medicine",
"Medical Clinics of North America",
"Journal of General Internal Medicine",
"Journal of Urban Health-Bulletin of the New York Academy of Medicine",
"Cleveland Clinic Journal of Medicine",
"Dental and Medical Problems",
"Palliative Medicine",
"Clinical Medicine",
"Systematic Reviews",
"Internal and Emergency Medicine",
"Balkan Medical Journal",
"European Journal of Clinical Investigation",
"American Family Physician",
"Journal of Pain and Symptom Management",
"Journal of Evidence Based Medicine",
"Presse Medicale",
"Diagnostics",
"Archives of Medical Science",
"Preventative Medicine",
"International Journal of Medical Sciences",
"Pain Medicine",
"Journal of Medical Economics",
"Frontiers in Medicine",
"Precision and Future Medicine",
"Journal of Clinical Medicine",
"Yonsei Medical Journal",
"Postgraduate Medicine",
"Postgraduate Medical Journal",
"Pragmatic and Observational Research",
"Journal of the American Board of Family Medicine",
"BMC Primary Care",
"Diagnostic and Prognostic Research",
"Hong Kong Medical Journal",
"European Journal of General Practice",
"Journal of the Formosan Medical Association",
"BioMedicine-Taiwan",
"Indian Journal of Medical Research",
"Journal of the Chinese Medical Association",
"Medicina-Lithuania",
"Canadian Family Physician",
"International Journal of Clinical Practice",
"Korean Journal of Internal Medicine",
"Clinics"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(gen_int_med_results_keyword, "gen_int_med_frames_keyword.csv")

gen_int_med_results <- all_results |> 
  filter(journal %in% c("Lancet",
                        "New England Journal of Medicine",
                        "Nature Reviews Disease Primers",
                        "JAMA-Journal of the American Medical Association",
                        "BMJ-British Medical Journal",
                        "JAMA Internal Medicine",
                        "Military Medical Research",
                        "Annals of Internal Medicine",
                        "Canadian Medical Association Journal",
                        "BMJ Evidence-Based Medicine",
                        "Journal of the Royal Society of Medicine",
                        "BMJ Medicine",
                        "EClinicalMedicine",
                        "PLOS Medicine",
                        "JAMA Network Open",
                        "Cochrane Database of Systematic Reviews",
                        "Journal of Internal Medicine",
                        "Journal of Cachexia Sarcopenia and Muscle",
                        "Medical Journal of Australia",
                        "BMC Medicine",
                        "Journal of Translational Internal Medicine",
                        "Amyloid-Journal of Protein Folding Disorders",
                        "Chinese Medical Journal",
                        "Deutsches Arzteblatt International",
                        "Mayo Clinic Proceedings",
                        "QJM-An International Journal of Medicine",
                        "Journal of Travel Medicine",
                        "European Journal of Internal Medicine",
                        "Translational Research",
                        "British Medical Bulletin",
                        "American Journal of Medicine",
                        "British Journal of General Practice",
                        "Annals of Family Medicine",
                        "Polish Archives of Internal Medicine-Polskie Archiwum Medycyny Wewnetrznej",
                        "American Journal of Preventative Medicine",
                        "Medical Sciences",
                        "Primary Care",
                        "DM Disease-A-Month",
                        "Annals of Medicine",
                        "Medical Clinics of North America",
                        "Journal of General Internal Medicine",
                        "Journal of Urban Health-Bulletin of the New York Academy of Medicine",
                        "Cleveland Clinic Journal of Medicine",
                        "Dental and Medical Problems",
                        "Palliative Medicine",
                        "Clinical Medicine",
                        "Systematic Reviews",
                        "Internal and Emergency Medicine",
                        "Balkan Medical Journal",
                        "European Journal of Clinical Investigation",
                        "American Family Physician",
                        "Journal of Pain and Symptom Management",
                        "Journal of Evidence Based Medicine",
                        "Presse Medicale",
                        "Diagnostics",
                        "Archives of Medical Science",
                        "Preventative Medicine",
                        "International Journal of Medical Sciences",
                        "Pain Medicine",
                        "Journal of Medical Economics",
                        "Frontiers in Medicine",
                        "Precision and Future Medicine",
                        "Journal of Clinical Medicine",
                        "Yonsei Medical Journal",
                        "Postgraduate Medicine",
                        "Postgraduate Medical Journal",
                        "Pragmatic and Observational Research",
                        "Journal of the American Board of Family Medicine",
                        "BMC Primary Care",
                        "Diagnostic and Prognostic Research",
                        "Hong Kong Medical Journal",
                        "European Journal of General Practice",
                        "Journal of the Formosan Medical Association",
                        "BioMedicine-Taiwan",
                        "Indian Journal of Medical Research",
                        "Journal of the Chinese Medical Association",
                        "Medicina-Lithuania",
                        "Canadian Family Physician",
                        "International Journal of Clinical Practice",
                        "Korean Journal of Internal Medicine",
                        "Clinics"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(gen_int_med_results, "gen_int_med_frames.csv")

#RESEARCH AND EXPERIMENTAL MEDICINE
res_exp_med_results_keyword <- all_results |> 
  filter(journal %in% c("Nature Medicine",
                        "Annual Review of Medicine",
                        "Science Translational Medicine",
                        "Trends in Molecular Medicine",
                        "Journal of Clinical Investigation",
                        "Interdisciplinary Medicine",
                        "Theranostics",
                        "Experimental and Molecular Medicine",
                        "Journal of Biomedical Science",
                        "Med",
                        "Biomarker Research",
                        "EBioMedicine",
                        "MedComm",
                        "Journal of Experimental Medicine",
                        "Cell Reports Medicine",
                        "Molecular Aspects of Medicine",
                        "Cold Spring Harbor Perspectives in Medicine",
                        "Molecular Biomedicine",
                        "Cancer Biology & Medicine",
                        "EMBO Molecular Medicine",
                        "Wiley Interdisciplinary Reviews-Nanomedicine and Nanobiotechnology",
                        "Clinical Science",
                        "Journal of Translation Medicine",
                        "Biomedicine & Pharmacotherapy",
                        "mAbs",
                        "Stem Cell Research & Therapy",
                        "Clinical and Translational Medicine",
                        "npj Vaccines",
                        "Inflammation and Regeneration",
                        "Molecular Medicine",
                        "Communications Medicine",
                        "JCI Insight",
                        "Molecular Therapy Nucleic Acids",
                        "Life Medicine",
                        "EPMA Journal",
                        "ALTEX-Alternatives to Animal Experimentation",
                        "International Journal of Molecular Medicine",
                        "Drug Delivery and Translational Research",
                        "Expert Reviews in Molecular Medicine",
                        "Molecular Therapy Oncology",
                        "Life Sciences",
                        "Precision Clinical Medicine",
                        "Journal of Zhejiang University-Science B",
                        "WIREs Mechanisms of Disease"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(res_exp_med_results_keyword, "res_exp_med_frames_keyword.csv")

res_exp_med_results <- all_results |> 
  filter(journal %in% c("Nature Medicine",
                        "Annual Review of Medicine",
                        "Science Translational Medicine",
                        "Trends in Molecular Medicine",
                        "Journal of Clinical Investigation",
                        "Interdisciplinary Medicine",
                        "Theranostics",
                        "Experimental and Molecular Medicine",
                        "Journal of Biomedical Science",
                        "Med",
                        "Biomarker Research",
                        "EBioMedicine",
                        "MedComm",
                        "Journal of Experimental Medicine",
                        "Cell Reports Medicine",
                        "Molecular Aspects of Medicine",
                        "Cold Spring Harbor Perspectives in Medicine",
                        "Molecular Biomedicine",
                        "Cancer Biology & Medicine",
                        "EMBO Molecular Medicine",
                        "Wiley Interdisciplinary Reviews-Nanomedicine and Nanobiotechnology",
                        "Clinical Science",
                        "Journal of Translation Medicine",
                        "Biomedicine & Pharmacotherapy",
                        "mAbs",
                        "Stem Cell Research & Therapy",
                        "Clinical and Translational Medicine",
                        "npj Vaccines",
                        "Inflammation and Regeneration",
                        "Molecular Medicine",
                        "Communications Medicine",
                        "JCI Insight",
                        "Molecular Therapy Nucleic Acids",
                        "Life Medicine",
                        "EPMA Journal",
                        "ALTEX-Alternatives to Animal Experimentation",
                        "International Journal of Molecular Medicine",
                        "Drug Delivery and Translational Research",
                        "Expert Reviews in Molecular Medicine",
                        "Molecular Therapy Oncology",
                        "Life Sciences",
                        "Precision Clinical Medicine",
                        "Journal of Zhejiang University-Science B",
                        "WIREs Mechanisms of Disease"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(res_exp_med_results, "res_exp_med_frames.csv")

#NEUROIMAGING
neuroimage_results_keyword <- all_results |> 
  filter(journal %in% c("Neuroimage",
                        "Journal of Neurointerventional Surgery",
                        "Neuroimage-Clinical"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(neuroimage_results_keyword, "neuroimage_frames_keyword.csv")

neuroimage_results <- all_results |> 
  filter(journal %in% c("Neuroimage",
                        "Journal of Neurointerventional Surgery",
                        "Neuroimage-Clinical"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(neuroimage_results, "neuroimage_frames.csv")

#NEUROSCIENCES
neurosci_results_keyword <- all_results |> 
  filter(journal %in% c("Nature Reviews Neuroscience",
                        "Nature Neuroscience",
                        "Nature Aging",
                        "Molecular Neurodegeneration",
                        "Nature Human Behavior",
                        "Neuron",
                        "Translational Neurodegeneration",
                        "Trends in Neurosciences",
                        "Annual Review of Neuroscience",
                        "Molecular Psychiatry",
                        "Journal of Neuroinflammation",
                        "Biological Psychiatry",
                        "Dialogues in Clinical Neuroscience",
                        "npj Parkinsons Disease",
                        "Brain Behavior and Immunity",
                        "Neuropsychopharmacology",
                        "Neural Regeneration Research",
                        "Neural Networks",
                        "Fluids and Barriers of the CNS",
                        "Progress in Neurobiology",
                        "Neuroscience Bulletin",
                        "Acta Neuropathologica Communications",
                        "Neurobiology of Disease",
                        "Annual Review of Vision Science",
                        "Current Neuropharmacology",
                        "Journal of NeuroEngineering and Rehabilitation",
                        "Current Opinion in Neurobiology",
                        "GLIA",
                        "CNS Neuroscience & Therapeutics",
                        "Journal of Parkinsons Disease",
                        "Neuropsychology Review",
                        "Developmental Cognitive Neuroscience",
                        "IEEE Transactions on Cognitive and Development Systems",
                        "Cellular and Molecular Neurobiology",
                        "Biological Psychiatry-Cognitive Neuroscience and Neuroimaging",
                        "Cephalalgia",
                        "Neuropharmacology",
                        "Journal of Cerebral Blood Flow and Metabolism",
                        "Frontiers in Aging Neuroscience",
                        "Brain Informatics",
                        "Journal of Physiology-London",
                        "Current Opinion in Neurology",
                        "Molecular Neurobiology",
                        "Cognitive Computation",
                        "Experimental Neurology",
                        "Reviews in the Neurosciences",
                        "Journal of Neuroendocrinology",
                        "International Journal of Tryptophan Research",
                        "Frontiers in Synaptic Neuroscience"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(neurosci_results_keyword, "neurosci_frames_keyword.csv")

neurosci_results <- all_results |> 
  filter(journal %in% c("Nature Reviews Neuroscience",
                        "Nature Neuroscience",
                        "Nature Aging",
                        "Molecular Neurodegeneration",
                        "Nature Human Behavior",
                        "Neuron",
                        "Translational Neurodegeneration",
                        "Trends in Neurosciences",
                        "Annual Review of Neuroscience",
                        "Molecular Psychiatry",
                        "Journal of Neuroinflammation",
                        "Biological Psychiatry",
                        "Dialogues in Clinical Neuroscience",
                        "npj Parkinsons Disease",
                        "Brain Behavior and Immunity",
                        "Neuropsychopharmacology",
                        "Neural Regeneration Research",
                        "Neural Networks",
                        "Fluids and Barriers of the CNS",
                        "Progress in Neurobiology",
                        "Neuroscience Bulletin",
                        "Acta Neuropathologica Communications",
                        "Neurobiology of Disease",
                        "Annual Review of Vision Science",
                        "Current Neuropharmacology",
                        "Journal of NeuroEngineering and Rehabilitation",
                        "Current Opinion in Neurobiology",
                        "GLIA",
                        "CNS Neuroscience & Therapeutics",
                        "Journal of Parkinsons Disease",
                        "Neuropsychology Review",
                        "Developmental Cognitive Neuroscience",
                        "IEEE Transactions on Cognitive and Development Systems",
                        "Cellular and Molecular Neurobiology",
                        "Biological Psychiatry-Cognitive Neuroscience and Neuroimaging",
                        "Cephalalgia",
                        "Neuropharmacology",
                        "Journal of Cerebral Blood Flow and Metabolism",
                        "Frontiers in Aging Neuroscience",
                        "Brain Informatics",
                        "Journal of Physiology-London",
                        "Current Opinion in Neurology",
                        "Molecular Neurobiology",
                        "Cognitive Computation",
                        "Experimental Neurology",
                        "Reviews in the Neurosciences",
                        "Journal of Neuroendocrinology",
                        "International Journal of Tryptophan Research",
                        "Frontiers in Synaptic Neuroscience"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(neurosci_results, "neurosci_frames.csv")

#PATHOLOGY
path_results_keyword <- all_results |> 
  filter(journal %in% c("Annual Review of Pathology-Mechanisms of Disease",
                        "Seminars in Immunopathology",
                        "Modern Pathology",
                        "Journal of Pathology",
                        "HLA",
                        "Cellular Oncology",
                        "American Journal of Surgical Pathology",
                        "Laboratory Investigation ",
                        "Head & Neck Pathology",
                        "Histopathology",
                        "Journal of Pathology Clinical Research",
                        "Experimental and Molecular Pathology",
                        "American Journal of Pathology'",
                        "Expert Review of Molecular Diagnostics",
                        "Seminars in Diagnostic Pathology",
                        "Journal of Molecular Diagnostics",
                        "Neuropathology and Applied Neurobiology",
                        "Pathology International",
                        "Disease Models & Mechanisms"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(path_results_keyword, "path_frames_keyword.csv")

path_results <- all_results |> 
  filter(journal %in% c("Annual Review of Pathology-Mechanisms of Disease",
                        "Seminars in Immunopathology",
                        "Modern Pathology",
                        "Journal of Pathology",
                        "HLA",
                        "Cellular Oncology",
                        "American Journal of Surgical Pathology",
                        "Laboratory Investigation ",
                        "Head & Neck Pathology",
                        "Histopathology",
                        "Journal of Pathology Clinical Research",
                        "Experimental and Molecular Pathology",
                        "American Journal of Pathology'",
                        "Expert Review of Molecular Diagnostics",
                        "Seminars in Diagnostic Pathology",
                        "Journal of Molecular Diagnostics",
                        "Neuropathology and Applied Neurobiology",
                        "Pathology International",
                        "Disease Models & Mechanisms"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(path_results, "path_frames.csv")

#PHARMACY AND PHARMACOLOGY
pharma_results_keyword <- all_results |> 
  filter(journal %in% c("Nature Reviews Drug Discovery",
                        "Drug Resistance Updates",
                        "Trends in Pharmacological Sciences",
                        "Advanced Drug Delivery Reviews",
                        "Pharmacological Reviews",
                        "Acta Pharmaceutica Sinica B",
                        "Drugs",
                        "Annual Review of Pharmacology and Toxicology",
                        "Pharmacology & Therapeutics ",
                        "Asian Journal of Pharmaceutical Sciences",
                        "Medicinal Research Reviews",
                        "Journal of Controlled Release",
                        "Pharmacological Research",
                        "Journal of Pharmaceutical Analysis",
                        "Acta Pharmacologica Sinica",
                        "Drug Delivery",
                        "British Journal of Pharmacology",
                        "Archives of Pharmacal Research",
                        "Drug Discovery Today",
                        "Biodrugs",
                        "Alimentary Pharmacology & Therapeutics",
                        "International Journal of Nanomedicine",
                        "International Journal of Pharmaceutics-X",
                        "Phytotherapy Research",
                        "European Heart Journal-Cardiovascular Pharmacotherapy",
                        "Biochemical Pharmacology",
                        "Clinical Pharmacology & Therapeutics",
                        "Pharmaceutics",
                        "Marine Drugs",
                        "Chemico-Biological Interactions",
                        "Expert Opinion on Drug Delivery",
                        "International Journal of Pharmaceutics",
                        "Drug Design Development and Therapy",
                        "Journal of Pharmaceutical Investigation",
                        "Journal of Drug Delivery Science and Technology",
                        "Expert Opinion on Drug Discovery",
                        "Pharmaceutical Biology",
                        "Frontiers in Pharmacology",
                        "Pharmaceuticals",
                        "European Journal of Pharmacology",
                        "European Journal of Pharmaceutical Sciences",
                        "International Immunopharmacology",
                        "Pharmacoeconomics",
                        "International Journal of Antimicrobial Agents",
                        "Toxicology",
                        "Journal of Clinical Lipidology",
                        "Antibiotics-Basel",
                        "Expert Opinion on Therapeutic Patents",
                        "Molecular Pharmaceutics",
                        "Antimicrobial Agents and Chemotherapy",
                        "Pharmaceutical Medicine",
                        "Expert Opinion on Therapeutic Targets",
                        "European Journal of Pharmaceutics and Biopharmaceutics",
                        "Journal of Cannabis Research",
                        "Journal of Pharmacy and Pharmaceutical Sciences",
                        "ADMET and DMPK",
                        "Pharmaceutical Research",
                        "Journal of Liposome Research",
                        "Current Opinion in Pharmacology",
                        "Environmental Toxicology and Pharmacology",
                        "Drug Development Research",
                        "Australian Prescriber",
                        "Expert Opinion on Investigational Drugs",
                        "Advances in Therapy",
                        "Advanced Pharmaceutical Bulletin",
                        "Recent Patents on Anti-Cancer Drug Discovery",
                        "Antiviral Research",
                        "Drug Metabolism and Disposition",
                        "Clinical Pharmacokinetics",
                        "AAPS Pharmscitech ",
                        "Therapeutic Advances in Psychopharmacology",
                        "Neurotoxicology",
                        "Journal of Drug Targeting",
                        "Biomedicines"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(pharma_results_keyword, "pharma_frames_keyword.csv")

pharma_results <- all_results |> 
  filter(journal %in% c("Nature Reviews Drug Discovery",
                        "Drug Resistance Updates",
                        "Trends in Pharmacological Sciences",
                        "Advanced Drug Delivery Reviews",
                        "Pharmacological Reviews",
                        "Acta Pharmaceutica Sinica B",
                        "Drugs",
                        "Annual Review of Pharmacology and Toxicology",
                        "Pharmacology & Therapeutics ",
                        "Asian Journal of Pharmaceutical Sciences",
                        "Medicinal Research Reviews",
                        "Journal of Controlled Release",
                        "Pharmacological Research",
                        "Journal of Pharmaceutical Analysis",
                        "Acta Pharmacologica Sinica",
                        "Drug Delivery",
                        "British Journal of Pharmacology",
                        "Archives of Pharmacal Research",
                        "Drug Discovery Today",
                        "Biodrugs",
                        "Alimentary Pharmacology & Therapeutics",
                        "International Journal of Nanomedicine",
                        "International Journal of Pharmaceutics-X",
                        "Phytotherapy Research",
                        "European Heart Journal-Cardiovascular Pharmacotherapy",
                        "Biochemical Pharmacology",
                        "Clinical Pharmacology & Therapeutics",
                        "Pharmaceutics",
                        "Marine Drugs",
                        "Chemico-Biological Interactions",
                        "Expert Opinion on Drug Delivery",
                        "International Journal of Pharmaceutics",
                        "Drug Design Development and Therapy",
                        "Journal of Pharmaceutical Investigation",
                        "Journal of Drug Delivery Science and Technology",
                        "Expert Opinion on Drug Discovery",
                        "Pharmaceutical Biology",
                        "Frontiers in Pharmacology",
                        "Pharmaceuticals",
                        "European Journal of Pharmacology",
                        "European Journal of Pharmaceutical Sciences",
                        "International Immunopharmacology",
                        "Pharmacoeconomics",
                        "International Journal of Antimicrobial Agents",
                        "Toxicology",
                        "Journal of Clinical Lipidology",
                        "Antibiotics-Basel",
                        "Expert Opinion on Therapeutic Patents",
                        "Molecular Pharmaceutics",
                        "Antimicrobial Agents and Chemotherapy",
                        "Pharmaceutical Medicine",
                        "Expert Opinion on Therapeutic Targets",
                        "European Journal of Pharmaceutics and Biopharmaceutics",
                        "Journal of Cannabis Research",
                        "Journal of Pharmacy and Pharmaceutical Sciences",
                        "ADMET and DMPK",
                        "Pharmaceutical Research",
                        "Journal of Liposome Research",
                        "Current Opinion in Pharmacology",
                        "Environmental Toxicology and Pharmacology",
                        "Drug Development Research",
                        "Australian Prescriber",
                        "Expert Opinion on Investigational Drugs",
                        "Advances in Therapy",
                        "Advanced Pharmaceutical Bulletin",
                        "Recent Patents on Anti-Cancer Drug Discovery",
                        "Antiviral Research",
                        "Drug Metabolism and Disposition",
                        "Clinical Pharmacokinetics",
                        "AAPS Pharmscitech ",
                        "Therapeutic Advances in Psychopharmacology",
                        "Neurotoxicology",
                        "Journal of Drug Targeting",
                        "Biomedicines"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(pharma_results, "pharma_frames.csv")

#PRIMARY HEALTH
prim_health_results_keyword <- all_results |> 
  filter(journal %in% c("npj Primary Care Respiratory Medicine",
                        "Family Medicine and Community Health",
                        "Korean Journal of Family Medicine"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(prim_health_results_keyword, "prim_health_frames_keyword.csv")

prim_health_results <- all_results |> 
  filter(journal %in% c("npj Primary Care Respiratory Medicine",
                        "Family Medicine and Community Health",
                        "Korean Journal of Family Medicine"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(prim_health_results, "prim_health_frames.csv")

#PSYCHIATRY
psychiatry_results_keyword <- all_results |> 
  filter(journal %in% c("World Psychiatry ",
                        "Lancet Psychiatry",
                        "Psychotherapy and Psychosomatics",
                        "JAMA Psychiatry",
                        "American Journal of Psychiatry",
                        "Evidence-Based Mental Health",
                        "Journal of the American Academy of Child and Adolescent Psychiatry",
                        "Nature Mental Health",
                        "British Journal of Psychiatry",
                        "Journal of Child Psychology and Psychiatry",
                        "General Psychiatry",
                        "European Psychiatry",
                        "Current Psychiatry Reports",
                        "Translational Psychiatry",
                        "Journal of Behavioral Addictions",
                        "Epidemiology and Psychiatric Sciences",
                        "JMIR Mental Health",
                        "Psychological Medicine",
                        "Body Image",
                        "Addiction",
                        "Asian Journal of Social Health and Behavior",
                        "Child and Adolescent Mental Health",
                        "Acta Psychiatrica Scandinavia",
                        "European Child & Adolescent Psychiatry",
                        "BMJ Mental Health",
                        "Current Opinion in Psychiatry",
                        "Schizophrenia Bulletin",
                        "Revista de Psiquiatria y Salud Mental",
                        "Child and Adolescent Psychiatry and Mental Health",
                        "Journal of Clinical Psychiatry",
                        "Journal of Anxiety Disorders",
                        "Journal of Eating Disorders",
                        "Asian Journal of Psychiatry",
                        "International Psychogeriatrics",
                        "International Journal of Eating Disorders",
                        "Comprehensive Psychiatry",
                        "Clinical Psychological Science",
                        "European Journal of Psychotraumatology",
                        "Schizophrenia",
                        "Internet Interventions-The Application of Information Technology in Mental and Behavioral Health",
                        "Journal of Psychopathology and Clinical Science",
                        "Psychiatry Research",
                        "Behavior Therapy",
                        "American Journal of Geriatric Psychiatry",
                        "Canadian Journal of Psychiatry-Revue Canadienne de Psychiatrie",
                        "International Journal of Mental Health Systems",
                        "World Journal of Biological Psychiatry",
                        "Biological Psychiatry: Global Open Science",
                        "Australian and New Zealand Journal of Psychiatry",
                        "Drug and Alcohol Dependence",
                        "BMC Psychiatry",
                        "Psychoneuroendocrinology",
                        "Mindfulness",
                        "Schizophrenia Research",
                        "Social Psychiatry and Psychiatric Epidemology",
                        "BJPsych Open",
                        "Eating Disorders",
                        "Brain Behavior & Immunity - Health",
                        "International Journal of Bipolar Disorders",
                        "Alpha Psychiatry"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(psychiatry_results_keyword, "psychiatry_frames_keyword.csv")

psychiatry_results <- all_results |> 
  filter(journal %in% c("World Psychiatry ",
                        "Lancet Psychiatry",
                        "Psychotherapy and Psychosomatics",
                        "JAMA Psychiatry",
                        "American Journal of Psychiatry",
                        "Evidence-Based Mental Health",
                        "Journal of the American Academy of Child and Adolescent Psychiatry",
                        "Nature Mental Health",
                        "British Journal of Psychiatry",
                        "Journal of Child Psychology and Psychiatry",
                        "General Psychiatry",
                        "European Psychiatry",
                        "Current Psychiatry Reports",
                        "Translational Psychiatry",
                        "Journal of Behavioral Addictions",
                        "Epidemiology and Psychiatric Sciences",
                        "JMIR Mental Health",
                        "Psychological Medicine",
                        "Body Image",
                        "Addiction",
                        "Asian Journal of Social Health and Behavior",
                        "Child and Adolescent Mental Health",
                        "Acta Psychiatrica Scandinavia",
                        "European Child & Adolescent Psychiatry",
                        "BMJ Mental Health",
                        "Current Opinion in Psychiatry",
                        "Schizophrenia Bulletin",
                        "Revista de Psiquiatria y Salud Mental",
                        "Child and Adolescent Psychiatry and Mental Health",
                        "Journal of Clinical Psychiatry",
                        "Journal of Anxiety Disorders",
                        "Journal of Eating Disorders",
                        "Asian Journal of Psychiatry",
                        "International Psychogeriatrics",
                        "International Journal of Eating Disorders",
                        "Comprehensive Psychiatry",
                        "Clinical Psychological Science",
                        "European Journal of Psychotraumatology",
                        "Schizophrenia",
                        "Internet Interventions-The Application of Information Technology in Mental and Behavioral Health",
                        "Journal of Psychopathology and Clinical Science",
                        "Psychiatry Research",
                        "Behavior Therapy",
                        "American Journal of Geriatric Psychiatry",
                        "Canadian Journal of Psychiatry-Revue Canadienne de Psychiatrie",
                        "International Journal of Mental Health Systems",
                        "World Journal of Biological Psychiatry",
                        "Biological Psychiatry: Global Open Science",
                        "Australian and New Zealand Journal of Psychiatry",
                        "Drug and Alcohol Dependence",
                        "BMC Psychiatry",
                        "Psychoneuroendocrinology",
                        "Mindfulness",
                        "Schizophrenia Research",
                        "Social Psychiatry and Psychiatric Epidemology",
                        "BJPsych Open",
                        "Eating Disorders",
                        "Brain Behavior & Immunity - Health",
                        "International Journal of Bipolar Disorders",
                        "Alpha Psychiatry"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(psychiatry_results, "psychiatry_frames.csv")

#CLINICAL PSYCHOLOGY
clin_psychology_results_keyword <- all_results |> 
  filter(journal %in% c("Annual Review of Clinical Psychology ",
                        "International Journal of Transgender Health",
                        "Clinical Psychology Review",
                        "Health Psychology Review",
                        "Clinical Psychology-Science and Practice",
                        "Clinical Child and Family Psychology Review",
                        "Journal of Consulting and Clinical Psychology ",
                        "Behaviour Research and Therapy",
                        "International Journal of Clinical and Health Psychology",
                        "Journal of Clinical Child and Adolescent Psychology",
                        "Personality Disorders-Theory Research and Treatment",
                        "Journal of Substance Abuse Treatment",
                        "Psychotherapy",
                        "Perspectives on Behavior Science",
                        "Addictive Behaviors",
                        "Assessment"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(clin_psychology_results_keyword, "clin_psychology_frames_keyword.csv")

clin_psychology_results <- all_results |> 
  filter(journal %in% c("Annual Review of Clinical Psychology ",
                        "International Journal of Transgender Health",
                        "Clinical Psychology Review",
                        "Health Psychology Review",
                        "Clinical Psychology-Science and Practice",
                        "Clinical Child and Family Psychology Review",
                        "Journal of Consulting and Clinical Psychology ",
                        "Behaviour Research and Therapy",
                        "International Journal of Clinical and Health Psychology",
                        "Journal of Clinical Child and Adolescent Psychology",
                        "Personality Disorders-Theory Research and Treatment",
                        "Journal of Substance Abuse Treatment",
                        "Psychotherapy",
                        "Perspectives on Behavior Science",
                        "Addictive Behaviors",
                        "Assessment"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(clin_psychology_results, "clin_psychology_frames.csv")

#SUBSTANCE ABUSE
substance_abuse_results_keyword <- all_results |> 
  filter(journal %in% c("Alcohol Research-Current Reviews",
                        "Substance Abuse and Rehabilitation",
                        "Tobacco Control",
                        "Current Addiction Reports",
                        "International Journal of Drug Policy",
                        "Harm Reduction Journal",
                        "Performance Enhancement and Health",
                        "Journal of Addiction Medicine",
                        "Addiction Science & Clinical Practice",
                        "Substance Abuse Treatment Prevention and Policy"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(substance_abuse_results_keyword, "sub_abuse_frames_keyword.csv")

substance_abuse_results <- all_results |> 
  filter(journal %in% c("Alcohol Research-Current Reviews",
                        "Substance Abuse and Rehabilitation",
                        "Tobacco Control",
                        "Current Addiction Reports",
                        "International Journal of Drug Policy",
                        "Harm Reduction Journal",
                        "Performance Enhancement and Health",
                        "Journal of Addiction Medicine",
                        "Addiction Science & Clinical Practice",
                        "Substance Abuse Treatment Prevention and Policy"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(substance_abuse_results, "sub_abuse_frames.csv")

#TOXICOLOGY
tox_results_keyword <- all_results |> 
  filter(journal %in% c("Environmental Health Perspectives",
                        "Integrated Environmental Assessment and Management",
                        "Particle and Fibre Toxicology",
                        "Environmental Chemistry and Ecotoxicology",
                        "Journal of Toxicology and Environmental Health-Part B-Critical Reviews",
                        "Archives of Toxicology",
                        "Reviews of Environmental Contamination and Toxicology",
                        "Ecotoxicology and Environmental Safety",
                        "Cell Biology and Toxicology",
                        "Inflammopharmacology",
                        "Journal of Exposure Science and Environmental Epidemiology",
                        "Frontiers in Toxicology",
                        "Journal of Xenobiotics",
                        "Aquatic Toxicology",
                        "Comparative Biochemistry and Physiology C-Toxicology & Pharmacology",
                        "Toxicological Sciences",
                        "Toxics",
                        "Critical Reviews in Toxicology",
                        "Toxins"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(tox_results_keyword, "tox_frames_keyword.csv")

tox_results <- all_results |> 
  filter(journal %in% c("Environmental Health Perspectives",
                        "Integrated Environmental Assessment and Management",
                        "Particle and Fibre Toxicology",
                        "Environmental Chemistry and Ecotoxicology",
                        "Journal of Toxicology and Environmental Health-Part B-Critical Reviews",
                        "Archives of Toxicology",
                        "Reviews of Environmental Contamination and Toxicology",
                        "Ecotoxicology and Environmental Safety",
                        "Cell Biology and Toxicology",
                        "Inflammopharmacology",
                        "Journal of Exposure Science and Environmental Epidemiology",
                        "Frontiers in Toxicology",
                        "Journal of Xenobiotics",
                        "Aquatic Toxicology",
                        "Comparative Biochemistry and Physiology C-Toxicology & Pharmacology",
                        "Toxicological Sciences",
                        "Toxics",
                        "Critical Reviews in Toxicology",
                        "Toxins"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(tox_results, "tox_frames.csv")

#PUBLIC, ENVIRONMENTAL, AND OCCUPATIONAL HEALTH 
pub_env_occ_health_results_keyword <- all_results |> 
  filter(journal %in% c("MMWR Recommendations and Reports",
                        "MMWR Surveillance Summaries",
                        "Lancet Public Health",
                        "Lancet Planetary Health",
                        "Annual Review of Public Health",
                        "Lancet Global Health",
                        "MMWR-Morbidity and Mortality Weekly Report",
                        "Lancet Regional Health-Europe",
                        "Analytic Methods in Accident Research",
                        "JAMA Health Forum",
                        "American Journal of Public Health",
                        "Current Environmental Health Reports",
                        "Lancet Regional Health-Western Pacific",
                        "Current Pollution Reports",
                        "Environmental Research",
                        "Lancet Regional Health-Americas",
                        "Environment & Health",
                        "Accident Analysis and Prevention",
                        "Lancet Regional Health-Southeast Asia",
                        "BMJ Global Health",
                        "European Journal of Epidemiology",
                        "International Journal of Epidemiology",
                        "Environmental Health",
                        "Nepal Journal of Epidemiology",
                        "Bulletin of the World Health Organization",
                        "Journal of Clinical Epidemiology",
                        "Social Science & Medicine",
                        "American Journal of Epidemiology",
                        "Travel Medicine and Infectious Disease",
                        "Global Health Research and Policy",
                        "Journal of Adolescent Health",
                        "Globalization and Health",
                        "One Health",
                        "Reviews on Environmental Health",
                        "International Journal of Hygiene and Environmental Health",
                        "Epidemiology",
                        "Journal of Safety Research",
                        "Antimicrobial Resistance and Infection Control ",
                        "Scandinavian Journal of Work Environment & Health",
                        "Journal of Global Health",
                        "Indoor Air",
                        "Health & Place",
                        "International Journal for Equity in Health",
                        "JMIR Serious Games",
                        "Emerging Themes in Epidemiology",
                        "Public Health Reviews",
                        "Biomedical and Environmental Sciences",
                        "Journal of Infection and Public Health",
                        "Journal of Occupational Health Psychology",
                        "JMIR Public Health and Surveillance ",
                        "Preventing Chronic Disease",
                        "Current Epidemiology Reports",
                        "European Journal of Public Health",
                        "GeoHealth",
                        "Drug Safety",
                        "AIDS Patient Care and STDs",
                        "Epidemiologic Reviews",
                        "Environmental Epidemiology",
                        "Progress in Disaster Science",
                        "Environmental Geochemistry and Health",
                        "Journal of Epidemiology and Community Health",
                        "Journal of Health and Social Behavior",
                        "BMC Public Health",
                        "Journal of Climate Change and Health",
                        "One Health Outlook",
                        "LGBT Health",
                        "Hygiene and Environmental Health Advances",
                        "Journal of Safety Science and Resilience",
                        "Frontiers in Public Health",
                        "Reproductive Health",
                        "Conflict and Health",
                        "Cancer Epidemiology Biomarkers & Prevention",
                        "ACS Chemical Health & Safety",
                        "Public Health Research & Practice",
                        "Disability and Health Journal",
                        "Risk Analysis",
                        "Digital Health",
                        "Health Reports",
                        "Journal of Transport & Health",
                        "Psychiatric Services",
                        "Clinical Epidemiology",
                        "Public Health",
                        "Health Expectations",
                        "Archives of Public Health",
                        "International Journal of Health Geographics",
                        "Annals of Global Health",
                        "Global Transitions",
                        "Occuptational and Environmental Medicine",
                        "Patient Education and Counseling",
                        "SSM-Population Health",
                        "Journal of Epidemiology and Global Health",
                        "Studies in Family Planning",
                        "Journal of Hospital Infection",
                        "Journal of Public Health",
                        "Salud Publica de Mexico",
                        "China CDC Weekly",
                        "American Journal of Industrial Medicine",
                        "International Journal of Technology Assessment in Health Care"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"abuse|drug|addict|opioid|opiate|alcohol|cannabis|depend|tolera|withdraw|assist|relaps|detox|abst|harm|reduc|agon|antagon|bing|compuls|recover|substance|impulse|mu|IV|intravenous|misuse|narcotic|inject|prescri|amphetamine|cocaine|overdose|phencyclidine|heroin|morphine|inject")) %>% 
  mutate(trigram = reorder(trigram, n))

write.csv(pub_env_occ_health_results_keyword, "pub_env_occ_health_frames_keyword.csv")

pub_env_occ_health_results <- all_results |> 
  filter(journal %in% c("MMWR Recommendations and Reports",
                        "MMWR Surveillance Summaries",
                        "Lancet Public Health",
                        "Lancet Planetary Health",
                        "Annual Review of Public Health",
                        "Lancet Global Health",
                        "MMWR-Morbidity and Mortality Weekly Report",
                        "Lancet Regional Health-Europe",
                        "Analytic Methods in Accident Research",
                        "JAMA Health Forum",
                        "American Journal of Public Health",
                        "Current Environmental Health Reports",
                        "Lancet Regional Health-Western Pacific",
                        "Current Pollution Reports",
                        "Environmental Research",
                        "Lancet Regional Health-Americas",
                        "Environment & Health",
                        "Accident Analysis and Prevention",
                        "Lancet Regional Health-Southeast Asia",
                        "BMJ Global Health",
                        "European Journal of Epidemiology",
                        "International Journal of Epidemiology",
                        "Environmental Health",
                        "Nepal Journal of Epidemiology",
                        "Bulletin of the World Health Organization",
                        "Journal of Clinical Epidemiology",
                        "Social Science & Medicine",
                        "American Journal of Epidemiology",
                        "Travel Medicine and Infectious Disease",
                        "Global Health Research and Policy",
                        "Journal of Adolescent Health",
                        "Globalization and Health",
                        "One Health",
                        "Reviews on Environmental Health",
                        "International Journal of Hygiene and Environmental Health",
                        "Epidemiology",
                        "Journal of Safety Research",
                        "Antimicrobial Resistance and Infection Control ",
                        "Scandinavian Journal of Work Environment & Health",
                        "Journal of Global Health",
                        "Indoor Air",
                        "Health & Place",
                        "International Journal for Equity in Health",
                        "JMIR Serious Games",
                        "Emerging Themes in Epidemiology",
                        "Public Health Reviews",
                        "Biomedical and Environmental Sciences",
                        "Journal of Infection and Public Health",
                        "Journal of Occupational Health Psychology",
                        "JMIR Public Health and Surveillance ",
                        "Preventing Chronic Disease",
                        "Current Epidemiology Reports",
                        "European Journal of Public Health",
                        "GeoHealth",
                        "Drug Safety",
                        "AIDS Patient Care and STDs",
                        "Epidemiologic Reviews",
                        "Environmental Epidemiology",
                        "Progress in Disaster Science",
                        "Environmental Geochemistry and Health",
                        "Journal of Epidemiology and Community Health",
                        "Journal of Health and Social Behavior",
                        "BMC Public Health",
                        "Journal of Climate Change and Health",
                        "One Health Outlook",
                        "LGBT Health",
                        "Hygiene and Environmental Health Advances",
                        "Journal of Safety Science and Resilience",
                        "Frontiers in Public Health",
                        "Reproductive Health",
                        "Conflict and Health",
                        "Cancer Epidemiology Biomarkers & Prevention",
                        "ACS Chemical Health & Safety",
                        "Public Health Research & Practice",
                        "Disability and Health Journal",
                        "Risk Analysis",
                        "Digital Health",
                        "Health Reports",
                        "Journal of Transport & Health",
                        "Psychiatric Services",
                        "Clinical Epidemiology",
                        "Public Health",
                        "Health Expectations",
                        "Archives of Public Health",
                        "International Journal of Health Geographics",
                        "Annals of Global Health",
                        "Global Transitions",
                        "Occuptational and Environmental Medicine",
                        "Patient Education and Counseling",
                        "SSM-Population Health",
                        "Journal of Epidemiology and Global Health",
                        "Studies in Family Planning",
                        "Journal of Hospital Infection",
                        "Journal of Public Health",
                        "Salud Publica de Mexico",
                        "China CDC Weekly",
                        "American Journal of Industrial Medicine",
                        "International Journal of Technology Assessment in Health Care"))|> 
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  mutate(trigram = reorder(trigram, n))

write.csv(pub_env_occ_health_results, "pub_env_occ_health_frames.csv")


#RURAL MENTIONS, prelim
rural_framegrams <- all_results %>%
  unnest_tokens(trigram, abstract, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,"rural|frontier|CAH|underserved|shortage|catchment|non-urban|country|remote|distance|desert|RUCA|farm|Native|tribal|reservation|nonmetropolitan|appalachia|rural-urban")) %>% 
  mutate(trigram = reorder(trigram, n))
