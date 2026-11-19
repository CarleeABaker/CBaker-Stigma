#new text analysis file since i'm scrapping most of the approach in the original file

library(tidyverse)
library(xml2)
library(tidytext)
library(dplyr)
library(textstem)
library(stringr)
library(readr)
library(purrr)

#this fixes all of the inconsistencies between the clarivate categories csv and the results from pubmed by lowering all of the titles, removing whitespace, removing differences in punctuation choices, and getting rid of ampersands. i giggled at str_squish--it removes the whitespace
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
      dplyr::select(join_key, category, jif_quart),
    by = "join_key"
  ) |> 
  dplyr::select(journal, abstract, category, jif_quart)

#there's some weirdness as i try to do descriptive statistics below, so i have to standardize some of the category names
joined_df_standard <- joined_df |> 
  mutate(category = str_replace_all(category, "Pharmacology & Pharmacy", "Pharmacology_Pharmacy")) |> 
  mutate(category = str_replace_all(category, "Public, Environmental & Occupational Health", "Public_Environmental_Occupational_Health")) |> 
  mutate(category = str_replace_all(category, "Genetics & Heredity", "Genetics_Heredity")) |> 
  mutate(category = str_replace_all(category, "Medicine, Research & Experimental", "Medicine_Research_Experimental")) |> 
  mutate(category = str_replace_all(category, "Integrative & Complementary Medicine", "Integrative_Complementary_Medicine")) |> 
  mutate(category = str_replace_all(category, "Medicine, General & Internal", "Medicine_General_Internal")) |> 
  mutate(category = str_replace_all(category, "Psychology, Clinical", "Clinical_Psychology")) |> 
  mutate(category = str_replace_all(category, "Clinical Neurology", "Clinical_Neurology")) |> 
  mutate(category = str_replace_all(category, "Emergency Medicine", "Emergency_Medicine")) |> 
  mutate(category = str_replace_all(category, "Behavioral Sciences", "Behavioral_Sciences")) |> 
  mutate(category = str_replace_all(category, "Endocrinology and Metabolism", "Endocrinology_Metabolism")) |> 
  mutate(category = str_replace_all(category, "Medical Informatics", "Medical_Informatics")) |> 
  mutate(category = str_replace_all(category, "Primary Health", "Primary_Health")) |> 
  mutate(category = str_replace_all(category, "Substance Abuse", "Substance_Abuse")) |> 
  mutate(category = str_replace_all(category, "Critical Care Medicine", "Critical_Care_Medicine"))

#the category NAs are causing issues for the accurate calculations for tf-idf. i'm just fixing it by hand because i've tried automating it and there are like 10 journals that refuse to cooperate, so I'm fixing it by hand. IT FUCKING WORKED
joined_df_standard <- joined_df_standard |> 
  mutate(category = case_when(
    journal == "Emergencias : revista de la Sociedad Espanola de Medicina de Emergencias" ~ "Emergency_Medicine",
    journal == "Cortex; a journal devoted to the study of the nervous system and behavior" ~ "Behavioral_Sciences",
    journal == "Brain : a journal of neurology" ~ "Clinical_Neurology, Neurosciences",
    journal == "Neurotherapeutics : the journal of the American Society for Experimental NeuroTherapeutics" ~ "Clinical_Neurology, Neurosciences, Pharmacology_Pharmacy", 
    journal == "The Neuroscientist : a review journal bringing neurobiology, neurology and psychiatry" ~ "Clinical_Neurology", 
    journal == "Thyroid : official journal of the American Thyroid Association" ~ "Endocrinology_Metabolism",
    journal == "BioFactors (Oxford, England)" ~ "Endocrinology_Metabolism",
    journal == "Phytomedicine : international journal of phytotherapy and phytopharmacology" ~ "Integrative_Complementary_Medicine, Pharmacology_Pharmacy",
    journal == "Lancet (London, England)" ~ "Medicine_General_Internal",
    journal == "Med (New York, N.Y.)" ~ "Medicine_Research_Experimental",
    journal == "Neuropsychopharmacology : official publication of the American College of Neuropsychopharmacology" ~ "Neurosciences, Pharmacology_Pharmacy, Psychiatry",
    journal == "Cephalalgia : an international journal of headache" ~ "Neurosciences",
    journal == "BioDrugs : clinical immunotherapeutics, biopharmaceuticals and gene therapy" ~ "Pharmacology_Pharmacy",
    journal == "The lancet. Psychiatry" ~ "Psychiatry",
    journal == "Addiction (Abingdon, England)" ~ "Substance_Abuse",
    journal == "The Lancet. Public health" ~ "Public_Environmental_Occupational_Health",
    journal == "Epidemiology (Cambridge, Mass.)" ~ "Public_Environmental_Occupational_Health",
    TRUE                            ~ category))

write.csv(joined_df_standard, "joined_df_standardized.csv")
  
#read in the keywords lists for substance use and rural, stem/lemmify them and make them regex-able
drug_terms <- read_csv("drug_keywords.csv")

drug_terms_regex <- paste0(drug_terms$term,collapse = "|")

regexify <- function(x){
  stems <- stem_words(x)
  lemmas <- lemmatize_words(x)
  c(stems,lemmas,x) %>% unique %>% 
    paste0(.,collapse = "|") %>% 
    tolower()
}

drug_terms_better_regex <- regexify(drug_terms$term)

#same thing for rural
rural_terms <- read_csv("rural_keywords.csv")

rural_terms_regex <- paste0(rural_terms$term,collapse = "|")

rural_terms_better_regex <- regexify(rural_terms$term)

rural_word_counts <- joined_df_standard |>
  select(abstract, category) |>
  unnest_tokens(term, abstract) |>
  anti_join(stop_words, by = c("term" = "word")) |>
  filter(str_detect(term, rural_terms_better_regex)) |> 
  count(category, term) |>
  filter(n > 10)

write.csv(rural_word_counts, "rural_word_counts.csv")

trigram_counts_rural <- joined_df_standard |>
  select(abstract, category) |>
  unnest_tokens(trigram, abstract, token = "ngrams", n = 3) |>
  filter(str_detect(trigram, rural_terms_better_regex)) |> 
  filter(!is.na(trigram)) |>
  count(category, trigram, sort = TRUE) |>
  filter(n > 3)

write.csv(trigram_counts_rural, "rural_trigrams.csv")
  

#get descriptive statistics, total. will have some issues with journals that are listed in multiple categories, so have to split the string in order to run
total_counts <- joined_df_standard |> 
  separate_longer_delim(category, delim = ", ") |> 
  count(category, sort = TRUE)

write.csv(total_counts, "descriptive_stats.csv")

abstracts_missing <- joined_df_standard |> 
  filter(is.na(abstract)) |> 
  separate_longer_delim(category, delim = ",") |>
  mutate(category = trimws(category)) |>
  count(category, sort = TRUE)

write.csv(abstracts_missing, "abstracts_missing.csv")
