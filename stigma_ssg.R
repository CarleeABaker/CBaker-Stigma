
# Load libraries 
library(tidyverse)
require(sandwich)
require(msm)
require(pscl)
library(sjPlot)
library(broom)
library(DescTools)
library(MASS)
library(sjtable2df)
library(clipr)
library(insight)
library(lmtest)
library(lmtest)

# (De)Stigma Analysis -----------------------------------------------------
joined_df <- read.csv("joined_df_standardized.csv")

#removes NAs and lowercases everything
data_cc <- joined_df %>% filter(complete.cases(joined_df))

stigma_terms <- "addict |user |junkie|alcoholic|drunk|habit|abuse|clean|dirty|addicted baby|opioid substitution therapy|opioid replacement therapy|abuser|druggie|relapse|substance dependent|medication-assisted treatment|medication assisted treatment"

destigma_terms <-"person with|people with|person in|people in|recovery|person in long-term recovery|person in long term recovery|person who previously used drugs|substance use disorder|opioid use disorder|alcohol use disorder|drug addiction|misuse|drug use|opioid use|other than prescribed|opioid agonist therapy|pharmacotherapy|addiction medication| moud |(MOUD)|remission|abstinen|baby with|newborn exposed|misuse|recovery|active use"

rural_terms <- "rural|nonmetropolitan|non-urban|small town|micropolitan|CAH|critical access hospital|rural health clinic|RHC|health professional shortage area|HPSA|longitudinal care|geographic barrier"

#dataframe including category column with multiple categories per row and adding a column with categorical variable for rural keywords
data_st <- data_cc %>%
  mutate(
    abstract        = str_to_lower(abstract),
    stigma          = str_count(abstract, stigma_terms),
    destigma        = str_count(abstract, destigma_terms),
    rural           = str_count(abstract, rural_terms),   # raw count
    rural_cat       = factor(as.integer(rural > 0), levels = c(0, 1)),  # binary factor
    discipline_fact = as.factor(category)
  )

#long df that separates each row containing multiple categories, introducing duplicates. will use later
df_long <- joined_df |> 
  separate_longer_delim(category, delim = ",") |> 
  mutate(category = str_trim(category))

df_long <- df_long %>% 
  filter(!is.na(abstract)) %>%
  mutate(abstract = str_to_lower(abstract))


df_long_st <- df_long |> 
  mutate(
    abstract        = str_to_lower(abstract),
    stigma          = str_count(abstract, stigma_terms),
    destigma        = str_count(abstract, destigma_terms),
    rural           = str_count(abstract, rural_terms),   # raw count
    rural_cat       = factor(as.integer(rural > 0), levels = c(0, 1)),  # binary factor
    discipline_fact = as.factor(category)
  )

# Stats  ---------------------------------------------------------------
#rural ~ stigma model
r_stig <- glm.nb(stigma ~ rural_cat, data = data_st)
summary(r_stig)
tab_model(r_stig)

data_st %>% group_by(rural_cat) %>% summarize(m=mean(stigma))

#rural ~ destig model
r_destig <- glm.nb(destigma ~ rural_cat, data = data_st)
summary(r_destig)
tab_model(r_destig)

#relevel to make General and Internal Medicine the reference value
df_long_st$discipline_fact <- relevel(df_long_st$discipline_fact, ref = "Medicine_General_Internal")

#additive model stigma + rural_cat
r_stig_dis <- glm.nb(stigma ~ rural_cat + discipline_fact, data = df_long_st)
tab_model(r_stig_dis)

#additive model destig + rural_cat
r_destig_dis <- glm.nb(destigma ~ rural_cat + discipline_fact, data = df_long_st)
tab_model(r_destig_dis)

#interaction model stigma + rural + mental health disciplines
mh_disciplines <- c("Psychiatry", "Clinical_Psychology", "Behavioral_Sciences")
su_disciplines <- c("Substance_Abuse")
gen_disciplines <- c("Public_Environmental_Occupational_Health", "Primary_Health", "Emergency_Medicine", "Critical_Care_Medicine", "Medicine_General_Internal")
other_spec_disciplines <- c("Clinical_Neurology", "Endocrinology_Metabolism", "Genetics_Heredity", "Integrative_Complementary_Medicine", "Medical_Informatics", "Medicine_Research_Experimental", "Neuroimaging", "Neurosciences", "Pathology", "Pharmacology_Pharmacy", "Toxicology")

#count variables for disciplinary breakdowns for interaction model
df_long_st <- df_long_st %>%
  mutate(
    generalist      = factor(as.integer(category %in% gen_disciplines), levels = c(0, 1)),
    mental_health   = factor(as.integer(category %in% mh_disciplines), levels = c(0, 1)),
    addiction       = factor(as.integer(category %in% su_disciplines), levels = c(0, 1)),
    other_specialist = factor(as.integer(category %in% other_spec_disciplines), levels = c(0, 1)))

#generalist disciplines not included so that they serve as reference category, consistent with other comparisons
r_stig_int <- glm.nb(
  stigma ~ rural_cat * addiction +
    rural_cat * mental_health +
    rural_cat * other_specialist,
  data = df_long_st
)

tab_model(r_stig_int)

#interaction model reformat
df_long_st <- df_long_st %>%
  mutate(
    discipline_group = case_when(
      category %in% mh_disciplines         ~ "Mental_Health",
      category %in% su_disciplines         ~ "Addiction",
      category %in% gen_disciplines        ~ "Generalist",
      category %in% other_spec_disciplines ~ "Other_Specialist"
    ),
    discipline_group = factor(discipline_group,
                              levels = c("Generalist", "Mental_Health", "Addiction", "Other_Specialist"))
  )

stig_int <- glm.nb(stigma ~ rural_cat+discipline_group, 
                   data = df_long_st)
tab_model(stig_int)

#destig interaction model
destig_int <- glm.nb(destigma ~ rural_cat*discipline_group, 
                   data = df_long_st)
tab_model(destig_int)

# Question: Does the presence of rural terms associate with the rate of stigma terms (Answer: yes)
r_stig <- glm.nb(stigma ~ rural_cat, data = data_st)
summary(r_stig)
tab_model(r_stig)

# Interaction model and the model without it (same rows, same data)
m3 <- glm.nb(stigma ~ rural_cat * discipline_group, data = df_long_st)
tab_model(m3)

m2 <- update(m3, . ~ rural_cat + discipline_group)
tab_model(m2)

# Cluster-robust variance matrix (replace abstract_id with your ID variable)
vc3 <- vcovCL(m3, cluster = df_long_st$X, type = "HC1")

# Coefficient table with robust SEs
coeftest(m3, vcov = vc3)

# Robust 95% CIs as IRRs
exp(cbind(IRR = coef(m3), coefci(m3, vcov = vc3)))

# Omnibus Wald test for the interaction terms (replaces the LRT)
waldtest(m2, m3,
         vcov = function(x) vcovCL(x, cluster = df_long_st$abstract_id, type = "HC1"),
         test = "Chisq")


# ROBUST TEST FOR INTERACTION MODEL  --------------------------------------
library(lmtest)

# Interaction model and the model without it (same rows, same data)
m3 <- glm.nb(stigma ~ rural_cat * discipline_group, data = df_long_st)
m2 <- update(m3, . ~ rural_cat + discipline_group)

# Cluster-robust variance matrix (replace abstract_id with your ID variable)
vc3 <- vcovCL(m3, cluster = df_long_st$X, type = "HC1")

# Coefficient table with robust SEs
coeftest(m3, vcov = vc3)

# Robust 95% CIs as IRRs
exp(cbind(IRR = coef(m3), coefci(m3, vcov = vc3)))

# Omnibus Wald test for the interaction terms (replaces the LRT)
waldtest(m2, m3,
         vcov = function(x) vcovCL(x, cluster = df_long_st$abstract_id, type = "HC1"),
         test = "Chisq")

# ROBUST RESULTS FOR M2 ---------------------------------------------------

vc2 <- vcovCL(m2, cluster = df_long_st$abstract_id, type = "HC1")
coeftest(m2, vcov = vc2)
exp(cbind(IRR = coef(m2), coefci(m2, vcov = vc2)))

#DESTIGMA ROBUST TESTING-------------------------------------------------------------------------------
# Question: Does the presence of rural terms associate with the rate of destigma terms (Answer: yes)
r_stig <- glm.nb(destigma ~ rural_cat, data = data_st)
summary(r_stig)
tab_model(r_stig)

# Interaction model and the model without it (same rows, same data)
dm3 <- glm.nb(destigma ~ rural_cat * discipline_group, data = df_long_st)
tab_model(dm3)

dm2 <- update(dm3, . ~ rural_cat + discipline_group)
tab_model(dm2)

# Cluster-robust variance matrix (replace abstract_id with your ID variable)
dvc3 <- vcovCL(dm3, cluster = df_long_st$X, type = "HC1")

# Coefficient table with robust SEs
coeftest(dm3, vcov = vc3)

# Robust 95% CIs as IRRs
exp(cbind(IRR = coef(dm3), coefci(dm3, vcov = vc3)))

# Omnibus Wald test for the interaction terms (replaces the LRT)
waldtest(dm2, dm3,
         vcov = function(x) vcovCL(x, cluster = df_long_st$abstract_id, type = "HC1"),
         test = "Chisq")


# ROBUST TEST FOR INTERACTION MODEL  --------------------------------------
library(lmtest)

# Interaction model and the model without it (same rows, same data)
dm3 <- glm.nb(destigma ~ rural_cat * discipline_group, data = df_long_st)
tab_model(dm3)
dm2 <- update(dm3, . ~ rural_cat + discipline_group)
tab_model(dm2)

# Cluster-robust variance matrix (replace abstract_id with your ID variable)
dvc3 <- vcovCL(dm3, cluster = df_long_st$X, type = "HC1")

# Coefficient table with robust SEs
coeftest(dm3, vcov = dvc3)

# Robust 95% CIs as IRRs
exp(cbind(IRR = coef(dm3), coefci(dm3, vcov = dvc3)))

# Omnibus Wald test for the interaction terms (replaces the LRT)
waldtest(dm2, dm3,
         vcov = function(x) vcovCL(x, cluster = df_long_st$abstract_id, type = "HC1"),
         test = "Chisq")

# Robust Results for DM2 

dvc2 <- vcovCL(dm2, cluster = df_long_st$abstract_id, type = "HC1")
coeftest(dm2, vcov = dvc2)
exp(cbind(IRR = coef(dm2), coefci(dm2, vcov = dvc2)))
