library(tidyverse)
library(here)

cohort <- "nicu"

nicu_term <- readxl::read_excel(here("data", cohort, "NICU-term MRNs for pSOFA.xlsx"))

nicu_term_psofa_summary <- read_csv(here("output", cohort, "nicu_psofa_summary.csv")) %>%
  filter(child_mrn_uf %in% nicu_term$child_mrn_uf)

nicu_term_drug_dose_summary <- read_csv(here("output", cohort, "nicu_drug_dose_summary.csv")) %>%
  filter(child_mrn_uf %in% nicu_term$child_mrn_uf)

nicu_term_vis_summary <- read_csv(here("output", cohort, "nicu_vis_summary.csv")) %>%
  filter(child_mrn_uf %in% nicu_term$child_mrn_uf)

nicu_term_q1hr_drug_dosage_summary <- read_csv(here("output", cohort, str_c(cohort, "_q1hr_drug_dosage_summary.csv"))) %>%
  filter(child_mrn_uf %in% nicu_term$child_mrn_uf)

write_csv(nicu_term_psofa_summary, here("output", cohort, str_c(cohort, "_term_psofa_summary.csv")))
write_csv(nicu_term_drug_dose_summary, here("output", cohort, str_c(cohort, "_term_drug_dose_summary.csv")))
write_csv(nicu_term_vis_summary, here("output", cohort, str_c(cohort, "_term_vis_summary.csv")))
write_csv(nicu_term_q1hr_drug_dosage_summary, here("output", cohort, str_c(cohort, "_term_q1hr_drug_dosage_summary.csv")))
