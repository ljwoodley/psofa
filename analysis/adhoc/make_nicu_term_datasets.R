library(tidyverse)
library(here)
library(readxl)

# For this script to work make_psofa_dataset.R must first be run on the nicu cohort to
# create nicu_psofa_data and nicu_psofa_summary files.
# Finally, change the input file dates to match your local file dates

cohort <- "nicu"

nicu_term_mrns <- read_excel(here("data", cohort, "NICU-term MRNs for pSOFA.xlsx"))

nicu_psofa_data <- read_rds(here("output", cohort, "nicu_psofa_data_2022-04-07.rds"))
nicu_psofa_summary <- read_csv(here("output", cohort, "nicu_psofa_summary_2022-04-07.csv"))

nicu_term_psofa_data <- nicu_psofa_data %>%
  filter(child_mrn_uf %in% nicu_term_mrns$child_mrn_uf)

nicu_term_psofa_summary <- nicu_psofa_summary %>%
  filter(child_mrn_uf %in% nicu_term_mrns$child_mrn_uf)

write_csv(nicu_term_psofa_data, here("output", cohort, str_c(cohort, "_term_psofa_data_", today(), ".csv")))
write_csv(nicu_term_psofa_summary, here("output", cohort, str_c(cohort, "_term_psofa_summary_", today(), ".csv")))
