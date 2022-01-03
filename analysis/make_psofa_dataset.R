library(tidyverse)
library(janitor)
library(lubridate)
library(psofa)
library(here)

categorized_respiratory_devices <- readxl::read_excel(here(
    "output",
    "categorized_respiratory_devices_2021-09-24-JLW.xlsx"
  ),
  sheet = 4) %>%
  filter(on_resp_support == 0)

read_child_encounter <- read_csv(here("data", "picu", "ctsit_child_encounter.csv")) %>%
  clean_names()

child_encounter <- get_child_encounter(read_child_encounter)

transformed_child_encounter <- child_encounter$transformed_child_encounter

expanded_child_encounter <- child_encounter$expanded_child_encounter

child_dob <- read_child_encounter %>%
  distinct(child_mrn_uf, child_birth_date)

read_child_labs <- read_csv(here("data", "picu", "PICU-child_labs.csv")) %>%
  clean_names() %>%
  filter(child_mrn_uf %in% transformed_child_encounter$child_mrn_uf) %>%
  mutate_if(is.character, tolower)

read_glasgow <- read_csv(here("data", "picu", "Child_Glasgow_PICU.csv")) %>%
  clean_names()

read_flowsheets <- read_csv(here("data", "picu", "PICU-child_flowsheets.csv")) %>%
  clean_names() %>%
  filter(child_mrn_uf %in% transformed_child_encounter$child_mrn_uf) %>%
  mutate_if(is.character, tolower)

read_medications <- read_csv(here("data", "picu", "PICU-child_medications.csv")) %>%
  clean_names() %>%
  filter(child_mrn_uf %in% transformed_child_encounter$child_mrn_uf) %>%
  mutate_if(is.character, tolower)

coagulation <- get_coagulation(read_child_labs)

hepatic <- get_hepatic(read_child_labs)

neurologic <- get_neurologic(read_glasgow)

renal <- get_renal(read_child_labs, child_dob)

cv_by_age_group <- get_cv_by_age_group(read_flowsheets, child_dob)

cv_by_vasoactive_infusion <- get_cv_by_vasoactive_infusion(read_medications, expanded_child_encounter)

cardiovascular <- get_cardiovascular(cv_by_age_group, cv_by_vasoactive_infusion)

respiratory_support_status <- get_respiratory_status(read_flowsheets, categorized_respiratory_devices)

pao2_fio2_resp_score <- get_pao2_fio2_resp_score(read_child_labs, transformed_child_encounter, respiratory_support_status)

nasal_flow_rate_state <- get_nasal_state(read_flowsheets)

complete_fio2_value <- get_fio2_value(
  categorized_respiratory_devices,
  read_flowsheets,
  nasal_flow_rate_state
)

spo2_value <- get_spo2_value(read_flowsheets)

respiratory <- get_respiratory(
  expanded_child_encounter,
  pao2_fio2_resp_score,
  complete_fio2_value,
  spo2_value,
  respiratory_support_status,
  nasal_flow_rate_state
)

psofa_score <- list(
  expanded_child_encounter,
  coagulation,
  hepatic,
  neurologic,
  renal,
  cardiovascular,
  respiratory
) %>%
  reduce(left_join, by = c("child_mrn_uf", "q1hr")) %>%
  group_by(child_mrn_uf, encounter) %>%
  fill(ends_with("_score"), .direction = "down") %>%
  ungroup() %>%
  mutate_at(vars(ends_with("_score")), ~ replace(., is.na(.), 0)) %>%
  mutate(
    psofa_score = coagulation_score + hepatic_score + neurologic_score + renal_score + cardiovascular_score + respiratory_score
  )
