library(tidyverse)
library(here)

# identify cohort of interest. e.g picu/pcicu/nicu
cohort <- "nicu"

psofa_data <- read_rds(here("output", cohort, str_c(cohort, "_psofa_data.rds")))
q1hr_drug_dosages <- read_rds(here("output", cohort, str_c(cohort, "_q1hr_drug_dosages.rds")))


psofa_summary <- psofa_data %>%
  mutate(psofa_above_zero = if_else(psofa_score > 0, 1, 0)) %>%
  group_by(child_mrn_uf, encounter, admit_datetime, dischg_datetime, dischg_disposition) %>%
  summarise(
    across(ends_with("_score"), list(max = max, sum = sum), .names = "{.col}_{fn}"),
    num_hours_psofa_above_zero = sum(psofa_above_zero),
    total_hospitalization_time_in_hours = n()
  ) %>%
  mutate(
    total_time_in_encounter = round(
      num_hours_psofa_above_zero / total_hospitalization_time_in_hours,
      2
    )
  )

drug_dose_summary <- psofa_data %>%
  mutate(total_dosage = dopamine + dobutamine + milrinone + vasopressin + epinephrine + norepinephrine,
         dosage_above_zero = if_else(total_dosage > 0, 1, 0)) %>%
  group_by(child_mrn_uf, encounter, admit_datetime, dischg_datetime, dischg_disposition) %>%
  summarise(
    across(
      c(dopamine,
        dobutamine,
        milrinone,
        vasopressin,
        epinephrine,
        norepinephrine,
        total_dosage
        ),
      list(max = max, sum = sum),
      .names = "{.col}_{fn}"
    ),
    num_hours_dosage_above_zero = sum(dosage_above_zero),
    total_hospitalization_time_in_hours = n()
  ) %>%
  mutate(
    total_time_in_encounter = round(
      num_hours_dosage_above_zero / total_hospitalization_time_in_hours,
      2
    )
  ) %>%
  select(-contains("above_zero"))

vis_summary <- psofa_data %>%
  mutate(
    vis_milrinone = 10 * milrinone,
    vis_vasopressin = 10 * vasopressin,
    vis_epinephrine = 100 * epinephrine,
    vis_norepinephrine = 100 * norepinephrine,
    vis_score = dopamine + dobutamine + vis_milrinone + vis_vasopressin + vis_epinephrine + vis_norepinephrine,
    vis_above_zero = if_else(vis_score > 0, 1, 0)
  ) %>%
  group_by(child_mrn_uf, encounter, admit_datetime, dischg_datetime, dischg_disposition) %>%
  summarise(
    across(
      c(dopamine,
        dobutamine,
        starts_with("vis_"),
        vis_score),
      list(max = max, sum = sum),
      .names = "{.col}_{fn}"
    ),
    num_hours_vis_above_zero = sum(vis_above_zero),
    total_hospitalization_time_in_hours = n()
  ) %>%
  mutate(
    total_time_in_encounter = round(
      num_hours_vis_above_zero / total_hospitalization_time_in_hours,
      2
    )
  ) %>%
  select(-contains("above_zero"))

q1hr_drug_dosage_summary <- q1hr_drug_dosages %>%
  mutate_at(
    vars(
      "epinephrine",
      "dopamine",
      "norepinephrine",
      "dobutamine",
      "vasopressin",
      "milrinone"
    ),
    ~ replace(., is.na(.), 0)
  ) %>%
  mutate(
    vis_milrinone = 10 * milrinone,
    vis_vasopressin = 10 * vasopressin,
    vis_epinephrine = 100 * epinephrine,
    vis_norepinephrine = 100 * norepinephrine,
    vis_score = dopamine + dobutamine + vis_milrinone + vis_vasopressin + vis_epinephrine + vis_norepinephrine
  )

write_csv(psofa_summary, here("output", cohort, str_c(cohort, "_psofa_summary.csv")))
write_csv(drug_dose_summary, here("output", cohort, str_c(cohort, "_drug_dose_summary.csv")))
write_csv(vis_summary, here("output", cohort, str_c(cohort, "_vis_summary.csv")))
write_csv(q1hr_drug_dosage_summary, here("output", cohort, str_c(cohort, "_q1hr_drug_dosage_summary.csv")))
