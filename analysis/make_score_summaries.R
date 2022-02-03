library(tidyverse)

psofa_data <- read_rds("output/psofa_data.rds")

psofa_summary <- psofa_data %>%
  mutate(psofa_above_zero = if_else(psofa_score > 0, 1, 0)) %>%
  group_by(child_mrn_uf, encounter) %>%
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
  group_by(child_mrn_uf, encounter) %>%
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

  ) %>%
  group_by(child_mrn_uf, encounter) %>%
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

write_csv(psofa_summary, "output/psofa_summary.csv")
write_csv(drug_summary, "output/drug_summary.csv")
write_csv(vis_summary, "output/vis_summary.csv")
