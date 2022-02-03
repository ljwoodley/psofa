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

vis_summary <- psofa_data %>%
  mutate(
    vis_score = epinephrine * 100 + dopamine + norepinephrine * 100 + dobutamine,
    vis_above_zero = if_else(vis_score > 0, 1, 0)
  ) %>%
  group_by(child_mrn_uf, encounter) %>%
  summarise(
    across(
      c(epinephrine,
        dopamine,
        norepinephrine,
        dobutamine,
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
  )

write_csv(psofa_summary, "output/psofa_summary.csv")
write_csv(vis_summary, "output/vis_summary.csv")
