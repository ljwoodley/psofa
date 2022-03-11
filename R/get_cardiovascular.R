#' Calculate cardiovascular score based on age group
#'
#' @param read_flowsheets the raw flowsheet data obtained from the IDR
#' @param child_dob the dob of each subjects
#'
#' @return A data frame with the calculated cardiovascular score by age group
#' @export
get_cv_by_age_group <-
  function(read_flowsheets,
           child_dob) {
    cv_by_age_group <- read_flowsheets %>%
      dplyr::filter(
        .data$flowsheet_group == "vitals" &
          .data$flo_meas_name %in% c("r map cuff", "r map a-line", "r map a-line 2") &
          !is.na(.data$meas_value)
      ) %>%
      dplyr::select(.data$child_mrn_uf, .data$recorded_time, .data$meas_value) %>%
      dplyr::mutate(
        q1hr = lubridate::floor_date(.data$recorded_time, "1hour"),
        meas_value = as.numeric(.data$meas_value)
      ) %>%
      dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
      # if there are multiple values within an hour take the minimum value
      dplyr::filter(.data$meas_value == min(.data$meas_value)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(child_dob, by = "child_mrn_uf") %>%
      dplyr::mutate(
        age_in_months = lubridate::interval(.data$child_birth_date, lubridate::as_date(.data$q1hr)) %/% months(1),
        age_group_cardiovascular_score = dplyr::case_when(
          .data$age_in_months < 1 & .data$meas_value >= 46 ~ 0,
          .data$age_in_months < 1 & .data$meas_value < 46 ~ 1,
          .data$age_in_months <= 11 & .data$meas_value >= 55 ~ 0,
          .data$age_in_months <= 11 & .data$meas_value < 55 ~ 1,
          .data$age_in_months <= 23 & .data$meas_value >= 60 ~ 0,
          .data$age_in_months <= 23 & .data$meas_value < 60 ~ 1,
          .data$age_in_months <= 59 & .data$meas_value >= 62 ~ 0,
          .data$age_in_months <= 59 & .data$meas_value < 62 ~ 1,
          .data$age_in_months <= 143 & .data$meas_value >= 65 ~ 0,
          .data$age_in_months <= 143 & .data$meas_value < 65 ~ 1,
          .data$age_in_months <= 216 & .data$meas_value >= 67 ~ 0,
          .data$age_in_months <= 216 & .data$meas_value < 67 ~ 1,
          .data$age_in_months > 216 & .data$meas_value >= 70 ~ 0,
          .data$age_in_months > 216 & .data$meas_value < 70 ~ 1
        )
      )

    return(cv_by_age_group)

  }

#' Align the start and end time for a given drug
#'
#' @param vasoactive_infusions a df with vasoactive infusions created in \code{\link{get_cv_by_vasoactive_infusion}}
align_drug_start_end_time <- function(vasoactive_infusions) {
  aligned_times <- vasoactive_infusions %>%
    tidyr::expand(.data$child_mrn_uf,
                  .data$med_order_display_name,
                  q1hr = seq(min(.data$q1hr), max(.data$med_order_end_datetime), by = "1 hour"))


  aligned_drug_dose <- aligned_times %>%
    dplyr::left_join(
      vasoactive_infusions %>%
        dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$total_dose_character),
      by = c("child_mrn_uf", "q1hr")
    ) %>%
    tidyr::fill(.data$total_dose_character, .direction = "down")

  return(aligned_drug_dose)
}

#' Calculate cardiovascular score based on vasoactive infusions
#'
#' @param read_medications the raw medications data obtained from the IDR
#' @param expanded_child_encounter a df returned by \code{\link{get_child_encounter}}
#'
#' @return A list containing the calculated cardiovascular score based on vasoactive infusions
#'         and the hourly drug dosages
#' @export
get_cv_by_vasoactive_infusion <-
  function(read_medications,
           expanded_child_encounter) {
    vasoactive_infusions <- read_medications %>%
      dplyr::filter(
        .data$med_order_route == 'intravenous' &
          stringr::str_detect(
            .data$med_order_display_name,
            "dobutamine|dopamine|epinephrine|norepinephrine|milrinone|vasopressin"
          ) &
          .data$med_order_discrete_dose_unit %in% c('mcg/kg/min','milli-units/kg/min') &
          !is.na(.data$total_dose_character)
      ) %>%
      dplyr::select(
        .data$child_mrn_uf,
        .data$med_order_route,
        .data$med_order_display_name,
        .data$take_datetime,
        .data$total_dose_character,
        .data$med_order_discrete_dose_unit,
        .data$med_order_end_datetime
      ) %>%
      dplyr::mutate(
        q1hr = lubridate::floor_date(.data$take_datetime, "1hour"),
        med_order_end_datetime = lubridate::floor_date(.data$med_order_end_datetime, "1hour")
      ) %>%
      dplyr::group_by(.data$child_mrn_uf,
                      .data$med_order_display_name,
                      .data$q1hr) %>%
      # if the last score recorded within an hour is 0 keep that as
      # it signifies that the drug was ended.
      # Otherwise keep the highest value within an hour
      mutate(score_priority = case_when(
        .data$take_datetime == max(.data$take_datetime) & .data$total_dose_character == 0 ~ 3,
        .data$total_dose_character == max(.data$total_dose_character) ~ 2,
        TRUE ~ 1
      )) %>%
      dplyr::slice_max(.data$score_priority, with_ties = FALSE) %>%
      dplyr::arrange(.data$child_mrn_uf, .data$q1hr) %>%
      dplyr::select(
        .data$child_mrn_uf,
        .data$med_order_display_name,
        .data$med_order_end_datetime,
        .data$q1hr,
        .data$total_dose_character
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$med_order_end_datetime)) %>%
      dplyr::left_join(
        expanded_child_encounter %>%
          dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$encounter),
        by = c("child_mrn_uf", "q1hr")
      ) %>%
      # Check for volume changes within a minute for a given drug
      group_by(child_mrn_uf, q1hr, med_order_display_name) %>%
      mutate(med_order_display_name = stringr::word(.data$med_order_display_name, 1)) %>%
      slice_max(total_dose_character, with_ties = FALSE) %>%
      ungroup() %>%
      # time that drug ended cannot occur before patient was admitted
      dplyr::filter(.data$q1hr <= .data$med_order_end_datetime)

    cv_by_vasoactive_infusion <- vasoactive_infusions %>%
      dplyr::group_split(
        .data$child_mrn_uf,
        .data$encounter,
        .data$med_order_display_name,
        .data$med_order_end_datetime
      ) %>%
      purrr::map_df(., align_drug_start_end_time) %>%
      dplyr::group_by(.data$child_mrn_uf,
                      .data$med_order_display_name,
                      .data$q1hr) %>%
      dplyr::slice_max(.data$total_dose_character, with_ties = FALSE) %>%
      tidyr::pivot_wider(
        names_from = .data$med_order_display_name,
        values_from = .data$total_dose_character
      ) %>%
      dplyr::mutate_at(dplyr::vars(-c("child_mrn_uf", "q1hr")), ~ replace(., is.na(.), 0)) %>%
      dplyr::mutate(
        vasoactive_cardiovascular_score = dplyr::case_when(
          .data$dopamine > 15 |
            .data$epinephrine > 0.1 | .data$norepinephrine > 0.1 ~ 4,
          .data$dopamine > 5 |
            (.data$epinephrine > 0 & .data$epinephrine <= 0.1) |
            (.data$norepinephrine > 0 & .data$norepinephrine <= 0.1) ~ 3,
          .data$dopamine > 0 | .data$dobutamine > 0 ~ 2
        )
      )

    return(cv_by_vasoactive_infusion)
  }

#' Joins the age group and vasoactive infusion cardiovascular datasets
#'
#'
#' @param cv_by_age_group a df returned by \code{\link{get_cv_by_age_group}}
#' @param cv_by_vasoactive_infusion a df returned by \code{\link{get_cv_by_vasoactive_infusion}}
#'
#' @return A data frame with the calculated cardiovascular score based on vasoactive infusions
#' @export
get_cardiovascular <-
  function(cv_by_age_group,
           cv_by_vasoactive_infusion) {
    cardiovascular <- cv_by_age_group %>%
      dplyr::full_join(cv_by_vasoactive_infusion, by = c("child_mrn_uf", "q1hr")) %>%
      dplyr::arrange(.data$child_mrn_uf, .data$q1hr) %>%
      dplyr::mutate(
        cardiovascular_score = dplyr::coalesce(
          .data$vasoactive_cardiovascular_score,
          .data$age_group_cardiovascular_score
        )
      ) %>%
      dplyr::select(
        .data$child_mrn_uf,
        .data$q1hr,
        .data$dopamine,
        .data$norepinephrine,
        .data$dobutamine,
        .data$epinephrine,
        .data$vasopressin,
        .data$milrinone,
        .data$cardiovascular_score,
      ) %>%
      dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = TRUE)

    return(cardiovascular)
  }
