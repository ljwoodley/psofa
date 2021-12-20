#' Calculate coagulation score
#' @description Ingests the raw child labs and calculates the coagulation score based on the platelet count.
#'              When there are multiple platelet counts recorded within an hour the last recorded value is kept.
#'
#' @param read_child_labs the raw child labs csv file obtained from the IDR
#'
#' @return A data frame with the calculated coagulation score
#'
#' @export
get_coagulation <- function(read_child_labs) {

  coagulation <- read_child_labs %>%
    dplyr::filter(.data$lab_name == 'platelet count' &
                    stringr::str_detect(.data$lab_result, "\\d")) %>%
    dplyr::mutate(
      lab_result = readr::parse_number(.data$lab_result),
      coagulation_score = dplyr::case_when(
        .data$lab_result < 20 ~ 4,
        .data$lab_result < 50 ~ 3,
        .data$lab_result < 100 ~ 2,
        .data$lab_result < 150 ~ 1,
        TRUE ~ 0
      ),
      q1hr = lubridate::floor_date(.data$inferred_specimen_datetime, "1hour")
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    # if there are multiple values within an hour take the last recorded value
    dplyr::filter(.data$inferred_specimen_datetime == max(.data$inferred_specimen_datetime)) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$coagulation_score)

  return(coagulation)
}
