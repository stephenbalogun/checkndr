#' Check for Number of rows with Valid Entries
#'
#' @param df dataframe of line list
#'
#' @keywords internal
#' @return table of rows with valid entries in all cells
#'
#' @examples NULL
# valid_cases <- function(df){
#' #  df |>
#' #    subset(
#' #      !is.na(client_state) &
#' #      !is.na(client_lga) &
#' #      sex %in% c("Male", "Female", "M", "F", "male", "female", "m", "f") &
#' #      age >= 15 &
#' #      !is.na(visit_date) &
#' #      hts_result %in% c("R", "Pos") &
#' #      hts_confirmatory_result %in% c("R", "Pos") | hts_tie_breaker_result %in% c("R", "Pos") &
#' #      !is.na(opt_out) &
#' #      !is.na(testing_point) &
#' #      recency_test_name %in% c("Asante", "AS") &
#' #      !is.na(recency_test_date) &
#' #      !is.na(recency_number) &
#' #      control_line %in% c("Yes", "No") &
#' #      verification_line %in% c("Yes", "No") &
#' #      longterm_line %in% c("Yes", "No") &
#' #      control_line %in% "Yes" & verification_line %in% "Yes" & longterm_line %in% "Yes" & recency_interpretation %in% "LongTerm" |
#' #      control_line %in% "Yes" & verification_line %in% "Yes" & longterm_line %in% "No" & recency_interpretation %in% "Recent" |
#' #      control_line %in% "Yes" & verification_line %in% "No" & longterm_line %in% "No" & recency_interpretation %in% "Negative" |
#' #      control_line %in% "No" & recency_interpretation %in% "Invalid" |
#' #      control_line %in% "Yes" & verification_line %in% "No" & longterm_line %in% "Yes" & recency_interpretation %in% "Invalid"|
#' #      control_line %in% "Yes" & verification_line %in% "Yes" & longterm_line %in% "No" & viral_load_requested %in% "Yes" &
#' #      !is.na(date_sample_collected) &
#' #      ifelse(
#' #        recency_interpretation %in% "Recent" & visit_date + lubridate::days(42) < max(visit_date, na.rm = TRUE),
#' #        !is.na(date_of_viral_load_result),
#' #        is.na(date_of_viral_load_result)
#' #      )
#' #    )
# }
valid_cases <- function(df) {
  df |>
    dplyr::filter(
      !is.na(recency_test_name),
      !is.na(opt_out),
      !is.na(client_state),
      !is.na(client_lga),
      sex %in% c("Male", "Female", "M", "F", "male", "female", "m", "f"),
      age >= 15,
      !is.na(visit_date),
      hts_screening_result %in% c("R", "Pos"),
      hts_confirmatory_result %in% c("R", "Pos") | hts_tie_breaker_result %in% c("R", "Pos"),
      hts_result %in% c("Pos", "POS", "pos") & hts_confirmatory_result %in% c("R", "Pos") |
        hts_result %in% c("Pos", "POS", "pos") & !hts_confirmatory_result %in% c("R", "Pos") & hts_tie_breaker_result %in% c("R", "Pos"),
      !is.na(opt_out),
      !is.na(testing_point),
      recency_test_name %in% c("Asante", "AS"),
      !is.na(recency_test_date) | recency_test_date >= visit_date,
      !is.na(recency_number),
      stringr::str_detect(recency_number, "[:alpha:]{2}[:digit:]{8}"),
      control_line %in% c("Yes", "No"),
      verification_line %in% c("Yes", "No"),
      longterm_line %in% c("Yes", "No"),
      control_line %in% "Yes" &
        verification_line %in% "Yes" &
        longterm_line %in% "Yes" &
        recency_interpretation %in% "LongTerm" |
        control_line %in% "Yes" &
          verification_line %in% "Yes" &
          longterm_line %in% "No" &
          recency_interpretation %in% "Recent" |
        control_line %in% "Yes" &
          verification_line %in% "No" &
          longterm_line %in% "No" &
          recency_interpretation %in% "Negative" |
        control_line %in% "No" &
          recency_interpretation %in% "Invalid" |
        control_line %in% "Yes" &
          verification_line %in% "No" &
          longterm_line %in% "Yes" &
          recency_interpretation %in% "Invalid" |
        control_line %in% "Yes" &
          verification_line %in% "Yes" &
          longterm_line %in% "No" &
          viral_load_requested %in% "Yes" &
          !is.na(date_sample_collected),
      ifelse(
        recency_interpretation %in% "Recent" &
          visit_date + lubridate::days(28) < max(visit_date, na.rm = TRUE),
        !is.na(date_of_viral_load_result),
        is.na(date_of_viral_load_result)
      )
    )
}



utils::globalVariables(
  c(
    "client_state", "client_lga", "sex", "age", "visit_date",
    "hts_result", "hts_confirmatory_result", "hts_tie_breaker_result",
    "testing_point", "opt_out", "recency_test_name", "recency_number",
    "recency_test_date", "control_line", "verification_line",
    "longterm_line", "recency_interpretation", "viral_load_requested",
    "date_sample_collected", "date_of_viral_load_result"
  )
)
