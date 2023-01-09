#' Perform Recency Data Quality Check
#'
#' @param df dataframe of recency line-list
#'
#' @keywords internal
#' @return table of data quality check
#'
#' @examples NULL
recency_dqa <- function(df) {
  obs_client_state <- glue::glue("{sum(is.na(df$client_state))} patients did not have a documented state of residence")

  obs_client_lga <- glue::glue("{sum(is.na(df$client_lga))} patients did not have a documented LGA of residence")

  obs_sex <- glue::glue("{sum(is.na(df$sex) + !df$sex %in% c('M', 'F', 'Male', 'Female', 'male', 'female'))} patients did not have a documented gender or is neither 'Male' nor 'Female'")

  obs_age <- glue::glue("{sum(is.na(df$age)) + sum(df$age < 15, na.rm = TRUE)} patients either did not have a documented age or the age documented is less than 15 years")

  obs_visit_date <- glue::glue("{sum(is.na(df$visit_date))} patients did not have a documented visit date")

  obs_hts_result <- glue::glue("{sum(!df$hts_result %in% c('R', 'Pos'))} patients did not have a 'reactive' screening result")

  obs_hts_confirmatory <- glue::glue("{sum(!df$hts_confirmatory_result %in% c('R', 'Pos', 'NR', 'Neg', 'Invalid'))} patients did not have a documented confirmatory result")

  obs_hts_tie <- glue::glue("{sum(df$hts_confirmatory_result %in% c('NR', 'Neg') & !df$hts_tie_breaker_result %in% c('R', 'Pos'))} patients with negative confirmatory result did not have a positive tie breaker")

  obs_testing_point <- glue::glue("{sum(is.na(df$testing_point))} patients did not have a documented HTS testing point")

  obs_recency_test <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & is.na(df$recency_test_date))} patients have a documented recency test but no date of testing for recency")

  obs_recency_number <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & is.na(df$recency_number))} patients have a documented recency test but no recency number")

  obs_control_line <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$control_line %in% c('Yes', 'No'))} patients with a documented recency test did not have the control line outcome")

  obs_verification_line <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$verification_line %in% c('Yes', 'No'))} patients with a documented recency test did not have the verification line outcome")

  obs_longterm_line <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$longterm_line %in% c('Yes', 'No'))} patients with a documented recency test did not have the long-term line outcome")

  obs_interpretation_longterm <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'LongTerm')} patients with all lines visible were not documented as 'Long-term' results")

  obs_interpretation_recent <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Recent')} patients with only control and verification lines visible were not documented as 'Recent' results")

  obs_interpretation_negative <- glue::glue("{sum(df$control_line %in% 'Yes' & !df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Negative')} patients with only the control line were not documented as 'Negative' results")

  obs_interpretation_invalid <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$control_line %in% 'Yes' & !df$recency_interpretation %in% 'Invalid') + sum(df$control_line %in% 'Yes' & !df$verification_line %in% 'Yes' & df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Invalid')} patients with either no control line or have longterm line but no verification line but were not documented as 'Invalid' results")

  obs_vl_request <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$viral_load_requested %in% 'Yes')} patients were identified as 'Recent' but did not have their viral load samples requested")

  obs_vl_sample <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & df$viral_load_requested %in% 'Yes' & is.na(df$date_sample_collected))} patients were identified as 'Recent' but did not have their date of sample colllection documented")

  obs_vl_result <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & df$viral_load_requested %in% 'Yes' & df$visit_date < max(df$visit_date, na.rm = TRUE) - lubridate::days(42) & is.na(df$date_of_viral_load_result))} patients with RTRI_RECENT results were identified over 6 weeks ago but are still without viral load result")

  obs_partial_duplicates <- glue::glue("{nrow(df |>  janitor::get_dupes(sex, date_of_birth, facility, visit_date))} entries are partially duplicated with the same 'sex', 'date_of_birth', 'facility', 'visit_date' and 'HIV status'")

  tibble::tribble(
    ~variables, ~observations,
    "Client state", obs_client_state,
    "Client LGA", obs_client_lga,
    "Sex", obs_sex,
    "Age", obs_age,
    "Visit date", obs_visit_date,
    "Screening result", obs_hts_result,
    "Confirmatory test", obs_hts_confirmatory,
    "Tie breaker test", obs_hts_tie,
    "HTS testing point", obs_testing_point,
    "Recency test", obs_recency_test,
    "Recency number", obs_recency_number,
    "Control line", obs_control_line,
    "Verification line", obs_verification_line,
    "Longterm line", obs_longterm_line,
    "Interpreted long-term", obs_interpretation_longterm,
    "Interpreted recent", obs_interpretation_recent,
    "Interpreted negative", obs_interpretation_negative,
    "Interpreted invalid", obs_interpretation_invalid,
    "Viral load requested", obs_vl_request,
    "Viral load sample date", obs_vl_sample,
    "Viral load results", obs_vl_result,
    "Partial duplicates", obs_partial_duplicates
  )
}
