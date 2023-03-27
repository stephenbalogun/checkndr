#' Perform Recency Data Quality Check
#'
#' @param df dataframe of recency line-list
#'
#' @keywords internal
#' @return table of data quality check
#'
#' @examples NULL
dqa_pos <- function(df) {
  obs_client_state <- glue::glue("{sum(is.na(df$client_state), na.rm = TRUE)} clients did not have a documented state of residence")

  obs_client_lga <- glue::glue("{sum(is.na(df$client_lga), na.rm = TRUE)} clients did not have a documented LGA of residence")

  obs_sex <- glue::glue("{sum(is.na(df$sex) + !df$sex %in% c('M', 'F', 'Male', 'Female', 'male', 'female'), na.rm = TRUE)} clients did not have a documented gender or is neither 'Male' nor 'Female'")

  obs_age <- glue::glue("{sum(is.na(df$age), na.rm = TRUE) + sum(df$age < 15, na.rm = TRUE)} clients either did not have a documented age or the age documented is less than 15 years")

  obs_visit_date <- glue::glue("{sum(is.na(df$visit_date), na.rm = TRUE)} clients did not have a documented visit date")

  obs_hts_result <- glue::glue("{sum(!df$hts_screening_result %in% c('R', 'Pos'), na.rm = TRUE)} clients did not have a 'reactive' screening result")

  obs_hts_confirmatory <- glue::glue("{sum(!df$hts_confirmatory_result %in% c('R', 'Pos', 'NR', 'Neg', 'Invalid'), na.rm = TRUE)} clients did not have a documented confirmatory result")

  obs_hts_tie <- glue::glue("{sum(df$hts_confirmatory_result %in% c('NR', 'Neg') & !df$hts_tie_breaker_result %in% c('R', 'Pos'), na.rm = TRUE)} clients with negative confirmatory result did not have a positive tie breaker")

  obs_wrong_hts <- glue::glue("{sum(df$hts_result %in% c('Pos', 'POS', 'pos') & (df$hts_screening_result %in% c('R', 'Pos') + df$hts_confirmatory_result %in% c('R', 'Pos') + df$hts_tie_breaker_result %in% c('R', 'Pos'))  < 2 | df$hts_result %in% c('Neg', 'neg', 'NEG') & (df$hts_screening_result %in% c('R', 'Pos') + df$hts_confirmatory_result %in% c('R', 'Pos') + df$hts_tie_breaker_result %in% c('R', 'Pos')) >= 2, na.rm = TRUE)} clients have their HTS test results wrongly interpreted")

  obs_testing_point <- glue::glue("{sum(is.na(df$testing_point), na.rm = TRUE)} clients did not have a documented HTS testing point")

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
    "HTS result interpretation", obs_wrong_hts,
    "HTS testing point", obs_testing_point
  )
}




dqa_recency <- function(df) {
  obs_opt_out <- glue::glue("{sum(is.na(df$opt_out) & !is.na(df$recency_test_name), na.rm = TRUE)} clients did not have a documented `opt_out` status but had Asante recency test done")

  obs_recency_test_name <- glue::glue("{sum(!df$recency_test_name %in% c('Asante', 'AS'), na.rm = TRUE)} clients did not opt out of recency testing but have no recency test name}")

  obs_recency_test_date <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & is.na(df$recency_test_date), na.rm = TRUE)} clients have a documented recency test but no date of testing for recency")

  obs_test_before_visit <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & df$recency_test_date < df$visit_date, na.rm = TRUE)} clients had their recency test date preceding the service visit date")

  obs_recency_number <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & is.na(df$recency_number), na.rm = TRUE)} clients have a documented recency test but no recency number")

  obs_valid_recency_number <- glue::glue("{sum(!stringr::str_detect(df$recency_number, '[:alpha:]{2}[:digit:]{8}'), na.rm = TRUE)} clients did not have the expected format for the recency number")

  obs_control_line <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$control_line %in% c('Yes', 'No'), na.rm = TRUE)} clients with a documented recency test did not have the control line outcome")

  obs_verification_line <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$verification_line %in% c('Yes', 'No'), na.rm = TRUE)} clients with a documented recency test did not have the verification line outcome")

  obs_longterm_line <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$longterm_line %in% c('Yes', 'No'), na.rm = TRUE)} clients with a documented recency test did not have the long-term line outcome")

  obs_interpretation_longterm <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'LongTerm', na.rm = TRUE)} clients with all lines visible were not documented as 'Long-term' results")

  obs_interpretation_recent <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Recent', na.rm = TRUE)} clients with only control and verification lines visible were not documented as 'Recent' results")

  obs_interpretation_negative <- glue::glue("{sum(df$control_line %in% 'Yes' & !df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Negative', na.rm = TRUE)} clients with only the control line were not documented as 'Negative' results")

  obs_interpretation_invalid <- glue::glue("{sum(df$recency_test_name %in% c('Asante', 'AS') & !df$control_line %in% 'Yes' & !df$recency_interpretation %in% 'Invalid', na.rm = TRUE) + sum(df$control_line %in% 'Yes' & !df$verification_line %in% 'Yes' & df$longterm_line %in% 'Yes' & !df$recency_interpretation %in% 'Invalid', na.rm = TRUE)} clients with either no control line or have longterm line but no verification line but were not documented as 'Invalid' results")

  obs_vl_request <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & !df$viral_load_requested %in% 'Yes', na.rm = TRUE)} clients were identified as 'Recent' but did not have their viral load samples requested")

  obs_vl_sample <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & df$viral_load_requested %in% 'Yes' & is.na(df$date_sample_collected), na.rm = TRUE)} clients were identified as 'Recent' but did not have their date of sample colllection documented")

  obs_vl_result <- glue::glue("{sum(df$control_line %in% 'Yes' & df$verification_line %in% 'Yes' & !df$longterm_line %in% 'Yes' & df$viral_load_requested %in% 'Yes' & df$recency_test_date < max(df$recency_test_date, na.rm = TRUE) - lubridate::days(28) & is.na(df$date_of_viral_load_result), na.rm = TRUE)} clients with RTRI_RECENT results were identified over 4 weeks ago but are still without viral load result")

  obs_partial_duplicates <- glue::glue("{nrow(df |>  janitor::get_dupes(sex, date_of_birth, facility, visit_date))} entries are at least partially duplicated with the same 'sex', 'date_of_birth', 'facility', 'visit_date' and 'HIV status'")

  tibble::tribble(
    ~variables, ~observations,
    "Opt out", obs_opt_out,
    "Recency test name", obs_recency_test_name,
    "Recency test date", obs_recency_test_date,
    "Recency date date before visit date", obs_test_before_visit,
    "Recency number", obs_recency_number,
    "Valid recency number", obs_valid_recency_number,
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
