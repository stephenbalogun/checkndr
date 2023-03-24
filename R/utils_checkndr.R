#' Recency Download Options
#'
#' @return NULL. Used for side effect
#' @keywords internal
#'
#' @examples NULL
#'
recency_download_opts <- function() {
  c(opts_one(), opts_two())
}


opts_one <- function() {
  c(
    "Client state", "Client LGA", "Sex", "Age", "Visit date", "Screening result", "Confirmatory test",
    "Tie breaker", "Testing point"
  )
}

opts_two <- function() {
  c(
    "Opt out", "Recency test name", "Recency test date", "Recency test before visit date",
    "Recency number", "Invalid recency number", "Control line", "Verification line", "Longterm line",
    "Interpreted longterm", "Interpreted recent", "Interpreted negative", "Interpreted invalid",
    "Viral load requested", "VL sample collection date", "Viral load result", "Partial duplicates"
  )
}


#' Recency Line-list Variables
#'
#' @return NULL. Used for side effect
#' @keywords internal
#'
#' @examples NULL
recency_var_names <- function() {
  c(
    "ip", "facility_state", "facility_lga", "facility", "client_state", "client_lga", "sex", "date_of_birth", "age",
    "age_group", "client_id", "visit_date", "hts_screening_result", "hts_result", "hts_confirmatory_result",
    "hts_tie_breaker_result", "testing_point", "index_client", "opt_out", "recency_test_name", "recency_test_date",
    "recency_number", "control_line", "verification_line", "longterm_line", "recency_interpretation",
    "final_recency_result", "viral_load_requested", "viral_load_result", "date_of_viral_load_result",
    "date_sample_collected", "date_sample_sent"
  )
}




#' HTS Download Options
#'
#' @return NULL. Used for side effect
#' @keywords internal
#'
#' @examples NULL
#'
hts_download_opts <- function() {
  c(
    "Client state", "Client LGA", "Sex", "Age", "Visit date", "First time visit", "Retesting for verification",
    "Session type", "Referred from", "Marital status", "Children under five", "Screening result", "Confirmatory test",
    "Tie breaker", "Testing point", "Recency test", "Recency number", "HTS setting", "Consent",
    "Control line", "Verification line", "Longterm line", "Interpreted longterm", "Interpreted recent",
    "Post test counseling", "Interpreted negative", "Interpreted invalid", "Recency result", "Recency interpretation",
    "Viral load requested", "Date sample collected", "Date sample sent", "Viral load result", "Partial duplicates"
  )
}



#' HTS Line-list Variables
#'
#' @return NULL. Used for side effect
#' @keywords internal
#'
#' @examples NULL
hts_var_names <- function() {
  c(
    "facility_state", "facility_lga", "datim_code", "facility_name", "client_code",
    "patient_identifier", "hospital_number", "client_state", "client_lga", "visit_date",
    "sex", "date_of_birth", "age", "age_group", "first_time_visit",
    "retesting_for_result_verification", "session_type", "referred_from", "marital_status", "no_of_own_children_lessthan_5",
    "no_of_wives", "previously_tested_hiv_negative", "client_pregnant", "hts_screening_result", "hts_screening_date",
    "hts_test_result", "hbv_test_result", "hvc_test_result", "syphilis_test_result", "hts_setting",
    "consent", "recency_assay", "recency_test_date", "recency_test_name", "final_recency_result",
    "recency_interpretation", "viral_load_result", "viral_load_result_date", "confirmatory_test_result",
    "confirmatory_test_date", "tie_breaker_test_result", "tie_breaker_test_date", "tested_for_hiv_before_within_this_year",
    "post_test_counselling_done", "control_line", "verification_line", "long_term_line", "is_index_client",
    "index_type", "index_client_id", "recency_number", "viral_load_request", "date_sample_collected",
    "date_sample_sent", "created_at"
  )
}
