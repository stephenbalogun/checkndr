
options(shiny.maxRequestSize = 500 * 1024^2)


recency_download_opts <- function() {
  c(
    "Client state", "Client LGA", "Sex", "Age", "Visit date", "Screening result", "Confirmatory test",
    "Tie breaker", "Testing point", "Opt out", "Recency test", "Recency number", "Control line",
    "Verification line", "Longterm line", "Interpreted longterm", "Interpreted recent", "Interpreted negative",
    "Interpreted invalid", "Viral load requested", "Viral load sample collection date", "Viral load result"
  )
}



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
