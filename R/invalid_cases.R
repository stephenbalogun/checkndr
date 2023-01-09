#' Check for Number of rows with Invalid Entries
#'
#' @param df dataframe of line list
#'
#' @keywords internal
#' @return table of rows with invalid entries in all cells
#'
#' @examples NULL
invalid_cases <- function(df){
  df |>
    dplyr::filter(
      is.na(client_state) |
        is.na(client_lga) |
        is.na(sex) | !sex %in% c("Male", "Female", "M", "F", "male", "female", "m", "f") |
        is.na(age) | age < 15 |
        is.na(visit_date) |
        !hts_result %in% c("R", "Pos") |
        !hts_confirmatory_result %in% c("R", "Pos", "NR", "Neg", "Invalid") |
        hts_confirmatory_result %in% c("NR", "Neg") & !hts_tie_breaker_result %in% c("R", "Pos") |
        is.na(testing_point) |
        recency_test_name %in% c("Asante", "AS") & is.na(recency_test_date) |
        recency_test_name %in% c("Asante", "AS") & is.na(recency_number) |
        recency_test_name %in% c("Asante", "AS") & !control_line %in% c("Yes", "No") |
        recency_test_name %in% c("Asante", "AS") & !verification_line %in% c("Yes", "No") |
        recency_test_name %in% c("Asante", "AS") & !longterm_line %in% c("Yes", "No") |
        control_line %in% "Yes" & verification_line %in% "Yes" & longterm_line %in% "Yes" & !recency_interpretation %in% "LongTerm" |
        control_line %in% "Yes" & verification_line %in% "Yes" & !longterm_line %in% "Yes" & !recency_interpretation %in% "Recent" |
        control_line %in% "Yes" & !verification_line %in% "Yes" & !longterm_line %in% "Yes" &
        !recency_interpretation %in% "Negative" |
        recency_test_name %in% c("Asante", "AS") & !control_line %in% "Yes" & !recency_interpretation %in% "Invalid" |
        control_line %in% "Yes" & !verification_line %in% "Yes" & longterm_line %in% "Yes" & !recency_interpretation %in% "Invalid" |
        control_line %in% "Yes" & verification_line %in% "Yes" & !longterm_line %in% "Yes" & !viral_load_requested %in% "Yes" |
        control_line %in% "Yes" & verification_line %in% "yes" & !longterm_line %in% "Yes" & viral_load_requested %in% "Yes" & is.na(date_sample_collected) |
        control_line %in% "Yes" & verification_line %in% "Yes" & !longterm_line %in% "Yes" & viral_load_requested %in% "Yes" & visit_date < max(visit_date, na.rm = TRUE) - lubridate::days(42) & is.na(date_of_viral_load_result)
    )
}


utils::globalVariables(
  c(
    "client_state",
    "client_lga",
    "sex",
    "age",
    "visit_date",
    "hts_result",
    "hts_confirmatory_result",
    "hts_tie_breaker_result",
    "testing_point",
    "recency_test_name",
    "recency_number",
    "recency_test_date",
    "control_line",
    "verification_line",
    "longterm_line",
    "recency_interpretation",
    "viral_load_requested",
    "date_sample_collected",
    "date_of_viral_load_result"
  )
)
