#' Generate Line-list of Selected Indicators
#'
#' @param input Selected indicator(s)
#' @param df Line-list supplied
#'
#' @export
#' @keywords internal
#' @return dataframe
#'
#' @examples NULL
recency_line_list <- function(input, df) {
  switch(input,
         "Client state" = dplyr::filter(df, is.na(client_state)),
         "Client LGA" = dplyr::filter(df, is.na(client_lga)),
         "Sex" = dplyr::filter(df, is.na(sex) | !sex %in% c("Male", "Female", "M", "F", "male", "female", "m", "f")),
         "Age" = dplyr::filter(df, is.na(age) | age < 15),
         "Visit date" = dplyr::filter(df, is.na(visit_date)),
         "Screening result" = dplyr::filter(df, !hts_result %in% c("R", "Pos")),
         "Confirmatory test" = dplyr::filter(df, !hts_confirmatory_result %in% c("R", "Pos", "NR", "Neg", "Invalid")),
         "Tie breaker" = dplyr::filter(df, hts_confirmatory_result %in% c("NR", "Neg"), !hts_tie_breaker_result %in% c("R", "Pos")),
         "Testing point" = dplyr::filter(df, is.na(testing_point)),
         "Opt out" = dplyr::filter(df, is.na(opt_out)),
         "Recency test" = dplyr::filter(df, recency_test_name %in% c("Asante", "AS"), is.na(recency_test_date)),
         "Recency number" = dplyr::filter(df, recency_test_name %in% c("Asante", "AS"), is.na(recency_number)),
         "Control line" = dplyr::filter(df, recency_test_name %in% c("Asante", "AS"), !control_line %in% c("Yes", "No")),
         "Verification line" = dplyr::filter(df, recency_test_name %in% c("Asante", "AS"), !verification_line %in% c("Yes", "No")),
         "Longterm line" = dplyr::filter(df, recency_test_name %in% c("Asante", "AS"), !longterm_line %in% c("Yes", "No")),
         "Interpreted longterm" = dplyr::filter(
           df, control_line %in% "Yes", verification_line %in% "Yes",
           longterm_line %in% "Yes", !recency_interpretation %in% "LongTerm"
         ),
         "Interpreted recent" = dplyr::filter(
           df, control_line %in% "Yes", verification_line %in% "Yes",
           !longterm_line %in% "Yes", !recency_interpretation %in% "Recent"
         ),
         "Interpreted negative" = dplyr::filter(
           df, control_line %in% "Yes", !verification_line %in% "Yes",
           !longterm_line %in% "Yes", !recency_interpretation %in% "Negative"
         ),
         "Interpreted invalid" = dplyr::filter(df, recency_test_name %in% c("Asante", "AS") & !control_line %in% "Yes" &
                                          !recency_interpretation %in% "Invalid" | control_line %in% "Yes" &
                                          !verification_line %in% "Yes" & longterm_line %in% "Yes" &
                                          !recency_interpretation %in% "Invalid"),
         "Viral load requested" = dplyr::filter(
           df, control_line %in% "Yes", verification_line %in% "Yes",
           !longterm_line %in% "Yes", !viral_load_requested %in% "Yes"
         ),
         "Viral load sample collection date" = dplyr::filter(
           df, control_line %in% "Yes", verification_line %in% "yes",
           !longterm_line %in% "Yes", viral_load_requested %in% "Yes",
           is.na(date_sample_collected)
         ),
         "Viral load result" = dplyr::filter(
           df, control_line %in% "Yes", verification_line %in% "Yes",
           !longterm_line %in% "Yes", viral_load_requested %in% "Yes",
           visit_date < max(visit_date, na.rm = TRUE) - lubridate::days(60), is.na(date_of_viral_load_result)
         )
  )
}


utils::globalVariables(
  c("client_state", "client_lga", "sex", "age", "visit_date",
  "hts_result", "hts_confirmatory_result", "hts_tie_breaker_result",
  "testing_point", "opt_out", "recency_test_name", "recency_number",
  "recency_test_date", "control_line", "verification_line",
  "longterm_line", "recency_interpretation", "viral_load_requested",
  "date_sample_collected", "date_of_viral_load_result"
  )
)
