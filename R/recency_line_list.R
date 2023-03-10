#' Generate Line-list of Selected Indicators
#'
#' @param input Selected indicator(s)
#' @param df Line-list supplied
#'
#' @keywords internal
#' @return dataframe
#'
#' @examples NULL
recency_line_list <- function(df, input) {
  out <- vector("list", length(input))

  for (i in seq_along(out)) {
    out[[i]] <- switch(input[[i]],
      "Opt out" = subset(
        df,
        is.na(opt_out) & !is.na(recency_test_name)
      ),
      "Client state" = subset(
        df,
        is.na(client_state)
        ),
      "Client LGA" = subset(
        df,
        is.na(client_lga)
        ),
      "Sex" = subset(
        df,
        is.na(sex) |
          !sex %in% c("Male", "Female", "M", "F", "male", "female", "m", "f")
        ),
      "Age" = subset(
        df,
        is.na(age) |
          age < 15
        ),
      "Screening result" = subset(
        df,
        !hts_result %in% c("R", "Pos")
        ),
      "Confirmatory test" = subset(
        df,
        hts_result %in% c("R", "Pos") &
          !hts_confirmatory_result %in% c("R", "Pos", "NR", "Neg", "Invalid")
        ),
      "Tie breaker" = subset(
        df,
        hts_result %in% c("R", "Pos") &
          hts_confirmatory_result %in% c("NR", "Neg") &
          !hts_tie_breaker_result %in% c("R", "Pos")),
      "Testing point" = subset(
        df,
        is.na(testing_point)
        ),
      "Recency test name" = subset(
        df,
        !recency_test_name %in% c("Asante", "AS")
        ),
      "Recency test date" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          is.na(recency_test_date) |
          recency_test_name %in% c("Asante", "AS") &
          recency_test_date < visit_date
        ),
      "Recency number" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          is.na(recency_number)
        ),
      "Control line" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          !control_line %in% c("Yes", "No")
        ),
      "Verification line" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          !verification_line %in% c("Yes", "No")
        ),
      "Longterm line" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          !longterm_line %in% c("Yes", "No")
        ),
      "Interpreted longterm" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          control_line %in% "Yes" & verification_line %in% "Yes" & longterm_line %in% "Yes" &
          !recency_interpretation %in% "LongTerm"
      ),
      "Interpreted recent" = subset(
        df,
        control_line %in% "Yes" & verification_line %in% "Yes" & !longterm_line %in% "Yes" &
          !recency_interpretation %in% "Recent"
      ),
      "Interpreted negative" = subset(
        df,
        control_line %in% "Yes" & !verification_line %in% "Yes" & !longterm_line %in% "Yes" &
          !recency_interpretation %in% "Negative"
      ),
      "Interpreted invalid" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          !control_line %in% "Yes" & !recency_interpretation %in% "Invalid" |
          control_line %in% "Yes" & !verification_line %in% "Yes" & longterm_line %in% "Yes" & !recency_interpretation %in% "Invalid"
        ),
      "Viral load requested" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          control_line %in% "Yes" & verification_line %in% "Yes" &
          !longterm_line %in% "Yes" & !viral_load_requested %in% "Yes"
      ),
      "VL sample collection date" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          control_line %in% "Yes" & verification_line %in% "Yes" &
          !longterm_line %in% "Yes" & viral_load_requested %in% "Yes" &
          is.na(date_sample_collected)
      ),
      "Viral load result" = subset(
        df,
        recency_test_name %in% c("Asante", "AS") &
          control_line %in% "Yes" & verification_line %in% "Yes" &
          !longterm_line %in% "Yes" & viral_load_requested %in% "Yes" &
          recency_test_date < max(recency_test_date, na.rm = TRUE) - 42 & is.na(date_of_viral_load_result)
      ),
      "Partial duplicates" = janitor::get_dupes(df, sex, date_of_birth, facility, visit_date)
    )
  }
  names(out) <- input

  out
}



utils::globalVariables(
  c(
    "client_state",
    "client_lga",
    "date_of_birth",
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
