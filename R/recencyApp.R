#' Specify the Function for the Data Quality
#'
#' @param df dataframe of line list
#'
#' @keywords internal
#' @return table of data quality check outcome
#'
#' @examples NULL
table_dqa_pos <- function(df) {
  dqa_pos(df)
}


table_dqa_recency <- function(df) {
  dqa_recency(df)
}


#' Filter rows with Complete Valid Entries
#'
#' @param df dataframe of line list
#'
#' @keywords internal
#' @return table of rows with valid entries in all cells
#'
#' @examples NULL
filter_valid <- function(df) {
  valid_cases(df)
}




#' Specify the Type of Line-list to be Requested
#'
#' @param input Selected indicator(s)
#' @param df Line-list supplied
#'
#' @keywords internal
#' @return line-list of requested indicator(s)
#'
#' @examples NULL
request_line_list <- function(input, df) {
  recency_line_list(input, df)
}



#' Start the Recency Data Quality Checker App
#'
#' @param .theme customise the appearance of your Shiny app. This can be done using the `bslib::bs_theme()` function.
#'
#' @return recency data quality app
#' @export recencyApp
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   recencyApp()
#' }
#'
recencyApp <- function(.theme = NULL) {
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- shiny::fluidPage(
    shinyFeedback::useShinyFeedback(),
    theme = .theme %||% default_theme(),
    shiny::titlePanel("Recency Data Quality Checker"),
    checkerUI(
      "recency",
      .opts = recency_download_opts(),
      .mult = TRUE
    )
  )

  server <- function(input, output, session) {
    checkerServer(
      "recency",
      date_var = "visit_date",
      partner_var = "ip",
      state_var = "facility_state",
      lga_var = "facility_lga",
      facility_var = "facility",
      var_names = recency_var_names()
    )
  }

  shiny::shinyApp(ui, server)
}
