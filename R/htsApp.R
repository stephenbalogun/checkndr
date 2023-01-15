
#' Start the HTS Data Quality Checker App
#'
#' @param .theme customise the appearance of your Shiny app. This can be done using the `bslib::bs_theme()` function.
#'
#' @return HTS data quality app
#' @export htsApp
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   htsApp()
#' }
#'
htsApp <- function(.theme = NULL) {
  options(shiny.maxRequestSize = 500 * 1024^2)

  ui <- shiny::fluidPage(
    theme = .theme %||% default_theme(),
    shiny::titlePanel("HTS Data Quality Checker"),
    checkerUI(
      "hts",
      .opts = hts_download_opts(),
      .mult = TRUE
    )
  )

  server <- function(input, output, session) {
    # checkerServer(
    #   "hts",
    #   date_var = "visit_date",
    #   partner_var = "ip",
    #   state_var = "facility_state",
    #   lga_var = "facility_lga",
    #   facility_var = "facility",
    #   var_names = hts_var_names()
    # )
  }

  shiny::shinyApp(ui, server)
}
