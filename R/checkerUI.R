#' User Interface
#'
#' @param id Namespace id for the Shiny module
#' @param .opts Download options
#' @param .mult Logical, whether user can select multiple download options at a time
#'
#' @return user interface
#' @export
#' @keywords internal
#'
#' @examples NULL
#'

checkerUI <- function(id, .opts, .mult) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput(
          shiny::NS(id, "upload"),
          label = "Upload line-list",
          accept = c(".csv", ".xlsx")
        ),
        pickInput(
          shiny::NS(id, "partner"),
          lab = "Implementing partner",
        ),
        pickInput(
          shiny::NS(id, "state"),
          lab = "State",
        ),
        pickInput(
          shiny::NS(id, "lga"),
          lab = "LGA",
        ),
        pickInput(
          shiny::NS(id, "facility"),
          lab = "Facility",
        ),
        shiny::dateRangeInput(
          shiny::NS(id, "period"),
          "Period",
          min = lubridate::ymd("2020-03-01"),
          format = "yyyy-mm-dd",
          language = "en"
        ),
        shiny::actionButton(
          shiny::NS(id, "update"),
          label = "Update",
          icon = shiny::icon("refresh"),
          class = "btn-success"
        ),
        htmltools::br(),
        htmltools::hr(),
        htmltools::br(),
        pickInput(
          shiny::NS(id, "indicator"),
          lab = "Download missing or wrong entries",
          opts = .opts,
          mult = .mult
        ),
        shiny::downloadButton(
          shiny::NS(id, "download"),
          label = "download",
          icon = shiny::icon("download")
        )
      ),
      shiny::mainPanel(
        DT::dataTableOutput(shiny::NS(id, "table"))
      )
    )
  )
}

