#' Title Select Input
#'
#' @param id input ID
#' @param lab title of the select input
#' @param opts select options
#' @param mult boolean, should multiple selections be possible?
#'
#' @export
#' @keywords internal
#' @return selected input
#'
#' @examples NULL
pickInput <- function(id, lab, opts = NULL, mult = TRUE) {
  shinyWidgets::pickerInput(
    inputId = id,
    label = lab,
    choices = opts,
    multiple = mult,
    options = list(
      `actions-box` = TRUE,
      dropAuto = TRUE,
      size = "auto",
      `live-search` = TRUE,
      `live-search-normalize` = TRUE
    )
  )
}
