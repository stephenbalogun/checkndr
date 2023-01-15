#' Write Multiple Excel Worksheets
#'
#' @param list_data datasets to be written into worksheets
#' @param f_name name of Excel workbook
#' @param ... other arguments supplied to `openxlsx::writeData()`
#'
#' @keywords internal
#' @return Excel workbook
#'
#' @examples NULL
write_results <- function(list_data, f_name, ...) {
  nms <- names(list_data)

  wb_des <- openxlsx::createWorkbook()

  for (i in seq_along(nms)) {
    openxlsx::addWorksheet(wb_des, nms[[i]])

    openxlsx::writeData(wb_des, sheet = nms[[i]], x = list_data[[i]], ...)

    openxlsx::saveWorkbook(
      wb_des,
      f_name,
      overwrite = TRUE
    )
  }
}
