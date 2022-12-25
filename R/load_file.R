#' Upload Line List
#'
#' @param name name of line-list file
#' @param path location of file on the computer
#' @param var_names variable names of the line-list
#'
#' @export
#' @keywords internal
#' @return dataframe of line-list
#'
#' @examples NULL
load_file <- function(name, path, var_names) {

  ext <- tools::file_ext(name)

  dt <- switch(ext,
               csv = tidyndr::read_ndr(
                 path,
                 type = "recency"
               ),
               xlsx = readxl::read_excel(
                 path,
                 na = c("", "NA", "NULL")
               ) |> janitor::clean_names(),
               shiny::validate("Invalid file; Please upload a .csv or .xlsx file")
  )

  dt |>
    dplyr::select(tidyselect::any_of(var_names))
}
