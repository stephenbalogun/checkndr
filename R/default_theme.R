#' Default Theme
#'
#' Set default theme for the Shiny applications
#'
#' @export
#' @keywords internal
#' @return no return value
#'
#' @examples NULL
default_theme <- function() {
  bslib::bs_theme(
    bootswatch = "minty",
    bg = "#fff",   #"#002B36",
    fg =  "#000",  #"#EEE8D5",
    base_font = bslib::font_google("Fira Sans"),
    heading_font = bslib::font_google("Pacifico"),
    accent = "red",
    primary = "#971230"
  )
}
