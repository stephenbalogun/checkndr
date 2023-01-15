#' Data Quality Check Server
#'
#' @param id Namespace id for the Shiny module
#' @param date_var the variable in the data set to be used for the calendar date
#' @param partner_var the 'Implementing Partner' variable from the data set
#' @param state_var the reference variable for 'State' in the data set
#' @param lga_var  the reference variable for 'LGA' in the data set
#' @param facility_var  the reference variable for 'Facility' in the data set
#' @param var_names all the variable names to be extracted from the supplied data set
#'
#' @return server component
#' @keywords internal
#'
#' @examples NULL
checkerServer <- function(id, date_var, partner_var, state_var, lga_var, facility_var, var_names) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # load data -----------------------------------------------------------------------------------------------------------------
      data <- shiny::reactive({
        shiny::req(input$upload)

        load_file(input$upload$name, input$upload$datapath, var_names)
      })

      # update date range using data period -----------------------------------------------------------------------------------

      shiny::observeEvent(data(), {
        range <- range(data()[[date_var]], na.rm = TRUE)

        shiny::updateDateRangeInput(
          session = session, inputId = "period", min = as.Date("2020-01-01"),
          max = Sys.Date(), start = range[[1]], end = range[[2]]
        )
      })

      # Populate choices of implementing partners ------------------------------------------------------------------------------

      shiny::observeEvent(data(), {
        partners <- sort(as.character(unique(data()[[partner_var]])))

        shinyWidgets::updatePickerInput(session = session, inputId = "partner", choices = partners, clearOptions = TRUE)
      })

      partners <- shiny::reactive({
        data()[data()[[partner_var]] %in% input$partner, ]
      })

      # Populate choices of states --------------------------------------------------------------------------------------------

      shiny::observeEvent(input$partner, {
        states <- sort(as.character(unique(partners()[[state_var]])))

        lgas <- sort(as.character(unique(partners()[[lga_var]])))

        facilities <- sort(as.character(unique(partners()[[facility_var]])))

        shinyWidgets::updatePickerInput(session = session, inputId = "state", choices = states, selected = states, clearOptions = TRUE)

        shinyWidgets::updatePickerInput(session = session, inputId = "lga", choices = lgas, selected = lgas, clearOptions = TRUE)

        shinyWidgets::updatePickerInput(
          session = session, inputId = "facility", choices = facilities,
          selected = facilities, clearOptions = TRUE
        )
      })

      states <- shiny::reactive({
        partners()[partners()[[state_var]] %in% input$state, ]
      })

      # Populate choices of LGAs ------------------------------------------------------------------------------------------------

      shiny::observeEvent(input$state, {
        lgas <- sort(unique(as.character(states()[[lga_var]])))

        facilities <- sort(as.character(unique(states()[[facility_var]])))

        shinyWidgets::updatePickerInput(session = session, inputId = "lga", choices = lgas, selected = lgas)

        shinyWidgets::updatePickerInput(session = session, inputId = "facility", choices = facilities, selected = facilities)
      })

      lgas <- shiny::reactive({
        states()[states()[[lga_var]] %in% input$lga, ]
      })


      # Populate choices of facilities ---------------------------------------------------------------------------------------

      shiny::observeEvent(input$lga, {
        facilities <- sort(unique(as.character(lgas()[[facility_var]])))

        shinyWidgets::updatePickerInput(session = session, inputId = "facility", choices = facilities, selected = facilities)
      })

      facilities <- shiny::reactive({
        lgas()[lgas()[[facility_var]] %in% input$facility, ]
      })

      # display table -------------------------------------------------------------------------------------------------------------
      dt <- shiny::eventReactive(input$update, {
        data() |>
          dplyr::filter(
            ip %in% input$partner,
            facility_state %in% input$state,
            facility_lga %in% input$lga,
            facility %in% input$facility,
            dplyr::between(.data[[date_var]], input$period[[1]], input$period[[2]]),
            !is.na(opt_out)
          )
      })


      valid_entries <- shiny::reactive({
        filter_valid(dt())
      })

      percent_valid <- shiny::reactive({
        nrow(valid_entries()) / nrow(dt())
      })


      output$summarybox <- shiny::renderUI({
        shiny::fluidRow(
          summaryBox::summaryBox2(
            "persons had recency test",
            nrow(dt()),
            width = 4,
            icon = "fas fa-clipboard-list",
            style = "info"
          ),
          summaryBox::summaryBox2(
            "valid entries", nrow(valid_entries()),
            width = 4,
            icon = "fas fa-vial",
            style = "success"
          ),
          summaryBox::summaryBox2(
            "valid entries",
            scales::percent(percent_valid(), accuracy = 0.1),
            width = 4,
            icon = "fas fa-yin-yang",
            style = dplyr::case_when(
              percent_valid() < 0.95 ~ "danger",
              dplyr::between(percent_valid(), 0.95, 0.999) ~ "warning",
              percent_valid() >= 0.999 ~ "success"
            )
          )
        )
      })


      output$table <- DT::renderDataTable({
        table_dqa(dt())
      })

      # download outputs ----------------------------------------------------------------------------------------------------------
      download_data <- shiny::reactive({
        request_line_list(dt(), input$indicator)
      })

      output$download <- downloadHandler(
        filename = function() {
          paste(Sys.Date(), " ", paste(input$indicator, collapse = "_"),
            " line-listing", ".xlsx",
            sep = ""
          )
        },
        content = function(file) {
          write_results(download_data(), f_name = file)
        }
      )
    }
  )
}


utils::globalVariables(
  c(
    "ip", "facility_state", "facility_lga",
    "facility", ".data", "downloadHandler"
  )
)
