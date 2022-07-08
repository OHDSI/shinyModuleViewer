


propensityModelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    div(strong("Table 3."),"Fitted propensity model, listing all coviates with non-zero coefficients. Positive coefficients indicate predictive of the target exposure."),
    DT::dataTableOutput(outputId = ns("propensityModelTable"))
  )
}



propensityModelServer <- function(id, selectedRow, inputParams) {
  assertthat::assert_that(is.reactive(selectedRow))
  assertthat::assert_that(is.reactive(inputParams))
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$propensityModelTable <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$comparator]
          model <- getPropensityModel(connection = connection,
                                      targetId = targetId,
                                      comparatorId = comparatorId,
                                      databaseId = row$databaseId,
                                      analysisId = row$analysisId)
          
          table <- preparePropensityModelTable(model)
          options = list(columnDefs = list(list(className = 'dt-right',  targets = 0)),
                         pageLength = 15,
                         searching = FALSE,
                         lengthChange = TRUE,
                         ordering = TRUE,
                         paging = TRUE)
          selection = list(mode = "single", target = "row")
          table <- DT::datatable(table,
                                 options = options,
                                 selection = selection,
                                 rownames = FALSE,
                                 escape = FALSE,
                                 class = "stripe nowrap compact")
          return(table)
        }
      })
      
    }
  )
}