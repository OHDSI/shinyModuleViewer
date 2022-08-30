

estimationDiagnosticsSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    reactable::reactableOutput(outputId = ns("diagnosticsTable"))
  )
}


estimationDiagnosticsSummaryServer <- function(id, connection, resultsSchema) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$diagnosticsTable <- reactable::renderReactable({
        data <- getDiagnosticsData(connection, resultsSchema)
        
        reactable::reactable(data,
                             striped = TRUE,
                             filterable = TRUE,
                             searchable = TRUE,
                             bordered = TRUE
                            )
      })
      
    }
  )
}
