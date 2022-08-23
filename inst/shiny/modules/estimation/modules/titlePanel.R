
titlePanelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("titleText"))
}


titlePanelServer <- function(id) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$titleText <- renderUI({
        titlePanel("Evidence Explorer")
      })
      
    }
  )
}