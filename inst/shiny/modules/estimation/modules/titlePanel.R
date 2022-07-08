
titlePanelViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("titleText"))
}


titlePanelServer <- function(id, blind) {
  assertthat::assert_that(!is.reactive(blind))
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$titleText <- renderUI({
        titlePanel(paste("Evidence Explorer", if(blind) "***Blinded***" else ""))
      })
      
    }
  )
}