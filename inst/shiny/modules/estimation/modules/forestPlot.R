

estimationForestPlotViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    plotOutput(outputId = ns("forestPlot")),
    uiOutput(outputId = ns("forestPlotCaption")),
    div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
        downloadButton(outputId = ns("downloadForestPlotPng"),
                       label = "Download plot as PNG"),
        downloadButton(outputId = ns("downloadForestPlotPdf"),
                       label = "Download plot as PDF"))
  )
}






estimationForestPlotServer <- function(id, selectedRow, inputParams) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      forestPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          results <- getEstimationMainResults(connection = connection,
                                    targetIds = row$targetId,
                                    comparatorIds = row$comparatorId,
                                    outcomeIds = row$outcomeId,
                                    analysisIds = row$analysisId)
          plot <- plotEstimationForest(results)
          return(plot)
        }
      })
      
      output$forestPlot <- shiny::renderPlot({
        forestPlot()
      })
      
      output$forestPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 6.</strong> Forest plot showing the per-database and summary hazard ratios (and 95 percent confidence
      intervals) comparing %s to %s for the outcome of %s, using %s. Estimates are shown both before and after empirical
      calibration. The I2 is computed on the uncalibrated estimates."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator, inputParams()$outcome, row$psStrategy)))
        }
      })
      
      output$downloadForestPlotPng <- shiny::downloadHandler(filename = "ForestPlot.png",
                                                             contentType = "image/png",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file, plot = forestPlot(), width = 12, height = 9, dpi = 400)
                                                             })
      
      output$downloadForestPlotPdf <- shiny::downloadHandler(filename = "ForestPlot.pdf",
                                                             contentType = "application/pdf",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file = file, plot = forestPlot(), width = 12, height = 9)
                                                             })
      
    }
  )
}
