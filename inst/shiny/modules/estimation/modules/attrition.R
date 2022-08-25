

attritionViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    plotOutput(outputId = ns("attritionPlot"), width = 600, height = 600),
    uiOutput(outputId = ns("attritionPlotCaption")),
    div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
        downloadButton(outputId = ns("downloadAttritionPlotPng"),
                       label = "Download diagram as PNG"),
        downloadButton(outputId = ns("downloadAttritionPlotPdf"),
                       label = "Download diagram as PDF"))
  )
}


attritionServer <- function(id, selectedRow, inputParams, connection, resultsSchema) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      attritionPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          attrition <- getEstimationAttrition(connection = connection,
                                    resultsSchema = resultsSchema,
                                    targetId = inputParams()$target,
                                    comparatorId = inputParams()$comparator,
                                    outcomeId = inputParams()$outcome,
                                    databaseId = row$databaseId,
                                    analysisId = row$analysisId)
          plot <- drawEstimationAttritionDiagram(attrition)
          return(plot)
        }
      })
      
      output$attritionPlot <- shiny::renderPlot({
        return(attritionPlot())
      })
      
      output$downloadAttritionPlotPng <- shiny::downloadHandler(filename = "Attrition.png",
                                                                contentType = "image/png",
                                                                content = function(file) {
                                                                  ggplot2::ggsave(file, plot = attritionPlot(), width = 6, height = 7, dpi = 400)
                                                                })
      
      output$downloadAttritionPlotPdf <- shiny::downloadHandler(filename = "Attrition.pdf",
                                                                contentType = "application/pdf",
                                                                content = function(file) {
                                                                  ggplot2::ggsave(file = file, plot = attritionPlot(), width = 6, height = 7)
                                                                })
      
      output$attritionPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 1.</strong> Attrition diagram, showing the Number of subjects in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after various stages in the analysis."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
      }
    )
}
