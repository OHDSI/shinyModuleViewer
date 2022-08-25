
propensityScoreDistViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    plotOutput(outputId = ns("psDistPlot")),
    div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                         that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                         two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
    div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
        downloadButton(outputId = ns("downloadPsDistPlotPng"),
                       label = "Download plot as PNG"),
        downloadButton(outputId = ns("downloadPsDistPlotPdf"),
                       label = "Download plot as PDF"))
  )
}











propensityScoreDistServer <- function(id, selectedRow, inputParams, connection, resultsSchema) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      psDistPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          if (FALSE && row$databaseId %in% metaAnalysisDbIds) {
            #TODO: update once MA implemented
            ps <- getEstimationPs(connection = connection,
                        targetIds = row$targetId,
                        comparatorIds = row$comparatorId,
                        analysisId = row$analysisId)
          } else {
            ps <- getEstimationPs(connection = connection,
                        resultsSchema = resultsSchema,
                        targetId = inputParams()$target,
                        comparatorId = inputParams()$comparator,
                        analysisId = row$analysisId,
                        databaseId = row$databaseId)
          }
          if (nrow(ps) == 0) {
            return(NULL) #TODO: handle more gracefully
          }
          plot <- plotEstimationPs(ps, inputParams()$target, inputParams()$comparator)
          return(plot)
        }
      })
      
      output$psDistPlot <- shiny::renderPlot({
        return(psDistPlot())
      })
      
      output$downloadPsDistPlotPng <- shiny::downloadHandler(filename = "Ps.png",
                                                             contentType = "image/png",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file, plot = psDistPlot(), width = 5, height = 3.5, dpi = 400)
                                                             })
      
      output$downloadPsDistPlotPdf <- shiny::downloadHandler(filename = "Ps.pdf",
                                                             contentType = "application/pdf",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file = file, plot = psDistPlot(), width = 5, height = 3.5)
                                                             })
      
    }
  )
}
