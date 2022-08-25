
subgroupsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    uiOutput(outputId = ns("subgroupTableCaption")),
    DT::dataTableOutput(outputId = ns("subgroupTable"))
  )
}

subgroupsServer <- function(id, selectedRow, inputParams) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      interactionEffects <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == inputParams()$outcome]
          subgroupResults <- getEstimationSubgroupResults(connection = connection,
                                                targetIds = targetId,
                                                comparatorIds = comparatorId,
                                                outcomeIds = outcomeId,
                                                databaseIds = row$databaseId,
                                                analysisIds = row$analysisId)
          if (nrow(subgroupResults) == 0) {
            return(NULL)
          } else {
            if (!row$unblind) {
              subgroupResults$rrr <- rep(NA, nrow(subgroupResults))
              subgroupResults$ci95Lb <- rep(NA, nrow(subgroupResults))
              subgroupResults$ci95Ub <- rep(NA, nrow(subgroupResults))
              subgroupResults$logRrr <- rep(NA, nrow(subgroupResults))
              subgroupResults$seLogRrr <- rep(NA, nrow(subgroupResults))
              subgroupResults$p <- rep(NA, nrow(subgroupResults))
              subgroupResults$calibratedP <- rep(NA, nrow(subgroupResults))
            }
            return(subgroupResults)
          }
        }
      })
      
      output$subgroupTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Table 4.</strong> Subgroup interactions. For each subgroup, the number of subject within the subroup
      in the target (<em>%s</em>) and comparator (<em>%s</em>) cohorts are provided, as well as the hazard ratio ratio (HRR)
      with 95 percent confidence interval and p-value (uncalibrated and calibrated) for interaction of the main effect with
      the subgroup."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
      output$subgroupTable <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          subgroupResults <- interactionEffects()
          if (is.null(subgroupResults)) {
            return(NULL)
          }
          subgroupTable <- prepareEstimationSubgroupTable(subgroupResults, output = "html")
          colnames(subgroupTable) <- c("Subgroup",
                                       "Target subjects",
                                       "Comparator subjects",
                                       "HRR",
                                       "P",
                                       "Cal.P")
          options <- list(searching = FALSE,
                          ordering = FALSE,
                          paging = FALSE,
                          bInfo = FALSE)
          subgroupTable <- DT::datatable(subgroupTable,
                                         options = options,
                                         rownames = FALSE,
                                         escape = FALSE,
                                         class = "stripe nowrap compact")
          return(subgroupTable)
        }
      })
    }
  )
}
