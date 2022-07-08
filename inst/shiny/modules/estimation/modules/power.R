


powerViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    shiny::uiOutput(outputId = ns("powerTableCaption")),
    shiny::tableOutput(outputId = ns("powerTable")),
    shiny::uiOutput(outputId = ns("timeAtRiskTableCaption")),
    shiny::tableOutput(outputId = ns("timeAtRiskTable"))
  )
}



powerServer <- function(id, selectedRow, inputParams, blind) {
  assertthat::assert_that(is.reactive(selectedRow))
  assertthat::assert_that(is.reactive(inputParams))
  assertthat::assert_that(!is.reactive(blind))
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$powerTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (!is.null(row)) {
          text <- "<strong>Table 1a.</strong> Number of subjects, follow-up time (in years), number of outcome
      events, and event incidence rate (IR) per 1,000 patient years (PY) in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after propensity score adjustment, as  well as the minimum detectable  relative risk (MDRR).
      Note that the IR does not account for any stratification."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        } else {
          return(NULL)
        }
      })
      
      output$powerTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          if (row$databaseId %in% metaAnalysisDbIds) {
            results <- getMainResults(connection = connection,
                                      targetIds = row$targetId,
                                      comparatorIds = row$comparatorId,
                                      outcomeIds = row$outcomeId,
                                      analysisIds = row$analysisId)
            table <- preparePowerTable(results, cohortMethodAnalysis, includeDatabaseId = TRUE)
            table$description <- NULL
            if (blind) {
              table$targetOutcomes  <- NA
              table$comparatorOutcomes   <- NA
              table$targetIr   <- NA
              table$comparatorIr   <- NA
            }
            table$databaseId[table$databaseId %in% metaAnalysisDbIds] <- "Summary"
            colnames(table) <- c("Source",
                                 "Target subjects",
                                 "Comparator subjects",
                                 "Target years",
                                 "Comparator years",
                                 "Target events",
                                 "Comparator events",
                                 "Target IR (per 1,000 PY)",
                                 "Comparator IR (per 1,000 PY)",
                                 "MDRR")
          } else {
            table <- preparePowerTable(row, cohortMethodAnalysis)
            table$description <- NULL
            table$databaseId <- NULL
            if (blind) {
              table$targetOutcomes  <- NA
              table$comparatorOutcomes   <- NA
              table$targetIr   <- NA
              table$comparatorIr   <- NA
            }
            colnames(table) <- c("Target subjects",
                                 "Comparator subjects",
                                 "Target years",
                                 "Comparator years",
                                 "Target events",
                                 "Comparator events",
                                 "Target IR (per 1,000 PY)",
                                 "Comparator IR (per 1,000 PY)",
                                 "MDRR")
          }
          return(table)
        }
      })
      
      output$timeAtRiskTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (!is.null(row)) {
          text <- "<strong>Table 1b.</strong> Time (days) at risk distribution expressed as
      minimum (min), 25th percentile (P25), median, 75th percentile (P75), and maximum (max) in the target
     (<em>%s</em>) and comparator (<em>%s</em>) cohort after propensity score adjustment."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        } else {
          return(NULL)
        }
      })
      
      output$timeAtRiskTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == inputParams()$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == inputParams()$outcome]
          if (row$databaseId %in% metaAnalysisDbIds) {
            followUpDist <- getCmFollowUpDist(cmFollowUpDist = cmFollowUpDist,
                                              connection = connection,
                                              targetId = targetId,
                                              comparatorId = comparatorId,
                                              outcomeId = outcomeId,
                                              analysisId = row$analysisId)
          } else {
            followUpDist <- getCmFollowUpDist(cmFollowUpDist = cmFollowUpDist,
                                              connection = connection,
                                              targetId = targetId,
                                              comparatorId = comparatorId,
                                              outcomeId = outcomeId,
                                              databaseId = row$databaseId,
                                              analysisId = row$analysisId)
          }
          table <- prepareFollowUpDistTable(followUpDist)
          return(table)
        }
      })
    })
}