
mainColumns <- c("description",
                 "databaseId",
                 "rr",
                 "ci95Lb",
                 "ci95Ub",
                 "p",
                 "calibratedRr",
                 "calibratedCi95Lb",
                 "calibratedCi95Ub",
                 "calibratedP")

mainColumnNames <- c("<span title=\"Analysis\">Analysis</span>",
                     "<span title=\"Data source\">Data source</span>",
                     "<span title=\"Hazard ratio (uncalibrated)\">HR</span>",
                     "<span title=\"Lower bound of the 95 percent confidence interval (uncalibrated)\">LB</span>",
                     "<span title=\"Upper bound of the 95 percent confidence interval (uncalibrated)\">UB</span>",
                     "<span title=\"Two-sided p-value (uncalibrated)\">P</span>",
                     "<span title=\"Hazard ratio (calibrated)\">Cal.HR</span>",
                     "<span title=\"Lower bound of the 95 percent confidence interval (calibrated)\">Cal.LB</span>",
                     "<span title=\"Upper bound of the 95 percent confidence interval (calibrated)\">Cal.UB</span>",
                     "<span title=\"Two-sided p-value (calibrated)\">Cal.P</span>")


resultsTableViewer <- function(id) {
  ns <- shiny::NS(id)
  
  DT::dataTableOutput(outputId = ns("mainTable"))
}




resultsTableServer <- function(id, resultSubset) {
  assertthat::assert_that(is.reactive(resultSubset))
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      selectedRow <- shiny::reactive({
        idx <- input$mainTable_rows_selected
        if (is.null(idx)) {
          return(NULL)
        } else {
          subset <- resultSubset()
          if (nrow(subset) == 0) {
            return(NULL)
          }
          row <- subset[idx, ]
          row$psStrategy <- gsub("^PS ", "", gsub(", .*$", "", cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId == row$analysisId]))
          return(row)
        }
      })
      
      output$mainTable <- DT::renderDataTable({
        table <- resultSubset()
        if (is.null(table) || nrow(table) == 0) {
          return(NULL)
        }
        table$description <- cohortMethodAnalysis$description[match(table$analysisId, cohortMethodAnalysis$analysisId)]
        table <- table[, mainColumns]
        table$rr <- prettyHr(table$rr)
        table$ci95Lb <- prettyHr(table$ci95Lb)
        table$ci95Ub <- prettyHr(table$ci95Ub)
        table$p <- prettyHr(table$p)
        table$calibratedRr <- prettyHr(table$calibratedRr)
        table$calibratedCi95Lb <- prettyHr(table$calibratedCi95Lb)
        table$calibratedCi95Ub <- prettyHr(table$calibratedCi95Ub)
        table$calibratedP <- prettyHr(table$calibratedP)
        colnames(table) <- mainColumnNames
        options = list(pageLength = 15,
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
        table
      })
      
      selectedRow
    })
}