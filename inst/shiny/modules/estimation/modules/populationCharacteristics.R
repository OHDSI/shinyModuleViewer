
populationCharacteristicsViewer <- function(id) {
  
  ns <- shiny::NS(id)
  shiny::div(
    uiOutput(outputId = ns("table1Caption")),
    DT::dataTableOutput(outputId = ns("table1Table"))
  )
}


populationCharacteristicsServer <- function(id, selectedRow, inputParams, connection, resultsSchema) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      output$table1Caption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Table 2.</strong> Select characteristics before and after propensity score adjustment, showing the (weighted)
      percentage of subjects  with the characteristics in the target (<em>%s</em>) and comparator (<em>%s</em>) group, as
      well as the standardized difference of the means."
          return(shiny::HTML(sprintf(text, inputParams()$target, inputParams()$comparator)))
        }
      })
      
      output$table1Table <- DT::renderDataTable({
        return(NULL) #TODO: waiting fix for cm_analysis_id
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          balance <- getCovariateBalance(connection = connection,
                                         resultsSchema = resultsSchema,
                                         targetId = inputParams()$target,
                                         comparatorId = inputParams()$comparator,
                                         outcomeId = inputParams()$outcome,
                                         databaseId = row$databaseId,
                                         analysisId = row$analysisId)
          if (nrow(balance) == 0) {
            return(NULL)
          }
          table1 <- prepareTable1(balance = balance,
                                  beforeLabel = paste("Before PS adjustment"),
                                  afterLabel = paste("After PS adjustment"))
          
          container <- htmltools::withTags(table(
            class = 'display',
            thead(
              tr(
                th(rowspan = 3, "Characteristic"),
                th(colspan = 3, class = "dt-center", paste("Before PS adjustment")),
                th(colspan = 3, class = "dt-center", paste("After PS adjustment"))
              ),
              tr(
                lapply(table1[1, 2:ncol(table1)], th)
              ),
              tr(
                lapply(table1[2, 2:ncol(table1)], th)
              )
            )
          ))
          options <- list(columnDefs = list(list(className = 'dt-right',  targets = 1:6)),
                          searching = FALSE,
                          ordering = FALSE,
                          paging = FALSE,
                          bInfo = FALSE)
          table1 <- DT::datatable(table1[3:nrow(table1), ],
                                  options = options,
                                  rownames = FALSE,
                                  escape = FALSE,
                                  container = container,
                                  class = "stripe nowrap compact")
          return(table1)
        }
      })
    }
  )
}
      