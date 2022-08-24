

source("modules/cohortgenerator/R/helper.R")
cohortGeneratorViewer <- function(id) {
  
  ns <- shiny::NS(id)

  fluidPage(
    tabsetPanel(
      id = ns("cohortGeneratorTabs"),
      tabPanel(title = "Cohort Counts",
               DT::dataTableOutput(outputId = ns("cohortCounts"))
      ),
      tabPanel(title = "Cohort Generation",
               DT::dataTableOutput(outputId = ns("cohortGeneration"))
      ),
      tabPanel(title = "Cohort Inclusions",
               reactable::reactableOutput(outputId = ns("inclusionStats"))
      )
    )
  )
}




cohortGeneratorServer <- function(id, generationConnectionDetails) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = generationConnectionDetails$dbms,
                                                                      user = generationConnectionDetails$user,
                                                                      password = generationConnectionDetails$password,
                                                                      server = sprintf("%s/%s", generationConnectionDetails$server,
                                                                                       generationConnectionDetails$database))
      
      
      connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      
      output$cohortCounts <- DT::renderDataTable({
        sql <- "SELECT * FROM POC.CG_COHORT_COUNT"
        data <- DatabaseConnector::querySql(connection = connection, sql = sql)
        data
      })
      
      output$cohortGeneration <- DT::renderDataTable({
        sql <- "SELECT * FROM POC.CG_COHORT_GENERATION"
        data <- DatabaseConnector::querySql(connection = connection, sql = sql)
        data
      })
      
      output$inclusionStats <- reactable::renderReactable({
        sql <- "SELECT * FROM POC.CG_COHORT_SUMMARY_STATS"
        data <- DatabaseConnector::querySql(connection = connection, sql = sql)
        
        reactable::reactable(
          data,
          groupBy = c("DATABASE_ID", "COHORT_DEFINITION_ID"),
          striped = TRUE,
          filterable = TRUE,
          searchable = TRUE,
          bordered = TRUE
        )
        
      })
      
    }
  )
}