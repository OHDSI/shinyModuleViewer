# @file Ui.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source("modules/prediction/modules/summaryTable.R")
source("modules/prediction/modules/covariateSummary.R")
source("modules/prediction/modules/settings.R")
source("modules/prediction/modules/cutoff.R")
source("modules/prediction/modules/discrimination.R")
source("modules/prediction/modules/calibration.R")
source("modules/prediction/modules/netBenefit.R")
source("modules/prediction/modules/validation.R")
source("modules/prediction/modules/download.R")

source("modules/prediction/databaseExtras.R")
source("modules/prediction/emptyPlot.R")

predictionViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shiny::tabsetPanel(
    id = ns('allView'),
    shiny::tabPanel(
      "All Models Summary",  
      summaryViewer(ns('sumTab'))
    ),
    
    shiny::tabPanel(
      "Explore Selected Model",
      
      shiny::tabsetPanel(
        id = ns('singleView'),
        shiny::tabPanel(
          "Development Settings",
          settingsViewer(ns('settings'))
        ),
        
        shiny::tabPanel(
          "Model",
          covariateSummaryViewer(ns('covariateSummary'))
        ),
        
        shiny::tabPanel(
          "Threshold Dependant", 
          cutoffViewer(ns('cutoff'))
        ), 
        
        shiny::tabPanel(
          "Discrimination",  
          discriminationViewer(ns('discrimination'))
        ),
        
        shiny::tabPanel(
          "Calibration", 
          calibrationViewer(ns('calibration'))
        ),
        
        shiny::tabPanel(
          "Net Benefit", 
          nbViewer(ns('netBenefit'))
        ),
        
        shiny::tabPanel(
          "Validation",
          validationViewer(ns('validation'))
        ),
        
        shiny::tabPanel(
          "Developer Info",
          shinydashboard::box(status = 'info',
                              title = "Developer Info",
                              solidHeader = TRUE,
                              side = "right",
                              shiny::tableOutput(ns('researcherInfo'))
          )
        ),
        
        shiny::tabPanel(
          "Download Model",
          downloadViewer(ns('download'))
        )
        
      )
    )
    
  )
  
}

predictionServer <- function(id, 
                             resultDatabaseSettings = list(myPort = 1)
                             ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # connect
      if(F){
      if(resultDatabaseSettings$myPort != ""){
        ParallelLogger::logInfo('Port')
        ParallelLogger::logInfo(paste(resultDatabaseSettings$myPort))
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = resultDatabaseSettings$targetDialect,
                            server = resultDatabaseSettings$myServer,
                            user = resultDatabaseSettings$myUser,
                            password = resultDatabaseSettings$myPassword,
                            port = resultDatabaseSettings$myPort)
        
      } else{
        ParallelLogger::logInfo('No Port')
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = resultDatabaseSettings$targetDialect,
                            server = resultDatabaseSettings$myServer,
                            user = resultDatabaseSettings$myUser,
                            password = resultDatabaseSettings$myPassword
                            )
        
      }
      
      onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          pool::poolClose(con)
        }
      })
      }
      
      # old connection 
      connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = resultDatabaseSettings$targetDialect,
                                                                      server = resultDatabaseSettings$myServer,
                                                                      user = resultDatabaseSettings$myUser,
                                                                      password = resultDatabaseSettings$myPassword,
                                                                      port = resultDatabaseSettings$myPort, 
                                                                      pathToDriver =  '/Users/jreps/Documents/drivers'
        )
      con <- DatabaseConnector::connect(connectionDetails)
      
      
      onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          DatabaseConnector::disconnect(con)
        }
      })
      
      summaryTable <- getDevSummary(
        con = con, 
        mySchema = resultDatabaseSettings$mySchema, 
        targetDialect = resultDatabaseSettings$targetDialect,
        myTableAppend = resultDatabaseSettings$myTableAppend
      )


      # use the summary module to select a result via row selection
      resultRow <- summaryServer('sumTab', summaryTable)
      
      # change to single model explore tab when summary table row is selected
      shiny::observeEvent(resultRow(), {
        shiny::updateTabsetPanel(session, "allView", selected = "Explore Selected Model")
      })
      
      # this loads all the results
      plpResult <- shiny::reactive({
        
        if(is.null(resultRow())){
          return(NULL)
        }
        
        loadPlpFromDb(
        chosenRow = summaryTable[resultRow(),], 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con, 
        val = F, 
        targetDialect = resultDatabaseSettings$targetDialect, 
        myTableAppend = resultDatabaseSettings$myTableAppend
      )
        })
      
      
      # ===========================================
      #  Single Result Exploring Modules
      # ===========================================
      
      covariateSummaryServer('covariateSummary',
                             plpResult,
                             summaryTable, 
                             resultRow, 
                             mySchema = resultDatabaseSettings$mySchema, 
                             con,
                             inputSingleView = input$singleView,
                             myTableAppend = resultDatabaseSettings$myTableAppend, 
                             targetDialect = resultDatabaseSettings$targetDialect
                             ) 
      
      setingsServer('settings', 
                    plpResult)
      
      cutoffServer('cutoff', 
                   plpResult)
      
      discriminationServer('discrimination', 
                           plpResult)
      
      calibrationServer(
        id = 'calibration', 
        plpResult
      ) 
      
      nbServer('netBenefit', 
               plpResult) 
      
      validationServer(
        id = 'validation', 
        plpResult = plpResult,
        useDatabase = T,
        summaryTable = summaryTable,
        resultRow = resultRow,
        con = con, 
        mySchema = resultDatabaseSettings$mySchema,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
      ) 
      
      
      downloadServer('download')
      #=======================
      # get researcher info
      #=======================
      output$researcherInfo <- shiny::renderTable(plpResult()$researcherInfo)
      
      
      
    }
  )
}
