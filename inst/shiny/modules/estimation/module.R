# @file module.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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

source("modules/estimation/R/DataPulls.R")
source("modules/estimation/R/PlotsAndTables.R")
source("modules/estimation/R/Utility.R")

source("modules/estimation/modules/titlePanel.R")
source("modules/estimation/modules/resultsTable.R")
source("modules/estimation/modules/attrition.R")
source("modules/estimation/modules/power.R")
source("modules/estimation/modules/populationCharacteristics.R")
source("modules/estimation/modules/propensityModel.R")
source("modules/estimation/modules/propensityScoreDistribution.R")
source("modules/estimation/modules/covariateBalance.R")
source("modules/estimation/modules/systematicError.R")
source("modules/estimation/modules/kaplanMeier.R")
source("modules/estimation/modules/forestPlot.R")
source("modules/estimation/modules/subgroups.R")



estimationViewer <- function(id) {
  ns <- shiny::NS(id)
  
  fluidPage(style = "width:1500px;",
            titlePanelViewer(ns("titlePanel")),
            tags$head(tags$style(type = "text/css", "
             #loadmessage {
                                 position: fixed;
                                 top: 0px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 5px 0px 5px 0px;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 100%;
                                 color: #000000;
                                 background-color: #ADD8E6;
                                 z-index: 105;
                                 }
                                 ")),
            conditionalPanel(id = ns("loadmessage"),
                             condition = "$('html').hasClass('shiny-busy')",
                             tags$div("Processing...")),
            fluidRow(
              column(width = 3,
                     uiOutput(outputId = ns("targetWidget")),
                     uiOutput(outputId = ns("comparatorWidget")),
                     uiOutput(outputId = ns("outcomeWidget")),
                     uiOutput(outputId = ns("databaseWidget")),
                     uiOutput(outputId = ns("analysisWidget"))
              ),
              column(width = 9,
                     resultsTableViewer(ns("resultsTable")),
                     conditionalPanel("output.rowIsSelected == true", ns = ns,
                                      tabsetPanel(id = ns("detailsTabsetPanel"),
                                                  tabPanel(title = "Power",
                                                           powerViewer(ns("power"))
                                                  ),
                                                  tabPanel(title = "Attrition",
                                                           attritionViewer(ns("attrition"))
                                                           ),
                                                  tabPanel(title = "Population characteristics",
                                                           populationCharacteristicsViewer(ns("popCharacteristics"))
                                                           ),
                                                  tabPanel(title = "Propensity model",
                                                           propensityModelViewer(ns("propensityModel"))
                                                           ),
                                                  tabPanel(title = "Propensity scores",
                                                           propensityScoreDistViewer(ns("propensityScoreDist"))
                                                           ),
                                                  tabPanel(title = "Covariate balance",
                                                           covariateBalanceViewer(ns("covariateBalance"))
                                                           ),
                                                  tabPanel(title = "Systematic error",
                                                           systematicErrorViewer(ns("systematicError"))
                                                           ),
                                                  tabPanel(title = "Forest plot",
                                                           forestPlotViewer(ns("forestPlot"))
                                                           ),
                                                  tabPanel(title = "Kaplan-Meier",
                                                           kaplanMeierViewer(ns("kaplanMeier"))
                                                           ),
                                                  tabPanel(title = "Subgroups",
                                                           subgroupsViewer(ns("subgroups"))
                                                           )

                                      ) # end tabsetPanel
                     ) # end conditionalPanel
              )
              
            )
  )
  
}



estimationServer <- function(id,
                             estimationConnectionDetails,
                             resultsSchema = "poc") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      titlePanelServer("titlePanel")
      
      connection <- NULL
      dataFolder <- NULL
      if (is.null(estimationConnectionDetails$server) ||
          (is.list(estimationConnectionDetails$server) && length(estimationConnectionDetails$server) == 0)) {
          assign("dataFolder", estimationConnectionDetails$dataFolder, envir = .GlobalEnv)
          
          loadEstimationData(estimationConnectionDetails$dataFolder)
      } else {
        connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = estimationConnectionDetails$dbms,
                                                                        user = estimationConnectionDetails$user,
                                                                        password = estimationConnectionDetails$password,
                                                                        server = sprintf("%s/%s", estimationConnectionDetails$server,
                                                                                         estimationConnectionDetails$database))
        
        
        connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      }
      
      
      shiny::onStop(function() {
        if (DBI::dbIsValid(con)) {
          DatabaseConnector::disconnect(con)
        }
      })
      
      output$targetWidget <- shiny::renderUI({
        targets <- getTargetChoices(connection, resultsSchema)
        shiny::selectInput(inputId = session$ns("target"),
                    label = "Target",
                    choices = getSelectNamedChoices(targets$targetId,
                                                    targets$cohortName))
      })

      output$comparatorWidget <- shiny::renderUI({
        comparators <- getComparatorChoices(connection, resultsSchema)
        shiny::selectInput(inputId = session$ns("comparator"),
                  label = "Comparator",
                  choices = getSelectNamedChoices(comparators$comparatorId,
                                                   comparators$cohortName))
      })
      
      output$outcomeWidget <- shiny::renderUI({
        outcomes <- getOutcomeChoices(connection, resultsSchema)
        shiny::selectInput(inputId = session$ns("outcome"),
                  label = "Outcome",
                  choices = getSelectNamedChoices(outcomes$outcomeId,
                                                  outcomes$cohortName))
      })
      output$databaseWidget<- shiny::renderUI({
        databases <- getDatabaseChoices(connection, resultsSchema)
        shiny::checkboxGroupInput(inputId = session$ns("database"),
                         label = "Data source",
                         choices =  getSelectNamedChoices(databases$databaseId,
                                                          databases$cdmSourceAbbreviation),
                         selected = unique(databases$databaseId))
      })
      output$analysisWidget <- shiny::renderUI({
        analyses <- getCmAnalysisOptions(connection, resultsSchema)
        shiny::checkboxGroupInput(inputId = session$ns("analysis"),
                         label = "Analysis",
                         choices =  getSelectNamedChoices(analyses$analysisId,
                                                          analyses$description),
                         selected = unique(analyses$analysisId))
      })
      
      
      inputParams <- reactive({
        t <- list()
        t$target <- input$target
        t$comparator <- input$comparator
        t$outcome <- input$outcome
        t$analysis <- input$analysis
        t$database <- input$database
        return(t)
      })

      resultSubset <- reactive({
        
        results <- getMainResults(connection = connection,
                                  resultsSchema = resultsSchema,
                                  targetIds = filterEmptyNullValues(input$target),
                                  comparatorIds = filterEmptyNullValues(input$comparator),
                                  outcomeIds = filterEmptyNullValues(input$outcome),
                                  databaseIds = filterEmptyNullValues(input$database),
                                  analysisIds = filterEmptyNullValues(input$analysis))
        results <- results[order(results$analysisId), ]
        
        
        results[which(results$unblind == 0), getColumnsToBlind(results)] <- NA
        
        return(results)
      })
      

      selectedRow <- resultsTableServer("resultsTable", resultSubset)

      output$rowIsSelected <- shiny::reactive({
        return(!is.null(selectedRow()))
      })
      
      
      if (!exists("cmInteractionResult")) {
        #TODO: update for testing once subgroup analysis completed
        shiny::hideTab(inputId = "detailsTabsetPanel", target = "Subgroups",
                       session = session)
      }
      
      outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)

      output$isMetaAnalysis <- shiny::reactive({
        #TODO: update once MA implemented
        row <- selectedRow()
        isMetaAnalysis <- FALSE # !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        if (!is.null(row)) {
          if (isMetaAnalysis) {
            hideTab("detailsTabsetPanel", "Attrition", session = session)
            hideTab("detailsTabsetPanel", "Population characteristics", session = session)
            hideTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            hideTab("detailsTabsetPanel", "Propensity model", session = session)
            showTab("detailsTabsetPanel", "Forest plot", session = session)
          } else {
            showTab("detailsTabsetPanel", "Attrition", session = session)
            showTab("detailsTabsetPanel", "Population characteristics", session = session)
            if (row$unblind) {
              showTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            } else{
              shiny::hideTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
            }
            showTab("detailsTabsetPanel", "Propensity model", session = session)
            hideTab("detailsTabsetPanel", "Forest plot", session = session)
          }
        }
        return(isMetaAnalysis)
      })
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)


      powerServer("power", selectedRow, inputParams, connection, resultsSchema)

      attritionServer("attrition", selectedRow, inputParams, connection, resultsSchema)

      populationCharacteristicsServer("popCharacteristics", selectedRow, inputParams, connection, resultsSchema)

      propensityModelServer("propensityModel", selectedRow, inputParams, connection, resultsSchema)

      propensityScoreDistServer("propensityScoreDist", selectedRow, inputParams, connection, resultsSchema)

      covariateBalanceServer("covariateBalance", selectedRow, inputParams, connection, resultsSchema)

      systematicErrorServer("systematicError", selectedRow, inputParams, connection, resultsSchema)

      kaplanMeierServer("kaplanMeier", selectedRow, inputParams, connection, resultsSchema)

      #TODO: complete once MA implemented
      # forestPlotServer("forestPlot", selectedRow, inputParams)

      #TODO: revisit once subgroup example conducted
      subgroupsServer("subgroups", selectedRow, inputParams)
   
    }
  )
}

