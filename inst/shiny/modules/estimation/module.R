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
                             blind = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      titlePanelServer("titlePanel", blind)
      
      connection <- NULL
      dataFolder <- NULL
      if (is.null(estimationConnectionDetails$server) ||
          (is.list(estimationConnectionDetails$server) && length(estimationConnectionDetails$server) != 0)) {
          connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = estimationConnectionDetails$dbms,
                                                                          user = estimationConnectionDetails$user,
                                                                          password = estimationConnectionDetails$password,
                                                                          server = sprintf("%s/%s", estimationConnectionDetails$server,
                                                                                           estimationConnectionDetails$database))
          
          
          connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
      } else {
        assign("dataFolder", estimationConnectionDetails$dataFolder, envir = .GlobalEnv)
        
        loadEstimationData(estimationConnectionDetails$dataFolder)
      }
      
      output$targetWidget <- shiny::renderUI({
        selectInput(inputId = session$ns("target"),
                    label = "Target",
                    choices = unique(exposureOfInterest$exposureName))
      })

      output$comparatorWidget <- shiny::renderUI({
        selectInput(inputId = session$ns("comparator"),
                  label = "Comparator",
                  choices = unique(exposureOfInterest$exposureName),
                  selected = unique(exposureOfInterest$exposureName)[2])
      })
      
      output$outcomeWidget <- shiny::renderUI({
        selectInput(inputId = session$ns("outcome"),
                  label = "Outcome",
                  choices = unique(outcomeOfInterest$outcomeName))
      })
      output$databaseWidget<- shiny::renderUI({
        checkboxGroupInput(inputId = session$ns("database"),
                         label = "Data source",
                         choices = database$databaseId,
                         selected = database$databaseId)
      })
      output$analysisWidget <- shiny::renderUI({
        checkboxGroupInput(inputId = session$ns("analysis"),
                         label = "Analysis",
                         choices = cohortMethodAnalysis$description,
                         selected = cohortMethodAnalysis$description)
      })
      
      if (blind) {
        shiny::hideTab(inputId = "detailsTabsetPanel", target = "Kaplan-Meier",
                       session = session)
      }
      if (!exists("cmInteractionResult")) {
        shiny::hideTab(inputId = "detailsTabsetPanel", target = "Subgroups",
                       session = session)
      }
      
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
        targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
        comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
        outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
        analysisIds <- cohortMethodAnalysis$analysisId[cohortMethodAnalysis$description %in% input$analysis]
        databaseIds <- input$database
        if (length(analysisIds) == 0) {
          analysisIds <- -1
        }
        if (length(databaseIds) == 0) {
          databaseIds <- "none"
        }
        results <- getMainResults(connection = connection,
                                  targetIds = targetId,
                                  comparatorIds = comparatorId,
                                  outcomeIds = outcomeId,
                                  databaseIds = databaseIds,
                                  analysisIds = analysisIds)
        results <- results[order(results$analysisId), ]
        if (blind) {
          results$rr <- rep(NA, nrow(results))
          results$ci95Ub <- rep(NA, nrow(results))
          results$ci95Lb <- rep(NA, nrow(results))
          results$logRr <- rep(NA, nrow(results))
          results$seLogRr <- rep(NA, nrow(results))
          results$p <- rep(NA, nrow(results))
          results$calibratedRr <- rep(NA, nrow(results))
          results$calibratedCi95Ub <- rep(NA, nrow(results))
          results$calibratedCi95Lb <- rep(NA, nrow(results))
          results$calibratedLogRr <- rep(NA, nrow(results))
          results$calibratedSeLogRr <- rep(NA, nrow(results))
          results$calibratedP <- rep(NA, nrow(results))
        }
        return(results)
      })
      
      selectedRow <- resultsTableServer("resultsTable", resultSubset)

      balance <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
          balance <- getCovariateBalance(connection = connection,
                                         targetId = targetId,
                                         comparatorId = comparatorId,
                                         databaseId = row$databaseId,
                                         analysisId = row$analysisId,
                                         outcomeId = outcomeId)
          return(balance)
        }
      })
      
      output$rowIsSelected <- shiny::reactive({
        return(!is.null(selectedRow()))
      })
      outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)

      output$isMetaAnalysis <- shiny::reactive({
        row <- selectedRow()
        isMetaAnalysis <- !is.null(row) && (row$databaseId %in% metaAnalysisDbIds)
        if (isMetaAnalysis) {
          hideTab("detailsTabsetPanel", "Attrition", session = session)
          hideTab("detailsTabsetPanel", "Population characteristics", session = session)
          hideTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
          hideTab("detailsTabsetPanel", "Propensity model", session = session)
          showTab("detailsTabsetPanel", "Forest plot", session = session)
        } else {
          showTab("detailsTabsetPanel", "Attrition", session = session)
          showTab("detailsTabsetPanel", "Population characteristics", session = session)
          if (!blind) {
            showTab("detailsTabsetPanel", "Kaplan-Meier", session = session)
          }
          showTab("detailsTabsetPanel", "Propensity model", session = session)
          hideTab("detailsTabsetPanel", "Forest plot", session = session)
        }
        return(isMetaAnalysis)
      })
      shiny::outputOptions(output, "isMetaAnalysis", suspendWhenHidden = FALSE)
      

      powerServer("power", selectedRow, inputParams, blind)
      
      attritionServer("attrition", selectedRow, inputParams)
      
      populationCharacteristicsServer("popCharacteristics", selectedRow, inputParams, balance)
      
      propensityModelServer("propensityModel", selectedRow, inputParams)
      
      propensityScoreDistServer("propensityScoreDist", selectedRow, inputParams)
      
      covariateBalanceServer("covariateBalance", selectedRow, balance)
      
      systematicErrorServer("systematicError", selectedRow, inputParams)
      
      kaplanMeierServer("kaplanMeier", selectedRow, inputParams)
      
      forestPlotServer("forestPlot", selectedRow, inputParams)
      
      subgroupsServer("subgroups", selectedRow, inputParams, blind)
   
    }
  )
}

