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

source("modules/estimation/R/DataPulls.R")
source("modules/estimation/R/PlotsAndTables.R")
#source("modules/estimation/global.R")



estimationViewerTest <- function(id) {
  tagList(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id, "bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}

estimationViewer <- function(id) {
  blind <- FALSE
  ns <- shiny::NS(id)
  
  fluidPage(style = "width:1500px;",
            titlePanel(paste("Evidence Explorer", if(blind) "***Blinded***" else "")),
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
                     # selectInput(inputId = ns("target"),
                     #             label = "Target",
                     #             choices = unique(exposureOfInterest$exposureName)),
                     # selectInput(inputId = ns("comparator"),
                     #             label = "Comparator",
                     #             choices = unique(exposureOfInterest$exposureName),
                     #             selected = unique(exposureOfInterest$exposureName)[2]),
                     # selectInput(inputId = ns("outcome"),
                     #             label = "Outcome",
                     #             choices = unique(outcomeOfInterest$outcomeName)),
                     # checkboxGroupInput(inputId = ns("database"),
                     #                    label = "Data source",
                     #                    choices = database$databaseId,
                     #                    selected = database$databaseId),
                     # checkboxGroupInput(inputId = ns("analysis"),
                     #                    label = "Analysis",
                     #                    choices = cohortMethodAnalysis$description,
                     #                    selected = cohortMethodAnalysis$description)
              ),
              column(width = 9,
                     DT::dataTableOutput(outputId = ns("mainTable")),
                     # conditionalPanel("output.rowIsSelected == true",
                                      tabsetPanel(id = ns("detailsTabsetPanel"),
                                                  tabPanel(title = "Power",
                                                           uiOutput(outputId = ns("powerTableCaption")),
                                                           tableOutput(outputId = ns("powerTable")),
                                                           # conditionalPanel("output.isMetaAnalysis == false",
                                                           uiOutput(outputId = ns("timeAtRiskTableCaption")),
                                                           tableOutput(outputId = ns("timeAtRiskTable"))
                                                           # )
                                                  ),
                                                  tabPanel(title = "Attrition",
                                                           plotOutput(outputId = ns("attritionPlot"), width = 600, height = 600),
                                                           uiOutput(outputId = ns("attritionPlotCaption")),
                                                           div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                               downloadButton(outputId = ns("downloadAttritionPlotPng"),
                                                                              label = "Download diagram as PNG"),
                                                               downloadButton(outputId = ns("downloadAttritionPlotPdf"),
                                                                              label = "Download diagram as PDF"))
                                                           ),
                                                  tabPanel(title = "Population characteristics",
                                                           uiOutput(outputId = ns("table1Caption")),
                                                           dataTableOutput(outputId = ns("table1Table"))
                                                           ),
                                                  tabPanel(title = "Propensity model",
                                                           div(strong("Table 3."),"Fitted propensity model, listing all coviates with non-zero coefficients. Positive coefficients indicate predictive of the target exposure."),
                                                           dataTableOutput(outputId = ns("propensityModelTable"))
                                                           ),
                                                  tabPanel(title = "Propensity scores",
                                                           plotOutput(outputId = ns("psDistPlot")),
                                                           div(strong("Figure 2."),"Preference score distribution. The preference score is a transformation of the propensity score
                                                                                                         that adjusts for differences in the sizes of the two treatment groups. A higher overlap indicates subjects in the
                                                                                                         two groups were more similar in terms of their predicted probability of receiving one treatment over the other."),
                                                           div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                               downloadButton(outputId = ns("downloadPsDistPlotPng"),
                                                                              label = "Download plot as PNG"),
                                                               downloadButton(outputId = ns("downloadPsDistPlotPdf"),
                                                                              label = "Download plot as PDF"))
                                                           ),
                                                  tabPanel(title = "Covariate balance",
                                                           conditionalPanel(condition = "output.isMetaAnalysis == false",
                                                                            uiOutput(outputId = ns("hoverInfoBalanceScatter")),
                                                                            plotOutput(outputId = ns("balancePlot"),
                                                                                       hover = hoverOpts(id = ns("plotHoverBalanceScatter"), delay = 100, delayType = "debounce")),
                                                                            uiOutput(outputId = ns("balancePlotCaption")),
                                                                            div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                                                downloadButton(outputId = ns("downloadBalancePlotPng"),
                                                                                               label = "Download plot as PNG"),
                                                                                downloadButton(outputId = ns("downloadBalancePlotPdf"),
                                                                                               label = "Download plot as PDF")
                                                                            )),
                                                           conditionalPanel(condition = "output.isMetaAnalysis == true",
                                                                            plotOutput(outputId = ns("balanceSummaryPlot")),
                                                                            uiOutput(outputId = ns("balanceSummaryPlotCaption")),
                                                                            div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                                                downloadButton(outputId = ns("downloadBalanceSummaryPlotPng"),
                                                                                               label = "Download plot as PNG"),
                                                                                downloadButton(outputId = ns("downloadBalanceSummaryPlotPdf"),
                                                                                               label = "Download plot as PDF")
                                                                            ))
                                                  ),
                                                  tabPanel(title = "Systematic error",
                                                           plotOutput(outputId = ns("systematicErrorPlot")),
                                                           div(strong("Figure 4."),"Systematic error. Effect size estimates for the negative controls (true hazard ratio = 1)
                                                                                    and positive controls (true hazard ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times."),
                                                           div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                               downloadButton(outputId = ns("downloadSystematicErrorPlotPng"),
                                                                              label = "Download plot as PNG"),
                                                               downloadButton(outputId = ns("downloadSystematicErrorPlotPdf"),
                                                                              label = "Download plot as PDF")
                                                           ),
                                                           conditionalPanel(condition = "output.isMetaAnalysis == true",
                                                                            plotOutput(outputId = ns("systematicErrorSummaryPlot")),
                                                                            div(strong("Figure 8."),"Fitted null distributions per data source."),
                                                                            div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                                                downloadButton(outputId = ns("downloadSystematicErrorSummaryPlotPng"),
                                                                                               label = "Download plot as PNG"),
                                                                                downloadButton(outputId = ns("downloadSystematicErrorSummaryPlotPdf"),
                                                                                               label = "Download plot as PDF")))
                                                           ),
                                                  tabPanel(title = "Forest plot",
                                                           plotOutput(outputId = ns("forestPlot")),
                                                           uiOutput(outputId = ns("forestPlotCaption")),
                                                           div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                               downloadButton(outputId = ns("downloadForestPlotPng"),
                                                                              label = "Download plot as PNG"),
                                                               downloadButton(outputId = ns("downloadForestPlotPdf"),
                                                                              label = "Download plot as PDF"))
                                                           ),
                                                  tabPanel(title = "Kaplan-Meier",
                                                           plotOutput(outputId = ns("kaplanMeierPlot"), height = 550),
                                                           uiOutput(outputId = ns("kaplanMeierPlotPlotCaption")),
                                                           div(style = "display: inline-block;vertical-align: top;margin-bottom: 10px;",
                                                               downloadButton(outputId = ns("downloadKaplanMeierPlotPng"),
                                                                              label = "Download plot as PNG"),
                                                               downloadButton(outputId = ns("downloadKaplanMeierPlotPdf"),
                                                                              label = "Download plot as PDF"))
                                                           ),
                                                  tabPanel(title = "Subgroups",
                                                           uiOutput(outputId = ns("subgroupTableCaption")),
                                                           dataTableOutput(outputId = ns("subgroupTable")))

                                      )
                     # )
              )
              
            )
  )
  
}


estimationServerTest <- function(id,
                             dataFolder,
                             blind = TRUE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      data <- reactive(mtcars[[input$var]])
      output$hist <- renderPlot({
        hist(data(), breaks = input$bins, main = input$var)
      }, res = 96)
    }
  )
}
estimationServer <- function(id,
                             dataFolder,
                             resultDatabaseSettings = list(),
                             blind = FALSE) {
  
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      assign("dataFolder", dataFolder, envir = .GlobalEnv)
      
      connection <- NULL # add creation
      
      print("pre-load data")
      loadData(dataFolder)
      
      
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
      
      output$targetWidget <- shiny::renderUI({
        print(exposureOfInterest$exposureName)
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
        shiny::hideTab(inputId = "detailsTabsetPanel", target = "Kaplan-Meier")
      }
      if (!exists("cmInteractionResult")) {
        shiny::hideTab(inputId = "detailsTabsetPanel", target = "Subgroups")
      }

      # shiny::observe({
      #   print(input$target)
      #   targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
      #   comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
      #   tcoSubset <- tcos[tcos$targetId == targetId & tcos$comparatorId == comparatorId, ]
      #   outcomes <- outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId %in% tcoSubset$outcomeId]
      #   shiny::updateSelectInput(session = session,
      #                            inputId = "outcome",
      #                            choices = unique(outcomes))
      # })
     #  
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

      output$rowIsSelected <- shiny::reactive({
        return(!is.null(selectedRow()))
      })
      outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)

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
        return(table)
      })

      output$powerTableCaption <- shiny::renderUI({
        row <- selectedRow()
        if (!is.null(row)) {
          text <- "<strong>Table 1a.</strong> Number of subjects, follow-up time (in years), number of outcome
      events, and event incidence rate (IR) per 1,000 patient years (PY) in the target (<em>%s</em>) and
      comparator (<em>%s</em>) group after propensity score adjustment, as  well as the minimum detectable  relative risk (MDRR).
      Note that the IR does not account for any stratification."
          return(shiny::HTML(sprintf(text, input$target, input$comparator)))
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
          return(shiny::HTML(sprintf(text, input$target, input$comparator)))
        } else {
          return(NULL)
        }
      })

      output$timeAtRiskTable <- shiny::renderTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
          if (row$databaseId %in% metaAnalysisDbIds) {
            followUpDist <- getCmFollowUpDist(connection = connection,
                                              targetId = targetId,
                                              comparatorId = comparatorId,
                                              outcomeId = outcomeId,
                                              analysisId = row$analysisId)
          } else {
            followUpDist <- getCmFollowUpDist(connection = connection,
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

      attritionPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
          attrition <- getAttrition(connection = connection,
                                    targetId = targetId,
                                    comparatorId = comparatorId,
                                    outcomeId = outcomeId,
                                    databaseId = row$databaseId,
                                    analysisId = row$analysisId)
          plot <- drawAttritionDiagram(attrition)
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
          return(shiny::HTML(sprintf(text, input$target, input$comparator)))
        }
      })

      output$table1Caption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Table 2.</strong> Select characteristics before and after propensity score adjustment, showing the (weighted)
      percentage of subjects  with the characteristics in the target (<em>%s</em>) and comparator (<em>%s</em>) group, as
      well as the standardized difference of the means."
          return(shiny::HTML(sprintf(text, input$target, input$comparator)))
        }
      })

      output$table1Table <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          bal <- balance()
          if (nrow(bal) == 0) {
            return(NULL)
          }
          table1 <- prepareTable1(balance = bal,
                                  beforeLabel = paste("Before PS adjustment"),
                                  afterLabel = paste("After PS adjustment"))

          container <- htmltools::withTags(table(
            class = 'display',
            shiny::thead(
              shiny::tr(
                shiny::th(rowspan = 3, "Characteristic"),
                shiny::th(colspan = 3, class = "dt-center", paste("Before PS adjustment")),
                shiny::th(colspan = 3, class = "dt-center", paste("After PS adjustment"))
              ),
              shiny::tr(
                lapply(table1[1, 2:ncol(table1)], th)
              ),
              shiny::tr(
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

      output$propensityModelTable <- DT::renderDataTable({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
          model <- getPropensityModel(connection = connection,
                                      targetId = targetId,
                                      comparatorId = comparatorId,
                                      databaseId = row$databaseId,
                                      analysisId = row$analysisId)

          table <- preparePropensityModelTable(model)
          options = list(columnDefs = list(list(className = 'dt-right',  targets = 0)),
                         pageLength = 15,
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
          return(table)
        }
      })

      psDistPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          if (row$databaseId %in% metaAnalysisDbIds) {
            ps <- getPs(connection = connection,
                        targetIds = row$targetId,
                        comparatorIds = row$comparatorId,
                        analysisId = row$analysisId)
          } else {
            targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
            comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
            outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
            ps <- getPs(connection = connection,
                        targetIds = targetId,
                        comparatorIds = comparatorId,
                        analysisId = row$analysisId,
                        databaseId = row$databaseId)
          }
          plot <- plotPs(ps, input$target, input$comparator)
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

      balancePlot <- shiny::reactive({
        bal <- balance()
        if (is.null(bal) || nrow(bal) == 0) {
          return(NULL)
        } else {
          row <- selectedRow()
          plot <- plotCovariateBalanceScatterPlot(balance = bal,
                                                  beforeLabel = "Before propensity score adjustment",
                                                  afterLabel = "After propensity score adjustment")
          return(plot)
        }
      })

      output$balancePlot <- shiny::renderPlot({
        return(balancePlot())
      })

      output$downloadBalancePlotPng <- shiny::downloadHandler(filename = "Balance.png",
                                                              contentType = "image/png",
                                                              content = function(file) {
                                                                ggplot2::ggsave(file, plot = balancePlot(), width = 4, height = 4, dpi = 400)
                                                              })

      output$downloadBalancePlotPdf <- shiny::downloadHandler(filename = "Balance.pdf",
                                                              contentType = "application/pdf",
                                                              content = function(file) {
                                                                ggplot2::ggsave(file = file, plot = balancePlot(), width = 4, height = 4)
                                                              })

      output$balancePlotCaption <- shiny::renderUI({
        bal <- balance()
        if (is.null(bal) || nrow(bal) == 0) {
          return(NULL)
        } else {
          row <- selectedRow()
          text <- "<strong>Figure 3.</strong> Covariate balance before and after propensity score adjustment. Each dot represents
      the standardizes difference of means for a single covariate before and after propensity score adjustment on the propensity
      score. Move the mouse arrow over a dot for more details."
          return(shiny::HTML(sprintf(text)))
        }
      })

      output$hoverInfoBalanceScatter <- shiny::renderUI({
        bal <- balance()
        if (is.null(bal) || nrow(bal) == 0) {
          return(NULL)
        } else {
          row <- selectedRow()
          hover <- input$plotHoverBalanceScatter
          point <- nearPoints(bal, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
          if (nrow(point) == 0) {
            return(NULL)
          }
          left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
          top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
          left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
          top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:",
                          left_px - 251,
                          "px; top:",
                          top_px - 150,
                          "px; width:500px;")
          beforeMatchingStdDiff <- formatC(point$beforeMatchingStdDiff, digits = 2, format = "f")
          afterMatchingStdDiff <- formatC(point$afterMatchingStdDiff, digits = 2, format = "f")
          shiny::div(
            style = "position: relative; width: 0; height: 0",
            shiny::wellPanel(
              style = style,
              shiny::p(shiny::HTML(paste0("<b> Covariate: </b>", point$covariateName, "<br/>",
                                          "<b> Std. diff before ",tolower(row$psStrategy),": </b>", beforeMatchingStdDiff, "<br/>",
                                          "<b> Std. diff after ",tolower(row$psStrategy),": </b>", afterMatchingStdDiff)))
            )
          )
        }
      })

      balanceSummaryPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          balanceSummary <- getCovariateBalanceSummary(connection = connection,
                                                       targetId = row$targetId,
                                                       comparatorId = row$comparatorId,
                                                       analysisId = row$analysisId,
                                                       beforeLabel = paste("Before", row$psStrategy),
                                                       afterLabel = paste("After", row$psStrategy))
          plot <- plotCovariateBalanceSummary(balanceSummary,
                                              threshold = 0.1,
                                              beforeLabel = paste("Before", row$psStrategy),
                                              afterLabel = paste("After", row$psStrategy))
          return(plot)
        }
      })

      output$balanceSummaryPlot <- shiny::renderPlot({
        balanceSummaryPlot()
      }, res = 100)

      output$balanceSummaryPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 7.</strong> Covariate balance before and after %s. The y axis represents
      the standardized difference of mean before and after %s on the propensity
      score. The whiskers show the minimum and maximum values across covariates. The box represents the
      interquartile range, and the middle line represents the median. The dashed lines indicate a standardized
      difference of 0.1."
          return(shiny::HTML(sprintf(text, row$psStrategy, row$psStrategy)))
        }
      })

      output$downloadBalanceSummaryPlotPng <- shiny::downloadHandler(filename = "BalanceSummary.png",
                                                                     contentType = "image/png",
                                                                     content = function(file) {
                                                                       ggplot2::ggsave(file, plot = balanceSummaryPlot(), width = 12, height = 5.5, dpi = 400)
                                                                     })

      output$downloadBalanceSummaryPlotPdf <- shiny::downloadHandler(filename = "BalanceSummary.pdf",
                                                                     contentType = "application/pdf",
                                                                     content = function(file) {
                                                                       ggplot2::ggsave(file = file, plot = balanceSummaryPlot(), width = 12, height = 5.5)
                                                                     })

      systematicErrorPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
          controlResults <- getControlResults(cohortMethodResult,
                                              connection = connection,
                                              targetId = targetId,
                                              comparatorId = comparatorId,
                                              analysisId = row$analysisId,
                                              databaseId = row$databaseId)

          plot <- plotScatter(controlResults)
          return(plot)
        }
      })

      output$systematicErrorPlot <- shiny::renderPlot({
        return(systematicErrorPlot())
      })

      output$downloadSystematicErrorPlotPng <- shiny::downloadHandler(filename = "SystematicError.png",
                                                                      contentType = "image/png",
                                                                      content = function(file) {
                                                                        ggplot2::ggsave(file, plot = systematicErrorPlot(), width = 12, height = 5.5, dpi = 400)
                                                                      })

      output$downloadSystematicErrorPlotPdf <- shiny::downloadHandler(filename = "SystematicError.pdf",
                                                                      contentType = "application/pdf",
                                                                      content = function(file) {
                                                                        ggplot2::ggsave(file = file, plot = systematicErrorPlot(), width = 12, height = 5.5)
                                                                      })

      systematicErrorSummaryPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          negativeControls <- getNegativeControlEstimates(connection = connection,
                                                          targetId = row$targetId,
                                                          comparatorId = row$comparatorId,
                                                          analysisId =  row$analysisId)
          if (is.null(negativeControls))
            return(NULL)

          plot <- plotEmpiricalNulls(negativeControls)
          return(plot)
        }
      })

      output$systematicErrorSummaryPlot <- shiny::renderPlot({
        return(systematicErrorSummaryPlot())
      }, res = 100)

      output$downloadSystematicErrorSummaryPlotPng <- shiny::downloadHandler(filename = "SystematicErrorSummary.png",
                                                                             contentType = "image/png",
                                                                             content = function(file) {
                                                                               ggplot2::ggsave(file, plot = systematicErrorSummaryPlot(), width = 12, height = 5.5, dpi = 400)
                                                                             })

      output$downloadSystematicErrorSummaryPlotPdf <- shiny::downloadHandler(filename = "SystematicErrorSummary.pdf",
                                                                             contentType = "application/pdf",
                                                                             content = function(file) {
                                                                               ggplot2::ggsave(file = file, plot = systematicErrorSummaryPlot(), width = 12, height = 5.5)
                                                                             })

      kaplanMeierPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
          km <- getKaplanMeier(connection = connection,
                               targetId = targetId,
                               comparatorId = comparatorId,
                               outcomeId = outcomeId,
                               databaseId = row$databaseId,
                               analysisId = row$analysisId)
          plot <- plotKaplanMeier(kaplanMeier = km,
                                  targetName = input$target,
                                  comparatorName = input$comparator)
          return(plot)
        }
      })

      output$kaplanMeierPlot <- shiny::renderPlot({
        return(kaplanMeierPlot())
      }, res = 100)

      output$downloadKaplanMeierPlotPng <- shiny::downloadHandler(filename = "KaplanMeier.png",
                                                                  contentType = "image/png",
                                                                  content = function(file) {
                                                                    ggplot2::ggsave(file, plot = kaplanMeierPlot(), width = 7, height = 5, dpi = 400)
                                                                  })

      output$downloadKaplanMeierPlotPdf <- shiny::downloadHandler(filename = "KaplanMeier.pdf",
                                                                  contentType = "application/pdf",
                                                                  content = function(file) {
                                                                    ggplot2::ggsave(file = file, plot = kaplanMeierPlot(), width = 7, height = 5)
                                                                  })

      output$kaplanMeierPlotPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 5.</strong> Kaplan Meier plot, showing survival as a function of time. This plot
      is adjusted using the propensity score: The target curve (<em>%s</em>) shows the actual observed survival. The
      comparator curve (<em>%s</em>) applies reweighting to approximate the counterfactual of what the target survival
      would look like had the target cohort been exposed to the comparator instead. The shaded area denotes
      the 95 percent confidence interval."
          return(shiny::HTML(sprintf(text, input$target, input$comparator)))
        }
      })


      forestPlot <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row) || !(row$databaseId %in% metaAnalysisDbIds)) {
          return(NULL)
        } else {
          results <- getMainResults(connection = connection,
                                    targetIds = row$targetId,
                                    comparatorIds = row$comparatorId,
                                    outcomeIds = row$outcomeId,
                                    analysisIds = row$analysisId)
          plot <- plotForest(results)
          return(plot)
        }
      })

      output$forestPlot <- shiny::renderPlot({
        forestPlot()
      })

      output$forestPlotCaption <- shiny::renderUI({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          text <- "<strong>Figure 6.</strong> Forest plot showing the per-database and summary hazard ratios (and 95 percent confidence
      intervals) comparing %s to %s for the outcome of %s, using %s. Estimates are shown both before and after empirical
      calibration. The I2 is computed on the uncalibrated estimates."
          return(shiny::HTML(sprintf(text, input$target, input$comparator, input$outcome, row$psStrategy)))
        }
      })

      output$downloadForestPlotPng <- shiny::downloadHandler(filename = "ForestPlot.png",
                                                             contentType = "image/png",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file, plot = forestPlot(), width = 12, height = 9, dpi = 400)
                                                             })

      output$downloadForestPlotPdf <- shiny::downloadHandler(filename = "ForestPlot.pdf",
                                                             contentType = "application/pdf",
                                                             content = function(file) {
                                                               ggplot2::ggsave(file = file, plot = forestPlot(), width = 12, height = 9)
                                                             })

      interactionEffects <- shiny::reactive({
        row <- selectedRow()
        if (is.null(row)) {
          return(NULL)
        } else {
          targetId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$target]
          comparatorId <- exposureOfInterest$exposureId[exposureOfInterest$exposureName == input$comparator]
          outcomeId <- outcomeOfInterest$outcomeId[outcomeOfInterest$outcomeName == input$outcome]
          subgroupResults <- getSubgroupResults(connection = connection,
                                                targetIds = targetId,
                                                comparatorIds = comparatorId,
                                                outcomeIds = outcomeId,
                                                databaseIds = row$databaseId,
                                                analysisIds = row$analysisId)
          if (nrow(subgroupResults) == 0) {
            return(NULL)
          } else {
            if (blind) {
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
          return(shiny::HTML(sprintf(text, input$target, input$comparator)))
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
          subgroupTable <- prepareSubgroupTable(subgroupResults, output = "html")
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

