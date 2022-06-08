# source("DataPulls.R")
# source("PlotsAndTables.R")

# positiveControlOutcome <- NULL
# 
# splittableTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
# 
# dataFolder <- "D:\\dev\\workspaces\\shinyModuleViewer\\tmp\\shinyData"
# blind <- FALSE
# 
# files <- list.files(dataFolder, pattern = ".rds")
# 
# # Find part to remove from all file names (usually databaseId):
# databaseFileName <- files[grepl("^database", files)]
# removeParts <- paste0(gsub("database", "", databaseFileName), "$")
# 
# # Remove data already in global environment:
# for (removePart in removeParts) {
#   tableNames <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", files[grepl(removePart, files)])) 
#   camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
#   camelCaseNames <- unique(camelCaseNames)
#   camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
#   suppressWarnings(
#     rm(list = camelCaseNames)
#   )
# }
# 
# # Load data from data folder. R data objects will get names derived from the filename:
# loadFile <- function(file, removePart) {
#   tableName <- gsub("_t[0-9]+_c[0-9]+$", "", gsub(removePart, "", file)) 
#   camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
#   if (!(tableName %in% splittableTables)) {
#     newData <- readRDS(file.path(dataFolder, file))
#     colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
#     if (exists(camelCaseName, envir = .GlobalEnv)) {
#       existingData <- get(camelCaseName, envir = .GlobalEnv)
#       newData$tau <- NULL
#       newData$traditionalLogRr <- NULL
#       newData$traditionalSeLogRr <- NULL
#       if (!all(colnames(newData) %in% colnames(existingData))) {
#          stop(sprintf("Columns names do not match in %s. \nObserved:\n %s, \nExpecting:\n %s", 
#                       file,
#                       paste(colnames(newData), collapse = ", "),
#                       paste(colnames(existingData), collapse = ", ")))
#                       
#       }
#       newData <- dplyr::bind_rows(existingData, newData)
#       newData <- unique(newData)
#     }
#     assign(camelCaseName, newData, envir = .GlobalEnv)
#   }
#   invisible(NULL)
# }
# # removePart <- removeParts[3]
# file <- files[grepl(removePart, files)][1]
# for (removePart in removeParts) {
#   invisible(lapply(files[grepl(removePart, files)], loadFile, removePart))
# }
# 
# tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
# tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
# metaAnalysisDbIds <- database$databaseId[database$isMetaAnalysis == 1]
#                
# 
# mainColumns <- c("description", 
#                  "databaseId", 
#                  "rr", 
#                  "ci95Lb",
#                  "ci95Ub",
#                  "p",
#                  "calibratedRr", 
#                  "calibratedCi95Lb",
#                  "calibratedCi95Ub",
#                  "calibratedP")
# 
# mainColumnNames <- c("<span title=\"Analysis\">Analysis</span>", 
#                      "<span title=\"Data source\">Data source</span>", 
#                      "<span title=\"Hazard ratio (uncalibrated)\">HR</span>",
#                      "<span title=\"Lower bound of the 95 percent confidence interval (uncalibrated)\">LB</span>",
#                      "<span title=\"Upper bound of the 95 percent confidence interval (uncalibrated)\">UB</span>", 
#                      "<span title=\"Two-sided p-value (uncalibrated)\">P</span>", 
#                      "<span title=\"Hazard ratio (calibrated)\">Cal.HR</span>",
#                      "<span title=\"Lower bound of the 95 percent confidence interval (calibrated)\">Cal.LB</span>",
#                      "<span title=\"Upper bound of the 95 percent confidence interval (calibrated)\">Cal.UB</span>", 
#                      "<span title=\"Two-sided p-value (calibrated)\">Cal.P</span>")