

loadCohortGeneratorData <- function(dataDir) {
  for(file in list.files(dataDir)) {
    data <- readr::read_csv(file.path(theDir, file))
    file <- sub('\\.csv$', '', file) 
    file <- SqlRender::snakeCaseToCamelCase(file)
    assign(file, data, envir = .GlobalEnv)
  }
}
