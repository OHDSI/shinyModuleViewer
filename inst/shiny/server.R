library(dplyr)

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  
  #=============
  # sidebar menu
  #=============
  
  output$sidebarMenu <- shinydashboard::renderMenu(
    shinydashboard::sidebarMenu(
      id ='menu',
      
      addInfo(
        item = shinydashboard::menuItem(
          text = "About", 
          tabName = "About", 
          icon = shiny::icon("info")
        ), 
        infoId = "AboutInfo"
      ),
      
      addInfo(
        shinydashboard::menuItem(
          text = "Data", 
          tabName = "Data", 
          icon = shiny::icon("database")
        ), 
        "DataInfo"
      ),
      
      addInfo(
        shinydashboard::menuItem(
          text = "Prediction Diagnostic", 
          tabName = "PredictionDiagnostic", 
          icon = shiny::icon("stethoscope")
        ), 
        "PredictionDiagnosticInfo"
      ),
      
      addInfo(
        shinydashboard::menuItem(
          text = "Prediction", 
          tabName = "Prediction", 
          icon = shiny::icon("table")
        ), 
        "PredictionInfo"
      ),
      
      addInfo(
        shinydashboard::menuItem(
          text = "Estimation", 
          tabName = "Estimation", 
          icon = shiny::icon("question")
        ), 
        "EstimationInfo"
      ),
      addInfo(
        shinydashboard::menuItem(
          text = "Cohort Generator", 
          tabName = "CohortGenerator", 
          icon = shiny::icon("question")
        ), 
        "CohortGeneratorInfo"
      )
      
    )
  )
  
  #=============
  # Helper
  #=============
  
  shiny::observeEvent(input$AboutInfo, {
    showInfoBox("About", "www/About.html")
  })
  shiny::observeEvent(input$PredictionInfo, {
    showInfoBox("Prediction", "www/Prediction.html")
  })
  shiny::observeEvent(input$DataInfo, {
    showInfoBox("Data", "www/Data.html")
  })
  
  #=============
  # module severs
  #=============
  aboutServer(id = 'about')
  
  dataServer(id = 'data')
  
  # run the module when it is selected the first time only
  runServer <- shiny::reactiveValues(
    Prediction = 0,
    PredictionDiagnostic = 0,
    Estimation = 0,
    CohortGenerator = 0
  )
  
  
  shiny::observeEvent(input$menu,{
    
    runServer[[input$menu]] <- runServer[[input$menu]] + 1
    
    if(input$menu == 'Prediction' & runServer[['Prediction']]==1){ 
      predictionServer(
        id = 'prediction', 
        resultDatabaseSettings = jsonlite::fromJSON(
          keyring::key_get(
            "resultDatabaseSettings", 
            "lungcancer"
          )
        )
      )
    }
    
    if(input$menu == 'PredictionDiagnostic' & runServer[['PredictionDiagnostic']]==1){
      predictionDiagnosticServer(
        id = 'predictionDiagnostic',
        resultDatabaseSettings = jsonlite::fromJSON(
          keyring::key_get(
            "resultDatabaseSettings", 
            "predictionDiagnostic"
          )
        )
      )
    }
    
    if(input$menu == 'Estimation' & runServer[['Estimation']]==1){
      estimationServer(id = 'estimation')
    }
    
    if(input$menu == 'CohortGenerator' & runServer[['CohortGenerator']]==1){
      cohortGeneratorServer(
        id = 'cohortGenerator',
        generationConnectionDetails = jsonlite::fromJSON(
          keyring::key_get("generationConnectionDetails")
        )
      )
    }
    
    
  }
  )
  
  
  
  
  
}
)

# helper

addInfo <- function(item, infoId) {
  infoTag <- tags$small(
    class = "badge pull-right action-button",
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
    type = "button", 
    id = infoId,
    "i"
  )
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

showInfoBox <- function(title, htmlFileName) {
  shiny::showModal(shiny::modalDialog(
    title = title,
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    shiny::HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
  ))
}
