# @file Ui.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This is a wrapper UI for each module
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

source("modules/estimation/module.R")
source("modules/prediction/module.R")
source("modules/predictionDiagnostic/module.R")
source("modules/cohortgenerator/module.R")
source("modules/data/module.R")
source("modules/about/module.R")

ui <- shinydashboard::dashboardPage(
  skin = 'black',
  
  shinydashboard::dashboardHeader(
    title = "OHDSI Analysis Viewer", 
    tags$li(
      shiny::div(
        shiny::img(
          src = 'logo.png',
          title = "OHDSI", 
          height = "40px", 
          width = "40px"
        ),
        style = "padding-top:0px; padding-bottom:0px;"
      ),
      class = "dropdown"
    )
  ), 
  
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenuOutput("sidebarMenu")
  ), # end sidebar
  
  # ADD EACH MODULE SHINY AS A TAB ITEM
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      
      shinydashboard::tabItem(
        tabName = "About",
        aboutViewer('intro')
      ),
      
      shinydashboard::tabItem(
        tabName = "Data",
        dataViewer('data')
      ),
      
      shinydashboard::tabItem(
        tabName = "PredictionDiagnostic",
        predictionDiagnosticViewer('predictionDiagnostic')
      ),
      
      shinydashboard::tabItem(
        tabName = "Prediction",
        predictionViewer('prediction')
      ),
      
      shinydashboard::tabItem(
        tabName = "Estimation",
        estimationViewer(id='estimation')
      ),
      
      shinydashboard::tabItem(
        tabName = "CohortGenerator",
        cohortGeneratorViewer(id="cohortGenerator")
      )
      
      
    )
    
  )
  
  
  
)
