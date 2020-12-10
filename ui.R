### BTPS calculator 2
library(shiny)
library(shinythemes)
library(dplyr)
library(purrr)
library(openxlsx)


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  titlePanel("BTPS calculator"),
  hr(),
  
  # UI : Sidebar ----------------------------------------------------------------------
  
  sidebarLayout(
    sidebarPanel(
      "ATPS" %>% h2(),
      helpText("Input parameters at ATPS"),
      br(),
      
      numericInput("temp","Temperature (˚c)",value = NULL),
      
      fluidRow(
        column(6,
               numericInput("fev1_a", "FEV1 (L)",value = NULL) ),
        column(6,
               numericInput("fvc_a", "FVC (L)", value = NULL) )
        
      ),
      
      fluidRow(
        column(6,
               numericInput("ic_a", "IC (L)", value = NULL ) ),
        column(6,
               numericInput("ec_a", "EC (L)", value = NULL ) )
        
      ),
      
      fluidRow(
        column(6,
               numericInput("tv_a", "TV (L)", value = NULL ) ),
        column(6,
               numericInput("vc_a", "VC (L)", value = NULL ) )
        
      ),
      
      numericInput("pef_a", "PEF (L/min)", value = NULL ),
      
      
      
      
      checkboxInput("custom","Add custom parameter at ATPS ?" , value = F),
      fluidRow(
        column(8,
               uiOutput("col_text"),
               
               fluidRow(
                 column(3,
                        uiOutput("add_disp")), 
                 column(3,
                        uiOutput("remove_disp"))
               ),
               
               
        ),
        column(4,
               uiOutput("col_num")
               
        )
        
      )
    ),
    
    
    # UI : Main ----------------------------------------------------------------------
    
    mainPanel(
      
      
      fluidRow(
        column(6,align = "left",
               "Factor to convert volume to 37˚c Sat" %>% h4()
               
        ),
        
        column(1,align = "left", "=" %>% h4()),
        column(4,align = "left",
               tags$span(style="color:red", textOutput("btps")) %>% h4()       
        )
        
        
      ),
      hr(),
      
      dataTableOutput("table"),
      fluidRow(
        column(width = 12, downloadButton("download", "Download .xlsx",class = "btn-block"))
      )
      
    )
    
  )
  
  
  
  
  
  
  
)

