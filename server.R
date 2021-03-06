### BTPS calculator 2
library(shiny)
library(shinythemes)
library(shinyFeedback)
library(pins)
library(dplyr)
library(purrr)
library(openxlsx)
library(knitr)
library(rmarkdown)


# Register ----------------------------------------------------------------

k <- paste0("3d488bc5f88a4dbb","117324e1573bf080061724db")

board_register_github(repo = "Lightbridge-AI/btps-data", 
                      token = k)

# Function ----------------------------------------------------------------

get_factor <- function(btps.df , temp){
  
  
  
  out  <- if(temp %in% c(20:37)){
    
    btps.df %>% 
      filter(Gas_temp_c %in% {{temp}} ) %>% 
      pull(Factor_37) 
    
    
  }else{
    
    lm_fit <- lm(Factor_37 ~ Gas_temp_c, data = btps.df)
    newpoint <- tibble(Gas_temp_c = c(temp))
    
    predict(lm_fit, newdata = newpoint, interval = "none") %>% 
      as.numeric() 
  }
  
  out 
  
}

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  btps_df <- pin_reactive("btps-data", board = "github")
  
  btps_factor <- reactive({
    
    req(input$temp)
    get_factor(btps_df(), input$temp)
    
  })
  
  output$btps <- renderText({ paste( btps_factor() %>% round(digits = 3)) })
  
  
  # Compute vars at BTPS ----------------------------------------------------
  
  
  fev1_b <- reactive({
    
    req(input$fev1_a)
    input$fev1_a * btps_factor()
    
  })
  
  fvc_b <- reactive({
    
    req(input$fvc_a)
    input$fvc_a * btps_factor()
    
  })
  
  ratio_a <- reactive({
    
    req(input$fev1_a, input$fvc_a)
    (input$fev1_a / input$fvc_a) * 100
    
  })
  
  pef_b <- reactive({
    
    req(input$pef_a)
    input$pef_a * btps_factor()
    
  })
  
  tv_b <- reactive({
    
    req(input$tv_a)
    input$tv_a * btps_factor()
    
  })
  
  ic_b <- reactive({
    
    req(input$ic_a)
    input$ic_a * btps_factor()
    
  })
  
  ec_b <- reactive({
    
    req(input$ec_a)
    input$ec_a * btps_factor()
    
  })
  
  
  # Feedback ----------------------------------------------------------------
  
  any_input <- reactive({ 
    
    c(input$fev1_a, input$fvc_a, input$ic_a, 
      input$ec_a, input$tv_a, input$vc_a, input$pef_a) %>% isTruthy()
    
  })
  
  custom_input <- reactive({
    
    input$custom %>% isTruthy()
  })
  
  
  observeEvent(c(any_input(), custom_input(),input$temp),
               shinyFeedback::feedbackWarning(
                 "temp",
                 !isTruthy(input$temp) && ( any_input() || custom_input() ),
                 "Please input temperature"
               )
  )
  
  
  
  # Compute each rows ------------------------------------------------------------
  
  df_1 <- reactive({ 
    
    if(!isTruthy(input$fev1_a)){tibble(Parameter = c("FEV1"),
                                       ATPS = c(NA),
                                       BTPS = c(NA),
                                       Unit = c("L"))
      
    }else{
      
      tibble(Parameter = c("FEV1"),
             ATPS = c(input$fev1_a),
             BTPS = c(fev1_b()),
             Unit = c("L"))    
    }
  })
  
  df_2 <- reactive({ 
    
    if(!isTruthy(input$fvc_a)){tibble(Parameter = c("FVC"),
                                      ATPS = c(NA),
                                      BTPS = c(NA),
                                      Unit = c("L"))
    }else{
      
      tibble(Parameter = c("FVC"),
             ATPS = c(input$fvc_a),
             BTPS = c(fvc_b()),
             Unit = c("L"))
      
    }
  })
  
  df_3 <- reactive({ 
    
    if(!isTruthy(input$fev1_a) | !isTruthy(input$fvc_a) ){tibble(Parameter = c("FEV1/FVC"),
                                                                 ATPS = c(NA),
                                                                 BTPS = c(NA),
                                                                 Unit = c("%"))
    }else{
      
      tibble(Parameter = c("FEV1/FVC"),
             ATPS = c(ratio_a()),
             BTPS = c(NA),
             Unit = c("%"))
      
    }
  })
  
  df_4 <- reactive({ 
    
    if(!isTruthy(input$pef_a)){tibble(Parameter = c("PEF"),
                                      ATPS = c(NA),
                                      BTPS = c(NA),
                                      Unit = c("L/min"))
    }else{
      
      tibble(Parameter = c("PEF"),
             ATPS = c(input$pef_a),
             BTPS = c(pef_b()),
             Unit = c("L/min"))
      
    }
  })
  
  df_5 <- reactive({ 
    
    if(!isTruthy(input$tv_a)){tibble(Parameter = c("TV"),
                                     ATPS = c(NA),
                                     BTPS = c(NA),
                                     Unit = c("L"))
    }else{
      
      tibble(Parameter = c("TV"),
             ATPS = c(input$tv_a),
             BTPS = c(tv_b()),
             Unit = c("L"))
      
    }
  })
  
  df_6 <- reactive({ 
    
    if(!isTruthy(input$ic_a)){tibble(Parameter = c("IC"),
                                     ATPS = c(NA),
                                     BTPS = c(NA),
                                     Unit = c("L"))
    }else{
      
      tibble(Parameter = c("IC"),
             ATPS = c(input$ic_a),
             BTPS = c(ic_b()),
             Unit = c("L"))
      
    }
  })
  
  irv_a <- reactive({
    
    input$ic_a - input$tv_a
  })
  
  df_7 <- reactive({ 
    
    if(!isTruthy(input$ic_a) | !isTruthy(input$tv_a)){tibble(Parameter = c("IRV"),
                                                             ATPS = c(NA),
                                                             BTPS = c(NA),
                                                             Unit = c("L"))
    }else{
      
      tibble(Parameter = c("IRV"),
             ATPS = c(irv_a()),
             BTPS = c(irv_a() * btps_factor() ),
             Unit = c("L"))
      
    }
  })
  
  df_8 <- reactive({ 
    
    if(!isTruthy(input$ec_a)){tibble(Parameter = c("EC"),
                                     ATPS = c(NA),
                                     BTPS = c(NA),
                                     Unit = c("L"))
    }else{
      
      tibble(Parameter = c("EC"),
             ATPS = c(input$ec_a),
             BTPS = c(ec_b()),
             Unit = c("L"))
      
    }
  })
  
  erv_a <- reactive({
    
    input$ec_a - input$tv_a
  })
  
  df_9 <- reactive({ 
    
    if(!isTruthy(input$ec_a) | !isTruthy(input$tv_a)){tibble(Parameter = c("ERV"),
                                                             ATPS = c(NA),
                                                             BTPS = c(NA),
                                                             Unit = c("L"))
    }else{
      
      tibble(Parameter = c("ERV"),
             ATPS = c(erv_a()),
             BTPS = c(erv_a() * btps_factor() ),
             Unit = c("L"))
      
    }
  })
  
  df_10 <- reactive({ 
    
    if(!isTruthy(input$vc_a)){tibble(Parameter = c("VC"),
                                     ATPS = c(NA),
                                     BTPS = c(NA),
                                     Unit = c("L"))
    }else{
      
      tibble(Parameter = c("VC"),
             ATPS = c(input$vc_a),
             BTPS = c(input$vc_a * btps_factor()),
             Unit = c("L"))
      
    }
  })
  
  # Custom parameters ------------------------------------------------------------
  
  check_string <- reactive({  
    
    if(input$custom == T){"show"}else{"not_show"}
    
  })
  
  observeEvent(input$custom,{
    updateTabsetPanel(session, "binary", selected = check_string())
    
  })
  
  action <- reactive({
    
    req(input$custom == T)
    
    if((input$add - input$remove) < 0){integer(0)
    }else{
      seq_len(c(input$add - input$remove + 1))
    }
    
  })
  
  
  col_names_tx <- reactive(paste("Parameter", action() ))
  col_names_num <- reactive(paste("Value", action() ))
  
  ### Glitch UI (Not use)
  
  # output$add_disp <- renderUI({
  #   
  #   if(input$custom == T){actionButton("add","Add")    }else{NULL}
  #   
  # })
  # 
  # output$remove_disp <- renderUI({
  #   
  #   if(input$custom == T){actionButton("remove","Remove")  }else{NULL}
  #   
  # })
  
  output$col_text <- renderUI({
    
    map2(.x = col_names_tx(), .y = col_names_tx() , ~ textInput(.x, .y , value = isolate(input[[.x]])))
    
  })
  
  output$col_num <- renderUI({
    
    map2(.x = col_names_num(), .y = col_names_num() , ~ numericInput(.x, .y , value = isolate(input[[.x]])))
    
  })
  
  
  df_custom <- reactive({
    
    if( !isTruthy(input$temp) ){ tibble(Parameter = map_chr(col_names_tx(), ~ input[[.x]] ),
                                        ATPS = map_dbl(col_names_num(), ~ input[[.x]] ),
                                        BTPS = c(NA),
                                        Unit = c(""))
    }else{
      
      df <- tibble(Parameter = map_chr(col_names_tx(), ~ input[[.x]] ),
                   ATPS = map_dbl(col_names_num(), ~ input[[.x]] ),
                   Unit = "")
      
      df %>% 
        mutate(BTPS = ATPS * btps_factor() , .after = ATPS)
      
    }
    
  })
  
  # Bind & Render Table  -----------------------------------------------------------
  
  df_all <- reactive({
    
    disp <-  if(input$custom == F){
      
      bind_rows( df_1(), df_2(), df_3(), df_4(), df_5(),
                 df_6(), df_7(), df_8(), df_9(), df_10())
      
    }else{
      
      bind_rows( df_1(), df_2(), df_3(), df_4(), df_5(),
                 df_6(), df_7(), df_8(), df_9(), df_10(), df_custom())
    }
    
    
    disp %>% modify_if(is.numeric, ~round(.x,digits = 2))  
    
  })
  
  
    output$table <- renderDataTable({
    
    df_all()
    
  },options = list(bPaginate = FALSE))
  
  # Download  -----------------------------------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0("Parameters at BTPS",".xlsx") #remove .xxx
    },
    content = function(file) {
      
      openxlsx::write.xlsx(df_all(), file)
    }
  )
  
  
  # Download - report_2 - not working -------------------------------------------------------
  
  # output$report_2 <- downloadHandler(
  #     # For PDF output, change this to "report.pdf"
  #     filename = "report.pdf",
  #     content = function(file) {
  #         # Copy the report file to a temporary directory before processing it, in
  #         # case we don't have write permissions to the current working dir (which
  #         # can happen when deployed).
  #         tempReport <- file.path(tempdir(), "report_pdf.Rmd")
  #         file.copy("report_pdf.Rmd", tempReport, overwrite = TRUE)
  #         
  #         # Set up parameters to pass to Rmd document
  #         params <- list(btps_factor = btps_factor(),
  #                        table = df_all())
  #         
  #         # Knit the document, passing in the `params` list, and eval it in a
  #         # child of the global environment (this isolates the code in the document
  #         # from the code in this app).
  #         rmarkdown::render(tempReport, output_file = file,
  #                           params = params,
  #                           envir = new.env(parent = globalenv())
  #         )
  #     }
  # )  
  
  # Download - report_3 -------------------------------------------------------
  
  output$report_3 <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_word.Rmd")
      file.copy("report_word.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(btps_factor = btps_factor(),
                     table = df_all(),
                     temp = input$temp)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
}
