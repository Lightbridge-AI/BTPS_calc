### BTPS calculator 2
library(shiny)
library(shinythemes)
library(dplyr)
library(purrr)
library(openxlsx)

# Function ----------------------------------------------------------------


get_factor_2 <- function(temp){
  
  btps <- tibble(Gas_temp_c = c(20:37),
                 Factor_37 = c(1.102,1.096,1.091,1.085,1.080,1.075,1.068,1.063,
                               1.057,1.051,1.045,1.039,1.032,1.026,1.020,1.014,1.007,1.000))
  
  
  out  <- if(temp %in% c(20:37)){
    
    btps %>% 
      filter(Gas_temp_c %in% c(floor({{temp}}) , ceiling({{temp}}) ) ) %>% 
      pull(Factor_37) 
    
    
  }else{
    
    lm_fit <- lm(Factor_37 ~ Gas_temp_c, data = btps)
    newpoint <- tibble(Gas_temp_c = c(temp))
    
    predict(lm_fit, newdata = newpoint, interval = "none") %>% 
      as.numeric() 
  }
  
  out 
  
}

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  btps_factor <- reactive({
    
    req(input$temp)
    get_factor_2(input$temp)
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
  
  action <- reactive({
    
    req(input$custom == T)
    
    if((input$add - input$remove) < 0){integer(0)
    }else{
      seq_len(c(input$add - input$remove + 1))
    }
    
  })
  
  
  col_names_tx <- reactive(paste("Parameter", action() ))
  col_names_num <- reactive(paste("Value", action() ))
  
  output$add_disp <- renderUI({
    
    if(input$custom == T){actionButton("add","Add")    }else{NULL}
    
  })
  
  output$remove_disp <- renderUI({
    
    if(input$custom == T){actionButton("remove","Remove")  }else{NULL}
    
  })
  
  output$col_text <- renderUI({
    
    map2(.x = col_names_tx(), .y = col_names_tx() , ~ textInput(.x, .y , value = isolate(input[[.x]])))
    
  })
  
  output$col_num <- renderUI({
    
    map2(.x = col_names_num(), .y = col_names_num() , ~ numericInput(.x, .y , value = isolate(input[[.x]])))
    
  })
  
  output$text <- renderText({
    map_chr(col_names_tx(), ~ input[[.x]] )
  })
  
  output$num <- renderText({
    map_dbl(col_names_num(), ~ input[[.x]] )
  })
  
  df_custom <- reactive({
    
    req(input$temp)
    df <- tibble(Parameter = map_chr(col_names_tx(), ~ input[[.x]] ),
                 ATPS = map_dbl(col_names_num(), ~ input[[.x]] ),
                 Unit = "")
    
    df %>% 
      mutate(BTPS = ATPS * btps_factor() , .after = ATPS)
    
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
    
  }, options =list(lengthMenu = c(15, 20, 30), pageLength = 15))
  
  # Download  -----------------------------------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0("Parameters at BTPS",".xlsx") #remove .xxx
    },
    content = function(file) {
      
      openxlsx::write.xlsx(df_all(), file)
    }
  )
  
  
  
  
  
  
  
  
}

