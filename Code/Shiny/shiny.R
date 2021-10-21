rm(list=ls())
# setwd("D:/WISC/stat628/Module 2/Stat628Module2_Group9")

library(shiny)
library(shinythemes)
library(mgcv)
library(rsconnect)
require('rsconnect')

BodyFatData <- read.csv('BodyFat.csv')

del_index = c(33, 39, 42, 48, 76, 96, 163, 172, 182, 216, 221)
BodyFat = BodyFatData[-del_index,]
BodyFat = subset(BodyFat, select = -c(IDNO,DENSITY))

final_model <- mgcv::gam(BODYFAT~ 1 + AGE  + WEIGHT
                         + s(ABDOMEN)
                         + s(THIGH)
                         + FOREARM + s(WRIST)
                         ,data = BodyFat, method = 'REML')


ui <- fluidPage(
  titlePanel("BodyFat Mearsurement for Men"),
  
  # Whole Layout
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "AGE",
                   label = "AGE(years)", min = 0,max = 100, step = 1,value = 25),
      
      textInput(inputId = "ABDOMEN",
                   label = "Circumference of ABDOMEN(cm)"),
      
      textInput(inputId = "WEIGHT",
                   label = "WEIGHT(lbs)"),
      
      textInput(inputId = "WRIST",
                   label = "Circumference of WRIST(cm)*"),
      
      textInput(inputId = "FOREARM",
                   label = "Circumference of FOREARM(cm)*"),
      
      textInput(inputId = "THIGH",
                   label = "Circumference of THIGH(cm)*"),
      
      helpText('* represents not necessary information.'),
      
      actionButton(inputId = "predict_button", label = "Calculate your BodyFat"
                   ,class = "btn-success"),
      actionButton(inputId = "clear_button", label = "Clear Output"
                   ),
      textOutput(outputId = "warnings"),
      textOutput(outputId = "warnings2"),
      tags$head(tags$style("#warnings{color: red;
                                 font-style: italic;
                                 }"
                 )
                 ),
      tags$head(tags$style("#warnings2{color: red;
                                 font-style: italic;
                                 }"
      )
      )
    ),
    
    mainPanel(
      textOutput(outputId = "imputation"),
      textOutput(outputId = "processing"),
      textOutput(outputId = "prediction"),
      plotOutput(outputId = "fit")
    )
    
  )
)

## Find the N nearest data, impute them with mean
imputation_for_missing_data <- function(N=10, AGE,ABDOMEN,WEIGHT
                   ,WRIST='',FOREARM='',THIGH=''){
  res = list(NULL)
  length(res) = 3
  if ((!(WRIST=='')) && (!(FOREARM=='')) && (!(THIGH==''))){
    return(res)
  }
  # print(res)
  n = nrow(BodyFat)
  dist = numeric(n)
  for (i in c(1:n)){
    if ((!(WRIST==''))){
      dist[i] = dist[i] + (scale(as.numeric(WRIST) - BodyFat$WRIST)[i])**2
    }
    if ((!(FOREARM==''))){
      dist[i] = dist[i] + (scale(as.numeric(FOREARM) - BodyFat$FOREARM)[i])**2
    }
    if ((!(THIGH==''))){
      dist[i] = dist[i] + (scale(as.numeric(THIGH) - BodyFat$THIGH)[i])**2
    }
    dist[i] = dist[i] + (scale(as.numeric(AGE) - BodyFat$AGE)[i])**2
                      + (scale(as.numeric(ABDOMEN) - BodyFat$ABDOMEN)[i])**2
                      + (scale(as.numeric(WEIGHT) - BodyFat$WEIGHT)[i])**2
  }
  # print(dist)
  index = order(dist)[1:N]
  # print(index)
  if ((WRIST=='')){
    res[[1]] = mean(BodyFat$WRIST[index])
  }
  if ((FOREARM=='')){
    res[[2]] = mean(BodyFat$FOREARM[index])
  }
  if ((THIGH=='')){
    res[[3]] = mean(BodyFat$THIGH[index])
  }
  return(res)
}

# Server function ------------------------------------------------
server <- function(input, output){
  
  pred_val <- function(AGE = input$AGE, ABDOMEN = input$ABDOMEN, WEIGHT = input$WEIGHT,
                       WRIST, FOREARM, THIGH){
    res = list()
    length(res)<-2
    new_x = data.frame(AGE = as.numeric(AGE), WEIGHT = as.numeric(WEIGHT),
                       ABDOMEN= as.numeric(ABDOMEN), THIGH = as.numeric(THIGH),
                       FOREARM = as.numeric(FOREARM), WRIST = as.numeric(WRIST))
    fit = predict(final_model, newdata = new_x, se = TRUE)
    upr = fit$fit + 2*fit$se.fit
    dwn = fit$fit - 2*fit$se.fit
    res[[1]] = paste("Estimated BodyFat is ",round(fit$fit,2)," with a 95% Confidence Interval "
          ,"[",round(dwn,2),",",round(upr,2),"].")
    res[[2]] = fit$fit
    return(res)
  }
  
  imputation_input <- reactive({
    # req(input$AGE)
    # req(input$ABDOMEN)
    # req(input$WEIGHT)
    
    WRIST = input$WRIST
    FOREARM = input$FOREARM
    THIGH = input$THIGH
    res_imput = imputation_for_missing_data(AGE = input$AGE, ABDOMEN = input$ABDOMEN, WEIGHT = input$WEIGHT,
                                            WRIST = input$WRIST, FOREARM = input$FOREARM, THIGH = input$THIGH)
    
    # print(res_imput)
    imput_text = ''
    output$processing<- renderText({
      "Calculating."
    })
    if((!is.null(res_imput[[1]]))){
      WRIST = res_imput[[1]]
      imput_text = paste0( imput_text,"WRIST = ", WRIST, '  ')
    }

    if((!is.null(res_imput[[2]]))){
      FOREARM = res_imput[[2]]
      imput_text = paste0( imput_text,"FOREARM = ", FOREARM, '  ')
    }
    if((!is.null(res_imput[[3]]))){
      THIGH = res_imput[[3]]
      imput_text = paste0( imput_text,"THIGH = ", THIGH, '  ')
    }
    # print(imput_text)
    output$processing<- renderText({
      "Calculating.."
    })
    res_val = pred_val(WRIST = WRIST, THIGH = THIGH, FOREARM = FOREARM)
    pred_bodyfat = res_val[[2]]
    if (pred_bodyfat<=1 || pred_bodyfat>=50){
      output$warnings<- renderText({
        "Error: Have an abnormal prediction, try another values."
      })
      output$processing <- renderText({
        "Finished!"})
      return('')
    }
    
    output$prediction <- renderText({
      res_val[[1]]
    })
    
    output$processing<- renderText({
      "Calculating..."
    })
    
    output$fit <- renderPlot({
      if(pred_bodyfat<6.48){
        lg_col = 'darkslategray1'
        lg_text = "Essential"
      }
      else if(pred_bodyfat<14.0187){
        lg_col = 'deepskyblue'
        lg_text = "Athletes"
      }
      else if(pred_bodyfat<18.9960){
        lg_col = 'green'
        lg_text = "Fit"
      }
      else if(pred_bodyfat<23.4314){
        lg_col = 'lightcoral'
        lg_text = "Acceptable"
      }
      else if(pred_bodyfat<37.1034){
        lg_col = 'Red'
        lg_text = "Obese"
      }
      else{
        lg_col = 'Red4'
        lg_text = "Over Obese"
      }
      par(pin = c(7,1))
      barplot(height=pred_bodyfat,horiz = TRUE, axes = FALSE,axisnames=FALSE, xlim = c(0,40),col = lg_col,
              legend.text = paste0("level of fitness: ",lg_text), ylab = 'BodyFat',names.arg = NULL,
              args.legend = list(x = "bottomright"))
      axis(3,c(0,6.4823,14.0187,18.9960,23.4314,37.1034,45.00))
    }, height = 300)
    
    output$processing <- renderText({
      "Finished!"
    })
    
    if (!(imput_text == '')){
      imput_text = paste0("Imputation for ", imput_text)
      output$warnings2 <- renderText({
        "Warnings: The Confidence interval may be underestimated since data was imputed."
      })
      return(paste(imput_text))
    }
    else{
      output$warnings2 <- renderText({
        ""
      })
      return('')
    }
  }
  )
  
  
  check_input <- reactive({
    output_text = ''
    if (input$AGE == ''){
      output_text = paste0(output_text,"Age ")
    }
    if (input$ABDOMEN == ''){
      output_text = paste0(output_text,"ABDOMEN ")
    }
    if (input$WEIGHT == ''){
      output_text = paste0(output_text,"WEIGHT ")
    }
    
    if (output_text != ''){
      output_text = paste0("Missing Values ", output_text,"are not allowed.")
      return(output_text)
    }
    
    
    # if (is.na(as.numeric(input$Age))){
    #   output_text = paste0(output_text,"Age ")
    # }
    if (is.na(as.numeric(input$ABDOMEN))){
      output_text = paste0(output_text,"ABDOMEN ")
    }
    if (is.na(as.numeric(input$WEIGHT))){
      output_text = paste0(output_text,"WEIGHT ")
    }
    if (is.na(as.numeric(input$WRIST)) && input$WRIST!=''){
      output_text = paste0(output_text,"WRIST ")
    }
    if (is.na(as.numeric(input$FOREARM))&& input$FOREARM!=''){
      output_text = paste0(output_text,"FOREARM ")
    }
    if (is.na(as.numeric(input$THIGH))&&input$THIGH!=''){
      output_text = paste0(output_text,"THIGH ")
    }
    
    
    
    if (output_text != ''){
      output_text = paste0("Please enter numbers for ", output_text,".")
      return(output_text)
    }
    
    if (as.numeric(input$AGE) < min(BodyFat$AGE) || as.numeric(input$AGE) > max(BodyFat$AGE)){
      output_text = paste0(output_text, "AGE ")
    }
    
    if (as.numeric(input$ABDOMEN) < 0.9*min(BodyFat$ABDOMEN) || as.numeric(input$ABDOMEN) > 1.1*max(BodyFat$ABDOMEN)){
      output_text = paste0(output_text, "ABDOMEN ")
    }
    if (as.numeric(input$WEIGHT) < 0.9*min(BodyFat$WEIGHT) || as.numeric(input$WEIGHT) > 1.1*max(BodyFat$WEIGHT)){
      output_text = paste0(output_text, "WEIGHT ")
    }
    if (input$WRIST!='' && (as.numeric(input$WRIST) < 0.9*min(BodyFat$WRIST) || as.numeric(input$WRIST) > 1.1*max(BodyFat$WRIST))){
      output_text = paste0(output_text, "WRIST ")
    }
    if (input$FOREARM!='' && (as.numeric(input$FOREARM) < 0.9*min(BodyFat$FOREARM) || as.numeric(input$FOREARM) > 1.1*max(BodyFat$FOREARM))){
      output_text = paste0(output_text, "FOREARM ")
    }
    if (input$THIGH!='' && (as.numeric(input$THIGH) < 0.9*min(BodyFat$THIGH) || as.numeric(input$THIGH) > 1.1*max(BodyFat$THIGH))){
      output_text = paste0(output_text, "THIGH ")
    }
    
    
    if (output_text != ''){
      output_text = paste0("Found extreame input values for ", output_text,", the estimate might be unreliable.")
      output$warnings <- renderText({
        output_text
      })
      return('')
    }
    
    return(output_text)
  })
  
  click_run <- reactive({
    res_check = check_input()
    if (res_check==''){
      return(imputation_input())
    }
    else{
      output$warnings <- renderText({
        res_check
      })
      return('')
    }
    
  })
  #quantile      0%      25%      50%      75%     100% 
  #         6.48342 14.01871 18.99608 23.43139 37.10338 
  
  
  observeEvent(input$clear_button,{
    output$processing <- renderText({
      ""
    })
    output$prediction <- renderText({
      ""
    })
    output$warnings <- renderText({
      ""
    })
    output$warnings2 <- renderText({
      ""
    })
    output$imputation <- renderText({
      ""
    })
    output$fit <- renderPlot({
      NULL
    })
    
  },ignoreInit = TRUE)
  observeEvent(input$predict_button,{
    
    output$imputation <- renderText(
      {
        click_run()}
    )
  },ignoreInit = TRUE)
  


   
}

shinyApp(ui = ui, server = server)