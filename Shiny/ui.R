library(shiny)
library(shinythemes)
library(mgcv)

BodyFatData <- read.csv('../BodyFat.csv')

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
                   ,class = "btn-success")
    ),
    
    mainPanel(
      textOutput(outputId = "imputation"),
      textOutput(outputId = "processing"),
      textOutput(outputId = "warnings"),
      textOutput(outputId = "prediction")
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
    new_x = data.frame(AGE = as.numeric(AGE), WEIGHT = as.numeric(WEIGHT),
                       ABDOMEN= as.numeric(ABDOMEN), THIGH = as.numeric(THIGH),
                       FOREARM = as.numeric(FOREARM), WRIST = as.numeric(WRIST))
    fit = predict(final_model, newdata = new_x, se = TRUE)
    upr = fit$fit + 2*fit$se.fit
    dwn = fit$fit - 2*fit$se.fit
    paste("Estimated BodyFat is ",round(fit$fit,2)," with a 95% interval "
          ,"[",round(dwn,2),",",round(upr,2),"].")
  }
  
  imputation_input <- reactive({
    req(input$AGE)
    req(input$ABDOMEN)
    req(input$WEIGHT)
    
    WRIST = input$WRIST
    FOREARM = input$FOREARM
    THIGH = input$THIGH
    res_imput = imputation_for_missing_data(AGE = input$AGE, ABDOMEN = input$ABDOMEN, WEIGHT = input$WEIGHT,
                                            WRIST = input$WRIST, FOREARM = input$FOREARM, THIGH = input$THIGH)
    
    # print(res_imput)
    imput_text = ''
    
    if((!is.null(res_imput[[1]]))){
      WRIST = res_imput[[1]]
      imput_text = paste0( imput_text,"WRIST = ", WRIST, ' ')
    }

    if((!is.null(res_imput[[2]]))){
      FOREARM = res_imput[[2]]
      imput_text = paste0( imput_text,"FOREARM = ", FOREARM, ' ')
    }
    if((!is.null(res_imput[[3]]))){
      THIGH = res_imput[[3]]
      imput_text = paste0( imput_text,"THIGH = ", THIGH, ' ')
    }
    # print(imput_text)
    output$prediction <- renderText({
      pred_val(WRIST = WRIST, THIGH = THIGH, FOREARM = FOREARM)
    })
    
    if (!(imput_text == '')){
      imput_text = paste0("Imputation for ", imput_text)
      return(paste(imput_text))
    }
    else{
      return('')
    }
  }
  )
  

  
    output$imputation <- renderText(
      { req(input$predict_button)
        imputation_input()}
    )
   
}

shinyApp(ui = ui, server = server)