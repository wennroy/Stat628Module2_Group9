library(shiny)
library(shinythemes)


fluidPage(
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