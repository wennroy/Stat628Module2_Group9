library(shiny)
library(shinythemes)


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
      
      actionButton(inputId = "Predict", label = "Calculate your BodyFat"
                   ,class = "btn-success")
    ),
    
    mainPanel(
      textOutput(outputId = "Prediction")
    )
    
  )
)

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)