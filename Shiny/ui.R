library(shiny)
library(shinythemes)

runExample("02_text")

ui <- fluidPage{
  titlePanel("BodyFat Mearsurement for Men"),
  
  # Whole Layout
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput(inputId = "AGE",
                   label = "years"),
      
      numericInput(inputId = "Circumference of ABDOMEN",
                   label = "cm"),
      
      numericInput(inputId = "WEIGHT",
                   label = "pounds"),
      
      numericInput(inputId = "Circumference of WRIST",
                   label = "cm"),
      
      numericInput(inputId = "Circumference of FOREARM",
                   label = "cm"),
      
      numericInput(inputId = "Circumference of THIGH",
                   label = "cm"),
    )
    
  )
}

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)