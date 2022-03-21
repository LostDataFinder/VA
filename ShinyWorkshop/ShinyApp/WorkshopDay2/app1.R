library(shiny)
library(tidyverse)
library(tools)

exam <- read_csv("data/Exam_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Subject Correlation Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "yvariable",
                  label = "y variable",
                  choices = c("English"="ENGLISH",
                              "Maths"="MATHS",
                              "Science"="SCIENCE"),
                  selected ="MATHS"),
      selectInput(inputId = "xvariable",
                  label = "x variable",
                  choices = c("English"="ENGLISH",
                              "Maths"="MATHS",
                              "Science"="SCIENCE"),
                  selected ="ENGLISH"),
      textInput(inputId = "plot_title",
                label = "Plot title",
                placeholder = "Enter text to be used as plot title"),
      actionButton(inputId = "goButton", label = "Go!")
      ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    input$goButton
    
    ggplot(data = exam,
           aes_string(x= input$xvariable,
                      y= input$yvariable)) +
      geom_point() +
      labs(title = isolate({
        toTitleCase(input$plot_title)
      }))
  })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
