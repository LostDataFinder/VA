library(shiny)
library(tidyverse)
library(tools)
library(plotly)

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
      ),
    mainPanel(
      plotlyOutput("scatterPlot") #use plotlyOuput
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$scatterPlot <- renderPlotly({ # use renderPlotly
    p<- ggplot(data = exam,
               aes_string(x= input$xvariable,
                          y= input$yvariable)) +
      geom_point(color = "grey 10",
                 size = 1)
    ggplotly(p)
      })
  }


# Run the application 
shinyApp(ui = ui, server = server)
