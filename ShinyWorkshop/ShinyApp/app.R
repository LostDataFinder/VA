library(shiny)
library(tidyverse)


#read file
exam <- read_csv("data/Exam_data.csv")

ui <- fluidPage(
  titlePanel("Pupils Examination Results Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label="Subject",
                  choices = c("English" = "ENGLISH", #ENGLISH is the name of data column
                              "Maths" = "MATHS",
                              "Science"="SCIENCE"),
                  selected = "ENGLISH" #set default choice
                  ),
      sliderInput(inputId = "bins",
                  label = "Number of bins",
                  min = 5,
                  max = 20,
                  value = 10 #set default slider value
                  )
      ),
    mainPanel(
      plotOutput("distPlot")
      )
  )
)

server <- function(input, output){
  output$distPlot <- renderPlot({
    x <- unlist(exam[,input$variable])
    
    ggplot(exam, aes(x))+ 
      geom_histogram(bins = input$bins, 
                     color = "black",
                     fill = "light blue")
    }
    )
  }

shinyApp(ui=ui, server=server)

