library(shiny)
library(tidyverse)
library(tools)
library(plotly)

exam <- read_csv("data/Exam_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Drill-down Bar Chart"),
  mainPanel(
      plotlyOutput("race"), #use plotlyOuput
      plotlyOutput("gender"),
      verbatimTextOutput("info")
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output){
  output@race <- renderPlotly({
    p<- exam%>%
      plot_ly(x = ~RACE)
  })
  output$gender <- renderPlotly({
    d<-event_data("plotly_click")
    if(is.null(d)) return(NULL)
    
    P<- exam%>%
      filter(RACE %in% d$x) %>%
      ggplot(aex(x=GENDER))+
      geom_bar()
    ggplotly(p) %>%
      layout(xaxis = list(title=d$x))
  })
  output$info <- renderPrint({
    event_data("plotly_click")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
