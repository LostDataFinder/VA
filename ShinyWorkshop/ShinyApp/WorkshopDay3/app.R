library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(plotly)
library(bslib)
library(thematic)

thematic::thematic_shiny()

exam <- read_csv("data/Exam_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version=4,bootswatch = "journal"), #apply theme
  navlistPanel(
    id= "tabset",
    "Data Preparation",
    tabPanel("Data Import","View Table"),
    tabPanel("Data Transformation", "Output Table"),
    "IDEA",
    tabPanel("Univariate Analysis", "Distribution Plot"),
    tabPanel("Bivariate Analysis", "Correlation matrix")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  bs_themer() #to display theme customiser
}


# Run the application 
shinyApp(ui = ui, server = server)
