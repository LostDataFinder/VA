library(shiny)
library(networkD3)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(htmlwidgets)
library(plotly)
library(DT)

# Read csv
procurement_data <- read_csv("data/government-procurement-via-gebiz.csv")
agency_ministry <- read_xlsx("data/AgencyMinistry.xlsx")

# Join the 2 datasets
merged_data <- merge(procurement_data, agency_ministry, by="agency")

# Convert award_date string values to YYYY-MM-DD date format
merged_data$award_date <- as.Date(merged_data$award_date, "%d/%m/%Y")
merged_data$wog <- "Public Sector"

# Filter by date range

#date_filtered_data<- filter(merged_data, award_date >= as.Date("2020-01-01") 
#                            & award_date <= as.Date("2020-12-31"))

# Aggregate data at WOG, Ministry and Agency levels
# WOG level aggregation
#wog_grouped_data <- date_filtered_data%>%
#  group_by(wog, supplier_name) %>%
#  summarize(total_amt = sum(awarded_amt))

# Ministry level aggregation
#ministry_grouped_data <- date_filtered_data%>%
#  group_by(ministry, supplier_name) %>%
#  summarize(total_amt = sum(awarded_amt))

# Agency level aggregation
#agency_grouped_data <- date_filtered_data%>%
#  group_by(agency, supplier_name) %>%
#  summarize(total_amt = sum(awarded_amt))

# FUnction to sort and filter top X suppliers
top_suppliers_apply <- function(data, number){
  
  sorted_data <- data[order(data$total_amt,
                                    na.last = TRUE,
                                    decreasing = TRUE),]
  
  if(number<=length(sorted_data$supplier_name)){
    sorted_data<-sorted_data[1:number,]
  }
  if(number>length(sorted_data$supplier_name)){
    sorted_data}
  sorted_data
}
###########

# Apply filter by top X suppliers
# Scenario 1: Top X Suppliers in WOG
#wog_supplier_filtered_data <- top_suppliers_apply(wog_grouped_data,50)
#top_suppliers <- as.data.frame(unique(wog_supplier_filtered_data$supplier_name))
#names(top_suppliers)[1] <- "supplier_name"
#supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name)


# Scenario 2: Top X Suppliers in Ministry
#ministry_supplier_filtered_data <- filter(ministry_grouped_data, ministry == "MHA")
#ministry_supplier_filtered_data <- top_suppliers_apply(ministry_supplier_filtered_data,50)
#top_suppliers <- as.data.frame(unique(ministry_supplier_filtered_data$supplier_name))
#names(top_suppliers)[1] <- "supplier_name"
#supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name &
#                                   ministry == "MHA") # this filter could be changed to exclude procurement of other ministry


# Scenario 3: Top X Suppliers in Agency
#agency_supplier_filtered_data <- filter(agency_grouped_data, ministry == "Housing and Development Board")
#agency_supplier_filtered_data <- top_suppliers_apply(agency_supplier_filtered_data,50)
#top_suppliers <- as.data.frame(unique(agency_supplier_filtered_data$supplier_name))
#names(top_suppliers)[1] <- "supplier_name"
#supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name &
#                                   agency == "Casino Regulatory Authority") # this filter could be changed to exclude procurement of other ministry



# Create links
#min_agency_links <- supplier_filtered_data%>%
#  group_by(ministry, agency) %>%
#  summarize(total_amt = sum(awarded_amt))
#names(min_agency_links)[1]<- "source"
#names(min_agency_links)[2]<- "target"
#min_agency_links$group<- min_agency_links$source


#agency_supplier_links <- supplier_filtered_data%>%
#  group_by(agency, supplier_name, ministry) %>%
#  summarize(total_amt = sum(awarded_amt))
#names(agency_supplier_links)[1]<- "source"
#names(agency_supplier_links)[2]<- "target"
#names(agency_supplier_links)[3]<- "group"


#links <- rbind(min_agency_links,agency_supplier_links)


# Create a data frame for nodes
#nodes <- links %>% 
#  summarise(name = union(source, target))



# Function to generate IDsource and IDtarget
IDgenerate <- function(data1, data2){
  IDlist <- c()
  for (row in data1){
    a <- match(row,data2) - 1
    IDlist <- append(IDlist,a)
  }
  IDlist <- as.data.frame(IDlist)
}

# Function for drilldown
funct <-
  function(n){
    isp <- sprintf("Select * From df Where df.name='%s';", n)
    isd <- sqldf::sqldf(isp)
    return(isd)
  }


# Find node IDs for links
#links$IDsource <- match(links$source, nodes$name) - 1
#links$IDtarget <- match(links$target, nodes$name) - 1

#p <- sankeyNetwork(
#  Links = links,
#  Nodes = nodes,
#  Source = "IDsource",
#  Target = "IDtarget",
#  Value = "total_amt",
#  NodeID = "name",
#  width = 1200,
#  height = 900,
#  nodeWidth = 5,
#  nodePadding = 3,
#  fontSize = 10,
#  sinksRight = TRUE,
#  NodeGroup = "name",
#  LinkGroup = "group"
  
#)

#print(p)


###########

#filtered_data1<- filter_apply(input_ministry = "MND", 
#                 #input_agency = "Housing and Development Board",
#                 from_award_date = as.Date("2015-01-01"),
#                 to_award_date = as.Date("2021-12-31"),
#                 from_awarded_amt = 9000000,
#                 to_awarded_amt = 999999999999)





##################################
# https://stackoverflow.com/questions/70754632/creating-sankey-diagram-in-r-making-the-plot-output-interpretable


########################################


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SINGAPORE PUBLIC SECTOR PROCUREMENT"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "input_level",
                      label = "Suppliers Across",
                      choices = c("Public Sector", "Ministry", "Agency"),
                      selected = "Ministry"),
            selectInput(inputId = "input_ministry",
                        label = "Ministry",
                        selected = "MCCY",
                        choices = sort(as.vector(unique(merged_data$ministry)))),
            selectInput(inputId = "input_agency",
                        label = "Agency",
                        selected = "Accounting and Corporate Regulatory Authority",
                        choices = sort(as.vector(unique(merged_data$agency)))),
            sliderInput(inputId = "input_top_suppliers", 
                        label = "Top Number of Suppliers",
                        min = 1,
                        max = 999,
                        value=20),
            dateRangeInput(inputId = "input_award_daterange",
                           label = "Award Date Range",
                           start = as.Date("2021-01-01"),
                           end = as.Date("2021-12-31"),
                           min = as.Date("2015-01-01"),
                           max = as.Date("2021-12-31"))
            #submitButton()
        ),

        # Show a plot of the generated distribution
        mainPanel(sankeyNetworkOutput("sankeyPlot"),
                  plotlyOutput("SupplierCustomerBar")
           
        )
    )
)

# Define server logic 
server <- shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp)
  


  output$sankeyPlot <- renderSankeyNetwork({
    
    # Filter by date range
    date_filtered_data<- merged_data %>% filter(award_date >= input$input_award_daterange[1]
                                                & award_date <= input$input_award_daterange[2])
    
    # Aggregate data at WOG, Ministry and Agency levels
    # WOG level aggregation
    wog_grouped_data <- date_filtered_data%>%
      group_by(wog, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    # Ministry level aggregation
    ministry_grouped_data <- date_filtered_data%>%
      group_by(ministry, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    # Agency level aggregation
    agency_grouped_data <- date_filtered_data%>%
      group_by(agency, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    if(input$input_level == "Public Sector"){
        wog_supplier_filtered_data <- top_suppliers_apply(wog_grouped_data,input$input_top_suppliers)
        top_suppliers <- as.data.frame(unique(wog_supplier_filtered_data$supplier_name))
        names(top_suppliers)[1] <- "supplier_name"
        supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name)
      }
      
    if(input$input_level == "Ministry"){
        ministry_supplier_filtered_data <- filter(ministry_grouped_data, ministry == input$input_ministry)
        ministry_supplier_filtered_data <- top_suppliers_apply(ministry_supplier_filtered_data,input$input_top_suppliers)
        top_suppliers <- as.data.frame(unique(ministry_supplier_filtered_data$supplier_name))
        names(top_suppliers)[1] <- "supplier_name"
        supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name
                                         & ministry==input$input_ministry)
      }
      
    if(input$input_level == "Agency"){
        agency_supplier_filtered_data <- filter(agency_grouped_data, agency == input$input_agency)
        agency_supplier_filtered_data <- top_suppliers_apply(agency_supplier_filtered_data,input$input_top_suppliers)
        top_suppliers <- as.data.frame(unique(agency_supplier_filtered_data$supplier_name))
        names(top_suppliers)[1] <- "supplier_name"
        supplier_filtered_data <- filter(date_filtered_data, supplier_name %in% top_suppliers$supplier_name
                                         & agency==input$input_agency) 
      } 
    
    
    # Create links
    min_agency_links <- supplier_filtered_data%>%
      group_by(ministry, agency) %>%
      summarize(total_amt = sum(awarded_amt))
    names(min_agency_links)[1]<- "source"
    names(min_agency_links)[2]<- "target"
    min_agency_links$group<- min_agency_links$source 
    
    
    agency_supplier_links <- supplier_filtered_data%>%
      group_by(agency, supplier_name, ministry) %>%
      summarize(total_amt = sum(awarded_amt))
    names(agency_supplier_links)[1]<- "source"
    names(agency_supplier_links)[2]<- "target"
    names(agency_supplier_links)[3]<- "group" 
    
    
    links <- rbind(min_agency_links,agency_supplier_links)
    
    # Create a data frame for nodes
    nodes <- data.frame(
      name=c(as.character(links$source), 
             as.character(links$target)) %>% unique()
    )
    
    #nodes <- as.data.frame(links$source)
    #names(nodes)[1]<- "name"
    
    #nodes_target <- as.data.frame(links$target)
    #names(nodes_target)[1]<- "name"
    
    #nodes <- rbind(nodes,nodes_target)
    
    
    # Find node IDs for links
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    #links$IDsource <- IDgenerate(links$source, nodes$name)
    
    #links$IDtarget <- IDgenerate(links$target, nodes$name)
    
    df<- links
    URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/',
                  'master/JSONdata/energy.json')
    energy <- jsonlite::fromJSON(URL)
    
    
    san <- sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "total_amt",
      NodeID = "name",
      width = 1200,
      height = 900,
      nodeWidth = 5,
      nodePadding = 3,
      fontSize = 10,
      sinksRight = FALSE,
      LinkGroup = "group",
      units = "SGD")

    clickFun <- 
      'function() { 
          d3.selectAll(".node").on("mousedown.drag", null);
          d3.selectAll(".node").on("click",function(d){ Shiny.onInputChange("id", d.name); });
        }'
    
    onRender(san, clickFun)
  
    
  })
  
  #output$SupplierCustomerBar <- renderPlotly({
  #  output$table <- DT::renderDataTable(DT::datatable(funct(input$id)))
  
  output$SupplierCustomerBar <- renderPlotly({
    
    # Filter by date range
    date_filtered_data<- merged_data %>% filter(award_date >= input$input_award_daterange[1]
                                                & award_date <= input$input_award_daterange[2])
    
    # Agency level aggregation
    agency_grouped_data <- date_filtered_data%>%
      group_by(agency, supplier_name) %>%
      summarize(total_amt = sum(awarded_amt))
    
    clickID <- input$id
    if(is.null(clickID)) return(NULL)
    
    p <- agency_grouped_data %>%
      filter(supplier_name == clickID) %>%
      ggplot(aes(x=reorder(agency,-total_amt),
                 y=total_amt,
                 text = paste(agency, "\nTotal amount: $", format(round(total_amt,1),big.mark=","))
                 
                 
                 ))+
      geom_bar(stat="identity", fill="steelblue")+
      labs(title = clickID, x = "Agency", y = "Total amount($)")+
      theme(axis.text.x = element_text(angle=30,vjust=0.5,hjust=1),
            panel.background = element_rect(fill = "white"))+
      scale_y_continuous(labels = function(x) format(x, big.mark=",", scientific = FALSE))
      theme_minimal()
    ggplotly(p, tooltip="text")
    })

  })
    
  #})
  

# Run the application 
shinyApp(ui = ui, server = server)
