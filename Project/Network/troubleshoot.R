library(shiny)
library(networkD3)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tidygraph)
library(ggraph)
library(igraph)
library(visNetwork)
library(RColorBrewer)

# Read csv
procurement_data <- read_csv("data/government-procurement-via-gebiz.csv")
agency_ministry <- read_xlsx("data/AgencyMinistry.xlsx")

# Join the 2 datasets
merged_data <- merge(procurement_data, agency_ministry, by="agency")

# Convert award_date string values to YYYY-MM-DD date format
merged_data$award_date <- as.Date(merged_data$award_date, "%d/%m/%Y")
merged_data$wog <- "Public Sector"

# Filter by date range

date_filtered_data<- filter(merged_data, award_date >= as.Date("2020-01-01") 
                            & award_date <= as.Date("2020-12-31") & tender_detail_status != "Awarded to No Suppliers")

# Filter data based on WOG, Ministry or Agency level
date_filtered_data <- filter(date_filtered_data, ministry == "MCCY")
#date_filtered_data <- filter(date_filtered_data, agency == "Housing and Development Board")

# Aggregate awarded_amt based on Agency-Supplier
agency_grouped_data <- date_filtered_data%>%
  group_by(ministry, agency, supplier_name) %>%
  summarize(total_amt = sum(awarded_amt))




#Extract top X suppliers of each agency

agencies <- as.data.frame(unique(agency_grouped_data$agency))
names(agencies)[1] <- "agency"


topXsuppliers <- data.frame(matrix(ncol=1, nrow=0))
names(topXsuppliers)[1] <- "supplier_name"


for (agencyname in agencies){
  a <- agencyname
  b <- dplyr::filter(agency_grouped_data, agency %in% a) %>%
    arrange(desc(total_amt)) %>%
    slice(1:10)
  c <- data.frame(b$supplier_name)
  names(c)[1] <- "supplier_name"
  topXsuppliers <- rbind(topXsuppliers,c)

}

# filter agency_grouped_data based on top suppliers
agency_records_with_topXsuppliers <- dplyr::filter(agency_grouped_data, supplier_name %in% topXsuppliers$supplier_name)

# Generate list of nodes

agency_nodes <- agency_records_with_topXsuppliers %>% 
  summarise(name =agency) %>%
    unique()
agency_nodes$type <- agency_nodes$ministry
agency_nodes <- subset(agency_nodes, select=-c(ministry, agency))

supplier_nodes <- subset(agency_records_with_topXsuppliers, select=-c(ministry, agency, total_amt))
supplier_nodes <- supplier_nodes %>% 
  summarise(name = supplier_name) %>%
  unique()
supplier_nodes$type <- "Supplier"

all_nodes <- rbind(agency_nodes , supplier_nodes)
all_nodes$id <- as.integer(rownames(all_nodes))
all_nodes <- all_nodes %>%
  rename(group = type, label = name)
all_nodes$title <- all_nodes$label



# Edges

n_edges <- agency_records_with_topXsuppliers
n_edges <- rename(n_edges, sourceLabel = agency, targetLabel = supplier_name)
n_edges <- subset(n_edges, select = -c(ministry))
n_edges <- left_join(n_edges, all_nodes, by = c("sourceLabel"="label")) %>%
  select(sourceLabel,targetLabel,total_amt,id)
n_edges <- rename(n_edges, source = id)
n_edges <- left_join(n_edges, all_nodes, by = c("targetLabel"="label")) %>%
  select(sourceLabel,targetLabel,total_amt, source, id)
n_edges <- rename(n_edges, target = id)


########## Static graph
static_graph <- tbl_graph(nodes = all_nodes,
                          edges = n_edges, 
                           directed = TRUE)


# Interactive network graph
# data prep
n_edges_aggregated <- n_edges %>%
  left_join(all_nodes, by = c("sourceLabel" = "label")) %>%
  rename(from = id) %>%
  left_join(all_nodes, by = c("targetLabel" = "label")) %>%
  rename(to = id) %>%
  group_by(from, to) %>%
  filter(total_amt > 1) %>%
  ungroup()
n_edges_aggregated$Weight <- n_edges_aggregated$total_amt
n_edges_aggregated$title <- paste("$", formatC(n_edges_aggregated$Weight, big.mark = ",", format = "f", digits = 0))
n_edges_aggregated$color.opacity <- 0.1*n_edges_aggregated$Weight/100000



all_nodes$betweenness_centrality <- betweenness(static_graph)

all_nodes$closeness_centrality <- closeness(static_graph)

all_nodes$degree_centrality <- degree(static_graph)





# Assigning centrality measures to node size 
# Degree centrality
all_nodes$size <- degree(static_graph) 


visNetwork(all_nodes, n_edges_aggregated) %>%
  visGroups(groupname = "Supplier", shape = "triangle", color = "orange") %>%
  visIgraphLayout(layout = "layout_nicely") %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE,
             selectedBy="closeness_centrality") %>%
  visInteraction() %>%
  visLegend() %>%
  visLayout(randomSeed = 123) %>%
  visEdges(color = palette()[n_edges_aggregated$Weight])

# visNetwork