###  Title: Shiny Application for U of U Job Application
###  Author: Cory Reschke
###  Date: 5-22-2017

## Description:
##   This application uses data from the "U.S. Chronic Disease Indicators" dataset, and specifically covers a
##   selection of tobacco-related data derived from the Center for Disease Control's "Behavioral Risk Factor  
##   Surveillance System" (BRFSS) data. The dashboard displays the top 5 and bottom 5 states for the selected topic 
##   over the selected years.
##
##   Original data was obtained from https://www.healthdata.gov/dataset/us-chronic-disease-indicators-cdi. I manipulated
##   some of the data before loading it into this project.

## Prerequisites:
##  Install 'shiny','ggplot2','dplyr', RColorBrewer', 'ggmap', 'maps', 'mapdata', & 'sqldf' packages
##  Working directory may need to be changed to reflect the users environment.

library(shiny)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sqldf)

# Pull the list of measures from the dataset. Used to setup UI
questions <- read.csv("./_data/cdc.csv") %>% 
  distinct(Question)

# Create User Interface
ui <- fluidPage( titlePanel("CDC Tobacco Data Exploration"),
                 br(),  # Line break
                 
                 # Sidebar for inputs
                 sidebarPanel(selectInput(inputId = "measureInp",
                                          label = "Select a measure",
                                          choices = questions,
                                          selected = questions[2,],
                                          multiple = F),
                              sliderInput(inputId = "yearInp",
                                          label="Select year range",
                                          min=2012,
                                          max=2014,
                                          value=c(2012, 2014))
                              ),
                 
                 # Main panel for visualizations
                 mainPanel(
                   tags$head(tags$style("body {background-color: lightgrey;
                                        font-family: Arial}")),
                   
                   fluidRow(
                     column(width = 6,plotOutput("Top5")),
                     column(width = 6,plotOutput("Bottom5"))
                   ),
                   br(), # Line break
                   fluidRow(
                     column(width = 6,plotOutput("Map"))
                   )
                   )
)

## Server handles the data processing steps and returns the resulting dataframe to UI
server <- function(input, output) {
  
  # Load data
  cdc_data <- read.csv("./_data/cdc.csv", stringsAsFactors = F)
  states <- map_data("state")
  
  # Prepare data
  var <- reactive({ 
    
    ## Subset the data based on the Season (Input) Selection and group by teams
    cdc_data %>% filter(YearStart >= input$yearInp[1] &
                          YearStart <= input$yearInp[2] &
                          Question == input$measureInp) %>%
      select(Question, LocationAbbr, LocationDesc, DataValue, DataValueType, DataValueUnit) %>% 
      group_by_(.dots= c("LocationAbbr","LocationDesc")) %>% 
      summarise(DataValue = mean(DataValue)) %>% 
      as.data.frame()
  })
  
  
  ## ggplots to show the top & bottom five states and also to generate the heat map
  ## The ggplot is built dynamically using renderPlot function
  
  # Top 5
  output$Top5 <- renderPlot({
    
    filterData <- var()
    
    # data
    first5 <- filterData %>% arrange(desc(DataValue)) %>% 
      head(5) %>% as.data.frame()
    
    # ggplot
    ggplot(first5, aes(x= reorder(LocationAbbr, -DataValue) , y=DataValue)) + 
      geom_bar(stat="identity" , fill="skyblue") +
      scale_fill_hue(name="Average") + 
      theme_classic() +
      xlab("State") +
      ylab(paste(cdc_data$DataValueType[1], " (", cdc_data$DataValueUnit, ")", sep = "")) + 
      geom_text(aes(label=round(DataValue,2)), vjust=-.5, size = 5) + 
      ggtitle("5 Highest States") + 
      theme(
        plot.title = element_text(size=20, face="bold", 
                                  margin = margin(10, 0, 10, 0)),
        axis.title = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14)
      )
    
  })
  
  # Bottom 5
  output$Bottom5 <- renderPlot({

    filterData <- var()
    
    # data
    last5 <- filterData %>% arrange(DataValue) %>%
      head(5) %>% as.data.frame()

    # ggplot
    ggplot(last5, aes(x= reorder(LocationAbbr, -DataValue) , y=DataValue)) + 
      geom_bar(stat="identity" , fill="skyblue") +
      scale_fill_hue(name="Average") + 
      theme_classic() +
      xlab("State") +
      ylab(paste(cdc_data$DataValueType[1], " (", cdc_data$DataValueUnit, ")", sep = "")) + 
      geom_text(aes(label=round(DataValue,2)), vjust=-.5, size = 5) + 
      ggtitle("5 Lowest States") + 
      theme(
        plot.title = element_text(size=20, face="bold", 
                                  margin = margin(10, 0, 10, 0)),
        axis.title = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14)
      )
  })
  
  # Heat map
  output$Map <- renderPlot({
    filterData <- var()
    mapData <- filterData %>% as.data.frame()
    
    # join map data with cdc data using SQLDF. Allows rendering of heat map.
    mapData <- sqldf('SELECT *
               FROM states
               LEFT JOIN mapData ON UPPER(states.region) = UPPER(mapData.LocationDesc)')
    
    # ggplot
    ggplot(mapData) + 
      geom_polygon(aes(x = long, y = lat, fill = DataValue, group = group), color = "white") + 
      coord_fixed(1.3) +
      scale_fill_distiller(palette="YlOrRd", trans="reverse") + 
      theme_classic() + #remove unnecessary grids and axis labels
      ggtitle("Heat Map for Selected Measure and Year Range") +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size=20, face="bold", 
                                  margin = margin(15, 0, 15, 0))
      )
  })

}

## Application execution
shinyApp( ui = ui , server = server)

