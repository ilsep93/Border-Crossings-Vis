#References:
#https://shiny.rstudio.com/gallery/date-and-date-range.html

library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(magrittr)
library(leaflet)

#plotly
#add date frame


bc <- read.csv("Border_Crossing_Entry_Data.csv")
bc$Date <- mdy_hms(bc$Date)

#Data cleaning for location
bc$Location %<>%
   str_replace_all("POINT", "") %>%
   str_replace_all("[()]","") %>%
   str_replace(" ","")

bc %<>%
   mutate(long = as.numeric(str_extract(Location, "([^ ]+)")), #everything before first space
          lat = as.numeric(str_extract(Location, "[^ ]*$"))) #everything after first space

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizing US Border Crossings"),
   
   br(),
   
   h5("Author: Ilse Paniagua"),
   
   br(),
   

   
   # Define UI
   ui <- fluidPage(
      
      # Sidebar layout with input and output definitions 
      sidebarLayout(
         sidebarPanel(
            fluidRow(column=4,
                     # Inputs
                     # Select Border
                     selectInput(inputId = "PortName1", 
                                 label = "Choose First Port of Entry:",
                                 choices = bc$Port.Name %>% unique() %>% sort(), 
                                 selected = "San Ysidro"
                                 
                     ),
                     
                     selectInput(inputId = "PortName2", 
                                 label = "Choose Second Port of Entry:",
                                 choices = bc$Port.Name %>% unique() %>% sort(), 
                                 selected = "Nogales"
                                 
                     ),
                     
                     selectInput(inputId = "Measure", label="Choose Entry Method:",
                                 choices = bc$Measure %>% unique() %>% sort(),
                                 selected= "Pedestrians")
            ),
            
            dateRangeInput(inputId = "Date", label="Choose Date Range:",
                           start = "1996-01-01",
                           end = "2019-03-01")
         ),
         
         
         # Outputs
         column(8,  
                mainPanel(
                   position="left",
                   plotlyOutput('trendplot'),
                   leafletOutput("map")
                ))
      )
   ))

# Server
server <- function(input, output) {
   
   #Creating data
   newbc <- reactive({
      bc %>%
         filter(Port.Name==input$PortName1 | Port.Name==input$PortName2) %>%
         filter(Measure== input$Measure) %>%
         filter(Date >= input$Date[1] & Date <= input$Date[2])
      
   })
   
   # Building Plotly output
   output$trendplot <- renderPlotly({
      
      p <- ggplot(data = newbc(), aes(x=Date, y=Value)) +
         geom_line(aes(color=Port.Name)) +
         geom_smooth(method ='loess') +
         theme_light() +
         labs(title= paste0("Border Crossings in ", input$PortName1, " & ", input$PortName2),
               x="Date",
              y="Number of crossings",
              color="Port Name",
              caption="Source: Bureau of Transportation Statistics (BTS)")
      
      ggplotly(p)
      
   })
   
   #Adding map output
   output$map <- renderLeaflet({
         
      leaflet(unique(newbc())) %>%
         addTiles() %>%
         addMarkers(~long, ~lat, popup= as.character(c(input$PortName1, input$PortName2)))
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

