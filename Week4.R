library(jsonlite)
library(leaflet)
library(shiny)
library(maps)
#library(ionicons)
library(httr)
library(lubridate)
library(stringr)
library(plyr)
library(dplyr)
library(rsconnect)
library(shinydashboard)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Bike-counters in Brussels"),
  column(3,radioButtons("typeofdatatime", "Period:",
               list("Hour" = "period_hour",
                    "Day" = "period_day",
                    "Year" = "period_year"))),
  
  column(3,numericInput("num", 
               h3("Numeric input for radius proportion"), 
               value = 300)),
  
  column(5,h3("Help text"),
         helpText("This map gives the daily/monthly/yearly", 
                  "number of bkes passing through some bike counters",
                  "in the city of Brussels.",
                  "Use the buttons to select the time period you want",
                  "And the radius proportion to have bigger or smaller circles.")),
  
  
  
  mainPanel(leafletOutput("mymap",width = "145%",height = 700)),
  textOutput("selected_var"))

server <- function(input, output, session) {
  
  
  
  base_bxl <- "https://data-mobility.brussels/bike/api/counts/?request=live"
  devices_bxl <- "https://data-mobility.brussels/bike/api/counts/?request=devices"
  
  call_live_bxl <- paste(base_bxl)
  
  Bikes_data_bxl <- GET(call_live_bxl)
  
  Bikes_data_df_bxl <- as.data.frame(
    fromJSON(
      content(Bikes_data_bxl, "text"), 
      flatten = TRUE))
  
  final_bxl <- cbind(
    as.data.frame(
      colnames(
        Bikes_data_df_bxl)),
    t(Bikes_data_df_bxl)[,1])
  
  
  rownames(final_bxl) <- c()
  
  extraction_date <- as_datetime(final_bxl$`t(Bikes_data_df_bxl)[, 1]`[1])
  
  final_bxl <- final_bxl[-1,]
  
  final_bxl$features.properties.device_name <- str_split_fixed(
    as.character(final_bxl$`colnames(Bikes_data_df_bxl)`),"[.]",3)[,2]
  
  final_bxl$info <- str_split_fixed(
    as.character(final_bxl$`colnames(Bikes_data_df_bxl)`),"[.]",3)[,3]
  
  names(final_bxl)[2] <- "Count"
  keeps <- c("features.properties.device_name",
             "info",
             "Count")
  final_bxl <- final_bxl[keeps]
  final_bxl$info <- revalue(final_bxl$info, c("hour_cnt"="Hour count",
                                              "day_cnt"="Day count",
                                              "year_cnt"="Year count",
                                              "cnt_time"="Time"))
  
  call_devices_bxl <- paste(devices_bxl)
  Bikes_devices_bxl <- GET(call_devices_bxl)
  #Bikes_data_text <- content(Bikes_data, "text")
  #Bikes_data_json <- fromJSON(Bikes_data_text, flatten = TRUE)
  Bikes_devices_df_bxl <- as.data.frame(fromJSON(content(Bikes_devices_bxl, "text"), flatten = TRUE))
  
  Bikes_devices_df_bxl$lat <- sapply(Bikes_devices_df_bxl$features.geometry.coordinates, "[[", 2)
  Bikes_devices_df_bxl$long <- sapply(Bikes_devices_df_bxl$features.geometry.coordinates, "[[", 1)
  
  keeps <- c("requestDate",
             "features.properties.device_name",
             "features.properties.road_nl",
             "features.properties.road_fr", 
             "features.properties.road_en",
             "lat",
             "long")
  Bikes_devices_df_bxl <- Bikes_devices_df_bxl[keeps]
  
  FINAL <- merge(final_bxl,Bikes_devices_df_bxl, by = "features.properties.device_name")
  
  Longitud <- FINAL$long[c(seq(1,length(FINAL[,1])-3,by=4))]
  Latitud  <- FINAL$lat[c(seq(1,length(FINAL[,1])-3,by=4))]
  
  
  output$mymap <- renderLeaflet({
    
    if (input$typeofdatatime == "period_hour"){
      
      Contentv2 <- lapply(seq(1,length(FINAL[,1])-3,by=4), function(i) {
        paste0( '<p>','<B>', FINAL[i, "features.properties.road_en"],'</B>', '<p></p>', 
                FINAL[i, "info"], ': ', 
                FINAL[i, "Count"], '<p></p>',
                FINAL[i+3, "Count"],'</p>') 
      })
      
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircles(lng = Longitud, 
                   lat = Latitud, 
                   weight = 2,
                   radius = (as.numeric(as.character(FINAL[c(seq(1,length(FINAL[,1])-3,by=4)), "Count"]))/max(as.numeric(as.character(FINAL[c(seq(1,length(FINAL[,1])-3,by=4)), "Count"]))))*input$num, 
                   popup = lapply(Contentv2, htmltools::HTML)) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
    } else if (input$typeofdatatime == "period_day"){
      Contentv3 <- lapply(seq(1,length(FINAL[,1])-3,by=4), function(i) {
        paste0( '<p>','<B>', FINAL[i, "features.properties.road_en"],'</B>', '<p></p>', 
                FINAL[i+1, "info"], ': ', 
                FINAL[i+1, "Count"], '<p></p>',
                FINAL[i+3, "Count"],'</p>') 
      })
      
      
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircles(lng = Longitud, 
                   lat = Latitud, 
                   weight = 2,
                   radius = (as.numeric(as.character(FINAL[c(1+seq(1,length(FINAL[,1])-3,by=4)), "Count"]))/max(as.numeric(as.character(FINAL[c(1+seq(1,length(FINAL[,1])-3,by=4)), "Count"]))))*input$num, 
                   popup = lapply(Contentv3, htmltools::HTML)) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      
    } else if (input$typeofdatatime == "period_year") {
      Contentv4 <- lapply(seq(1,length(FINAL[,1])-3,by=4), function(i) {
        paste0( '<p>','<B>', FINAL[i, "features.properties.road_en"],'</B>', '<p></p>', 
                FINAL[i+2, "info"], ': ', 
                FINAL[i+2, "Count"], '<p></p>',
                FINAL[i+3, "Count"],'</p>') 
      })
      
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addCircles(lng = Longitud, 
                   lat = Latitud, 
                   weight = 2,
                   radius = (as.numeric(as.character(FINAL[c(2+seq(1,length(FINAL[,1])-3,by=4)), "Count"]))/max(as.numeric(as.character(FINAL[c(2+seq(1,length(FINAL[,1])-3,by=4)), "Count"]))))*input$num, 
                   popup = lapply(Contentv4, htmltools::HTML)) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      
    }
    
  })
}



shinyApp(ui, server)
