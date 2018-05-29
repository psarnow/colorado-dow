## server.r
library(ggplot2)
require(rCharts)
library(leaflet)
library(rgdal)
library(sp)
library(maptools)
library(magrittr)
require(RColorBrewer)
library(dplyr)
require(plyr)

gpclibPermit()

myPalette <- c("#d73027", "#fdae61", 
               "#fee090", "#ffffbf", "#e0f3f8",
               "#abd9e9", "#4575b4")
CO_Elk_Harvest <- read.csv(file = "data/CO_Elk_2010-2014yearsWPrefPts.csv")
#CO_Elk_Harvest <- read.csv(file = "data/CO_Elk_2010-2014years.csv")

CO_Elk_Harvest$Type <- as.character(CO_Elk_Harvest$Type)
CO_Elk_Harvest$Season <- as.character(CO_Elk_Harvest$Season)
CO_Elk_Harvest$GMUID <- as.integer(CO_Elk_Harvest$GMUID)
CO_Elk_Harvest$Year <- as.character(CO_Elk_Harvest$Year)

CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="First")] <- 1
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Second")] <- 2
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Third")] <- 3
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Fourth")] <- 4


# shapefile <- readShapeSpatial("data/BigGameGMUBoundaries03172015.shp", proj4string = CRS("+proj=utm +zone=13")) #old
shapefile <- rgdal::readOGR("data/BigGameGMUBoundaries03172015.shp")
shapefile$GMUID <- as.character(shapefile$GMUID)

# # converting utm to longlat
shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))

GMUIDclick <- NULL
print("data loaded")
print(head(CO_Elk_Harvest))

shinyServer(function(input, output) {
  
  data_to_chart2 <- reactive ({
                   GMUIDclick <- input$map_shape_click$id
                   print(paste('clicked',GMUIDclick))
                   if (input$prefpointsdisplay==TRUE) {prefpts <- input$prefpts[2]}
                   if (input$prefpointsdisplay==FALSE) {prefpts <- 50}
                   if (is.null(GMUIDclick)) {GMUIDclick <- "77"}
                   print(paste('clicked',GMUIDclick))
                   if (input$means == "Rifle") {
                     dplyr:::filter(CO_Elk_Harvest, 
                                    Type == input$means 
                                    & GMUID == GMUIDclick
                                    & Season_Type == "General"
                                    & Season != "All"
                                    & Year != "Last 5"
                                    & Year != "Last 3"
                                    & Residency == input$Residency
                                    & Sex == input$Sex
                                    & PrefPoints <= prefpts)
                   }
                   else {
                     print("get non rifle data")
                     dplyr:::filter(CO_Elk_Harvest, 
                                    Type == input$means 
                                    & GMUID == GMUIDclick
                                    & Year != "Last 5"
                                    & Year != "Last 3")
                   }
                   })

  filtered_data <- reactive ({
    if (input$prefpointsdisplay==TRUE) {prefpts <- input$prefpts[2]}
    if (input$prefpointsdisplay==FALSE) {prefpts <- 50}
    if (input$means == "Rifle") {
      print(paste('input',input))
      color_data <- dplyr:::filter(CO_Elk_Harvest, Type == input$means
                                   & Season_Type == "General"
                                   & Season == input$season
                                   & Year == input$year
                                   & Residency == input$Residency
                                   & Sex == input$Sex
                                   & PrefPoints <= prefpts)
    }
    else {
      color_data <- dplyr:::filter(CO_Elk_Harvest, Type == input$means
                                   & Year == input$year)
    }
    
    color_data <- color_data[,c("GMUID",input$var)]
    color_data$GMUID <- as.character(color_data$GMUID)
    color_data_merge <- left_join(shapefile@data,color_data, by=c("GMUID"))
    color_data <- color_data_merge[,c("GMUID",input$var)]
    
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  color_units_by <- reactive({
    print(filtered_data()[,2])
    maxcuts <- 5
    maxcuts1 <- 5
    while(maxcuts+1 >= maxcuts1) { 
    maxcuts1 <- n_distinct(quantile(filtered_data()[,2],probs = seq(0, 1, (1/maxcuts)),na.rm=TRUE))
    print(quantile(filtered_data()[,2],probs = seq(0, 1, (1/maxcuts)),na.rm=TRUE))
    maxcuts <- maxcuts - 1
    print("maxcuts")
    print(maxcuts)
    
    }
    print("maxcuts final")
    maxcuts1 <- maxcuts1 - 1
    print(maxcuts1)
    if(maxcuts1 >= 3 ) {color_units_by1 <- colorQuantile("RdYlBu", domain = filtered_data()[,2],n=maxcuts)}
    if(maxcuts1 < 3) {color_units_by1 <- colorBin("RdYlBu", domain = filtered_data()[,2],bins=3)}
    #color_units_by1 <- colorQuantile("RdYlBu", domain = filtered_data()[,2],n=maxcuts1)
    color_units_by1(filtered_data()[,2])
  })
  
  output$myChart <- renderChart({


    print(data_to_chart2())
    print("bulding barchart")
    h1 <- hPlot(x = "Season", y = input$var, 
            data = data_to_chart2(), 
            type = "column", 
            group = "Year",
            color = "Year")
    #h1$xAxis(text = 'Season')
    h1$title(text = paste("Unit",input$map_shape_click$id,"by Season",sep=" "))
    #h1$yAxis(text = input$value )
    #h1$chart(color = hcpalette)
    h1$addParams(dom = 'myChart')
    return(h1)

  })
 
  
  output$map <- renderLeaflet({

    print("building map")

    leaflet() %>% 
    addTiles(urlTemplate="http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png") %>%
    setView(-106,39.1438,zoom = 7) %>%
    #comment out if using the proxy observer  
    addPolygons(data = shapefile,
                  fillColor = ~color_units_by(),
                  popup = as.character(shapefile@data$GMUID),
                  layerId = as.character(shapefile@data$GMUID),
                  stroke = 1,
                  color = "grey",
                  smoothFactor = 2) %>%
      
#     addLegend(position = "bottomright", 
#               pal = colorQuantile("RdYlBu", domain = filtered_data()[,2],n=5),
#               values = filtered_data()[,2]
#               ) %>%
    mapOptions(zoomToLimits = "first")
    
  })

# would like to add an observer so I can just update the colors... but right now the entire
# polygons need to be redrawn, which is where the time suck is.
  
#   observe({
#     leafletProxy("map") %>%
#       clearShapes() %>%
#       addPolygons(data = shapefile,
#                   #fillColor = ~RdYlBu(shapefile@data[,input$var]),
#                   fillColor = ~color_units_by(),
#                   popup = as.character(shapefile@data$GMUID),
#                   layerId = as.character(shapefile@data$GMUID),
#                   stroke = 1,
#                   color = "grey",
#                   smoothFactor = 2)
#   })
 
#   observe({
#     proxy <- leafletProxy("map")
#     # Remove any existing legend, and only if the legend is
#     # enabled, create a new one.
#     proxy %>% clearControls()
#       pal <- colorQuantile("RdYlBu", domain = filtered_data()[,2],n=5)
# 
#       proxy %>% addLegend(position = "bottomright",
#                           pal = pal, values = filtered_data()[,2]
#       )
#     
#   })
  
})
