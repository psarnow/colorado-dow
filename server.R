#server.R
#devtools::install_github('ButlerFirm/leaflet')
library(shiny)
library(leaflet)
library(rgdal)
library(jsonlite)
library(scales)
library(dplyr)
require(rCharts)

# googleLink <- function(lat,lng,zoom) {
#   sprintf("http://www.google.com/maps/place/%s,%s/@%s,%s,%sz/data=!3m1!1e3",
#           lat, lng, lat, lng, zoom)
# }
#CO_Elk_Harvest <- read.csv(file = "data/CO_Elk_2010-2014yearsWPrefPts.csv")
#CO_Elk_Harvest <- read.csv(file = "data/CO_Elk_2010-2014years.csv")
CO_Elk_Harvest <- read.csv(file = "data/CO_Elk_Harvest_Predict2015.csv")

CO_Elk_Harvest$Type <- as.character(CO_Elk_Harvest$Type)
CO_Elk_Harvest$Season <- as.character(CO_Elk_Harvest$Season)
CO_Elk_Harvest$ids <- as.character(CO_Elk_Harvest$GMUID)
CO_Elk_Harvest$Year <- as.character(CO_Elk_Harvest$Year)

CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="First")] <- 1
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Second")] <- 2
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Third")] <- 3
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Fourth")] <- 4

CO_Elk_Harvest$Harvest_X_Success <- CO_Elk_Harvest$Harvest * CO_Elk_Harvest$Success

geojson <- fromJSON("data/DOW.geojson",simplifyVector = FALSE) # 28sec to draw
#geojson <- fromJSON("CO_Boundaries_forGEOJSON_5.geojson",simplifyVector = FALSE) # 18secs to draw, too simple
#geojson <- fromJSON("CO_Boundaries_forGEOJSON_1.geojson",simplifyVector = FALSE) # 22secs to draw, prob too simole

geojson$style = list(
  weight = 2,
  color = "#555555",
  opacity = 1,
  fillColor = "none",
  fillOpacity = 0.5
)

geojson$features <- lapply(geojson$features, function(feat) {
  feat$id <- as.character(feat$properties$GMUID) # Must set ids
  feat
})

ids <- as.character(sapply(geojson$features,function(x) x$id))
allUnits <- data.frame(ids = ids, stringsAsFactors = FALSE)
allUnits <- left_join(allUnits,CO_Elk_Harvest,by=c("ids"))
# note id specified in GeoJSON on highest level (of single feature) for use in removeFeatureGeoJSON and styleFeatureGeoJSON

instructions <- "Drawing Hunting Units<br/>This will take 30-60 seconds<br/><br/>
                 Use this tool to help plan your next season of elk hunting.
                 The hunting units can be colored by the inputs you select at
                 the bottom of the page. You can also click a unit after it is
                 colored to chart the historical data for that unit."

shinyServer(function(input, output, session) {
  
  data_to_chart2 <- reactive ({
    #GMUIDclick <- input$map_shape_click$id
    GMUIDclick <- input$mymap_geojson_click$featureId
    if (is.null(GMUIDclick)) {GMUIDclick <- "77"}
    print(GMUIDclick)
    if (input$means == "Rifle") {
      dplyr:::filter(CO_Elk_Harvest, 
                     Type == input$means 
                     & GMUID == GMUIDclick
                     & Season_Type == "General"
                     & Season != "All"
                     & Year != "Last 5"
                     & Year != "Last 3")
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
  
  output$mymap <- renderLeaflet({
    print('basemap')
    leaflet() %>%
      addTiles(urlTemplate="http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png") %>%
      setView(-106,39.1438,zoom = 7) %>%
      addPopups(-105.327298, 38.597131, instructions,
                options = popupOptions(closeButton = FALSE)
      ) 
  })
  
  output$myChart <- renderChart({
    #print(data_to_chart2())
    print("bulding barchart")
    
    h1 <- hPlot(x = "Season", y = input$colorBy, 
                data = data_to_chart2(), 
                type = "column", 
                group = "Year",
                color = "Year")
    #h1$xAxis(text = 'Season')
    h1$title(text = paste("Unit",input$mymap_geojson_click$featureId,"by Season",sep=" "))
    #h1$yAxis(text = input$value )
    #h1$chart(color = hcpalette)
    h1$addParams(dom = 'myChart')
    return(h1)
  })
  
  addedData <- reactiveValues()
  addedData$df <- allUnits
  removedData <- reactiveValues()
  #removedData$ids <- data.frame(ids = as.character(), gdp_md_est = as.numeric(), pop_est = as.numeric(), stringsAsFactors = FALSE)
  
  #this really needs to be a reactive function
  #reactive ({

          
color_map <- reactive ({
          if (input$colorBy == "none") {
            print('nocolor')
            for (i in 1:length(addedData$df$ids)) {
              leafletProxy("mymap") %>%
                styleFeatureGeoJSON(layerId ='geojsonlayer', featureId = addedData$df$ids[i],
                                    style = sprintf('{"fillColor": "%s"}',"none"))
            }
          } else {
            print('coloring')
            
            if (input$means == "Rifle") {
              addedData$df <- dplyr:::filter(CO_Elk_Harvest, Type == input$means
                                             & Season_Type == "General"
                                             & Season == input$season
                                             & Year == input$year)
            }
            else {
              addedData$df <- dplyr:::filter(CO_Elk_Harvest, Type == input$means
                                             & Year == input$year2)
            }
            
            #color_data <- color_data[,c("GMUID",input$var)]
#             colorByData <- rescale(addedData$df[[input$colorBy]])
            colorByData <- addedData$df[[input$colorBy]]
#             colnames(colorByData) <- c("statcolor")
#             print(names(colorByData))
            print(colorByData)
            quantileNum <- 5
            probs <- seq(0, 1, length.out = quantileNum + 1)
            bins <- quantile(colorByData, probs, na.rm = TRUE, names = FALSE)
            while (length(unique(bins)) != length(bins)) {
              quantileNum <- quantileNum - 1
              probs <- seq(0, 1, length.out = quantileNum + 1)
              bins <- quantile(colorByData, probs, na.rm = TRUE, names = FALSE)
            }
            palcolors <- colorQuantile("RdYlBu", domain = colorByData,n=quantileNum)
            #clear previous legend
            leafletProxy("mymap") %>% clearControls()
            # Add a legend
            leafletProxy("mymap") %>% addLegend(position = "bottomright",
                                    pal=palcolors, 
                                    values=colorByData, 
                                    labFormat = function(type, cuts, p) {
                                                n = length(cuts)
                                                p = paste0(round(p * 100), '%')
                                                cuts = paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]))
                                                # mouse over the legend labels to see the percentile ranges
                                                paste0('<span title="', p[-n], " - ", p[-1], '">', cuts,'</span>')
                                                }
                                    )

            addColorSetStyle <- function(featureId,color) {
              leafletProxy("mymap") %>% 
                styleFeatureGeoJSON(layerId ='geojsonlayer', featureId = featureId,
                                    style = sprintf('{"fillColor": "%s"}',palcolors(color)))
            }
            mapply(addColorSetStyle,addedData$df$ids,colorByData)
          }
})

observeEvent(input$colorBy, {
  observeEvent(input$season, {
    observeEvent(input$year, {
      observeEvent(input$year2, {
        observeEvent(input$means,color_map()
        )
      })
    })
  })
})
                   
#                    
# isolate(observeEvent(input$colorBy, color_map()))
# isolate(observeEvent(input$season, color_map()))

#if(!is.null(color_map()) {observeEvent(input$season, color_map())
# observeEvent(input$year, color_map())
# observeEvent(input$means, color_map())

  #   observeEvent(input$mymap_geojson_click, {
  #     if (input$popupAll==FALSE) {
  #       content <- as.character(tagList(
  #         tags$strong(paste0("GeoJSON ID: ",input$mymap_geojson_click$properties$GMUID)),
  #         tags$a(target="_blank",href=googleLink(input$mymap_geojson_click$lat, input$mymap_geojson_click$lng,input$mymap_zoom),"Google Maps")
  #       ))
  #       leafletProxy("mymap") %>% clearPopups()
  #       leafletProxy("mymap") %>% addPopups(input$mymap_geojson_click$lng, input$mymap_geojson_click$lat, content)
  #     }
  #   })
  observeEvent(input$addGeojson, {
    print('start addGeojson')
    #leafletProxy("mymap") %>% clearPopups()

    leafletProxy("mymap") %>%
      addGeoJSON(geojson,layerId ='geojsonlayer',smoothFactor=2) 
    updateSelectizeInput(session, 'setstyle', choices = addedData$df$ids, server = TRUE)
    updateSelectizeInput(session, 'removefeature', choices = addedData$df$ids, server = TRUE)
    updateSelectizeInput(session, 'addfeature', choices = NULL, server = TRUE)
    print('addGeojson complete')
    leafletProxy("mymap") %>% clearPopups()
  })
  observeEvent(input$setstyle, {
    print('setstyle')
    print(input$setstyle)
    leafletProxy("mymap") %>%
      styleFeatureGeoJSON(layerId ='geojsonlayer', featureId = input$setstyle, style = '{"fillColor" :"red"}')
  })
  #   observeEvent(input$removefeature, {
  #     if(is.null(input$removefeature)==FALSE && input$removefeature != "") {
  #       leafletProxy("mymap") %>%
  #         removeFeatureGeoJSON(layerId ='geojsonlayer', featureId = input$removefeature)
  #       if (length(addedData$df$ids) > 1) {
  #         addedData$df <- addedData$df[-c(which(addedData$df$ids==input$removefeature)),]
  #       }
  #       removedData$df <- rbind(removedData$df,allUnits[which(allUnits$ids==input$removefeature),])
  #       updateSelectizeInput(session, 'setstyle', choices = addedData$df$ids, server = TRUE, selected=NULL)
  #       updateSelectizeInput(session, 'removefeature', choices = addedData$df$ids, server = TRUE, selected=NULL)
  #       updateSelectizeInput(session, 'addfeature', choices = removedData$df$ids, server = TRUE, selected=NULL)
  #     }
  #   })
  #   observeEvent(input$addfeature, {
  #     if(is.null(input$addfeature)==FALSE && input$addfeature != "") {
  #       geojson <- geojson$features[[seq_along(geojson$features)[sapply(geojson$features,
  #                                                                       FUN = function(x) x[["id"]] == input$addfeature)]]]
  #       leafletProxy("mymap") %>%
  #         addFeatureGeoJSON(geojson, layerId ='geojsonlayer') # can use a list (slow)
  #       if (length(addedData$df$ids) > 1) {
  #         removedData$df <- removedData$df[-c(which(removedData$df$ids==input$addfeature)),]
  #       }
  #       addedData$df <- rbind(addedData$df,allUnits[which(allUnits$ids==input$addfeature),])
  #       updateSelectizeInput(session, 'setstyle', choices = addedData$df$ids, server = TRUE, selected=NULL)
  #       updateSelectizeInput(session, 'removefeature', choices = addedData$df$ids, server = TRUE, selected=NULL)
  #       updateSelectizeInput(session, 'addfeature', choices = removedData$df$ids, server = TRUE, selected=NULL)
  #     }
  #   })
  observeEvent(input$clearGeojson, {
    leafletProxy("mymap") %>% removeGeoJSON(layerId ='geojsonlayer')
  })
  #   observeEvent(input$clear, {
  #     leafletProxy("mymap") %>% clearTiles()
  #   })
  observeEvent(input$mymap_geojson_mouseover, {
    leafletProxy("mymap") %>%
      styleFeatureGeoJSON(layerId ='geojsonlayer', featureId = input$mymap_geojson_mouseover$featureId,
                          style = list(weight=3,color="orange", fillOpacity = .9)) # or string
    #     leafletProxy("mymap") %>% clearPopups()
    #     leafletProxy("mymap") %>% addPopups(input$mymap_geojson_mouseover$lng, 
    #                                         input$mymap_geojson_mouseover$lat,
    #                                         input$mymap_geojson_mouseover$featureId)
  })
  observeEvent(input$mymap_geojson_mouseout, {
    leafletProxy("mymap") %>%
      styleFeatureGeoJSON(layerId ='geojsonlayer', featureId = input$mymap_geojson_mouseout$featureId,
                          #                           style = '{"weight": 2, "color": "#555555"}') # or string
                          style = list(weight=2,color="#555555", fillOpacity = .5))
    
  })
}
)