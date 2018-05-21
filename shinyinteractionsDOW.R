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
CO_Elk_Harvest <- read.csv(file = "data/CO_Elk_2010-2014years.csv")

CO_Elk_Harvest$Type <- as.character(CO_Elk_Harvest$Type)
CO_Elk_Harvest$Season <- as.character(CO_Elk_Harvest$Season)
CO_Elk_Harvest$ids <- as.character(CO_Elk_Harvest$GMUID)
CO_Elk_Harvest$Year <- as.character(CO_Elk_Harvest$Year)

CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="First")] <- 1
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Second")] <- 2
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Third")] <- 3
CO_Elk_Harvest$Season[(CO_Elk_Harvest$Season=="Fourth")] <- 4

CO_Elk_Harvest$Harvest_X_Success <- CO_Elk_Harvest$Harvest * CO_Elk_Harvest$Success

# LOAD PREFERENCE PTS DATA
ElkPrefPts <- read.csv(file = "data/ElkPrefPts.csv")

ElkPrefPts$Type <- as.character(ElkPrefPts$Type)
ElkPrefPts$Season <- as.character(ElkPrefPts$Season)
ElkPrefPts$ids <- as.character(ElkPrefPts$GMUID)
ElkPrefPts$Year <- as.character(ElkPrefPts$Year)

ElkPrefPts$Season[(ElkPrefPts$Season=="First")] <- 1
ElkPrefPts$Season[(ElkPrefPts$Season=="Second")] <- 2
ElkPrefPts$Season[(ElkPrefPts$Season=="Third")] <- 3
ElkPrefPts$Season[(ElkPrefPts$Season=="Fourth")] <- 4

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

ui <-
  navbarPage(title="Where to Hunt - Colorado",
             tabPanel("Map",
                      fluidPage(
                        fluidRow(
                          column(8,
                                 leafletOutput("mymap",height=600)
                          ),
                          column(4,
                                 br(),
                                 br(),
                                 showOutput("myChart", "highcharts")
                          )
                        ),
                        fluidRow( 
                          column(2,
                                 #actionButton("clear", "clear"),
                                 actionButton("addGeojson", "Draw Hunt Units"),
                                 actionButton("clearGeojson", "Clear Hunt Units")
                          ),
                          column(2,
                                 radioButtons(inputId="means", 
                                              label = h4("Hunt by"),
                                              choices = list("Rifle", "Archery", "Muzzleloader"), 
                                              selected = "Rifle")
                          ),
                          column(2,
                                 #selectInput('setstyle', label = "Color a Unit Red!", choices = NULL,selected = NULL),
                                 #selectInput('removefeature', label = "Remove a Unit!", choices = NULL,selected = NULL),
                                 #selectInput('addfeature', label = "Add back a Unit!", choices = NULL,selected = NULL),
                                 radioButtons('colorBy',
                                             label= h4("Color by Selected Field"),
                                             choices=c("Success","Hunters","Bulls","Cows","Calves","Harvest","Days Hunted"="Days","Harvest * Success"="Harvest_X_Success","none"),
                                             selected = "Success")
                          ),
                          column(2,
                                 radioButtons('year',
                                             label= h4("Year"),
                                             choices = c('Last 5','Last 3','2014','2013','2012','2011','2010'),
                                             selected = "Last 5")
                          ),
                          column(2,
                                 # Display this only if Rifle is selected
                                 conditionalPanel(condition = "input.means == 'Rifle'",
                                                  radioButtons("season",
                                                               label = h4("Season"),
                                                               choices = c('1'=1,'2'=2,'3'=3,'4'=4),
                                                               selected = 2)
                                 )
                          ),
                          column(2,
                                 checkboxInput(inputId="prefpointsdisplay", h4("Preference\nPts?"), value = FALSE),
                                 
                                 # Display this only if the prefpoints is selected
                                 conditionalPanel(condition = "input.prefpointsdisplay == true",
                                                  radioButtons(inputId="Residency", 
                                                               label = "Residency",
                                                               choices = list("Resident", "Nonresident"), 
                                                               selected = "Resident"),
                                                  radioButtons(inputId="Sex", 
                                                               label = "Sex",
                                                               choices = list("Bull", "Cow","Either"), 
                                                               selected = "Bull"),
                                                  sliderInput(inputId="prefpts", label = h3("Display Pts"), min = 0, 
                                                              max = 23, value = c(0, 23))
                                 )
                          )
                        )
                      )
             )
  )
server <- function(input, output, session) {
  
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
    print('basemap start')
    leaflet() %>%
      addTiles(urlTemplate="http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png") %>%
      setView(-106,39.1438,zoom = 7)
    print('basemap complete')
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
  observeEvent(input$colorBy, {
  observeEvent(input$season, {
  observeEvent(input$year, {
  observeEvent(input$means, {
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
                                     & Year == input$year)
      }
      
      colorByData <- rescale(addedData$df[[input$colorBy]])
      #print(colorByData)
      
      quantileNum <- 5
      probs <- seq(0, 1, length.out = quantileNum + 1)
      bins <- quantile(colorByData, probs, na.rm = TRUE, names = FALSE)
      while (length(unique(bins)) != length(bins)) {
        quantileNum <- quantileNum - 1
        probs <- seq(0, 1, length.out = quantileNum + 1)
        bins <- quantile(colorByData, probs, na.rm = TRUE, names = FALSE)
      }
      pal <- colorQuantile("RdYlBu", domain = colorByData,n=quantileNum)

      addColorSetStyle <- function(featureId,color) {
        leafletProxy("mymap") %>%
          styleFeatureGeoJSON(layerId ='geojsonlayer', featureId = featureId,
                              style = sprintf('{"fillColor": "%s"}',pal(color)))
      }
      mapply(addColorSetStyle,addedData$df$ids,colorByData)
    }
  })
  })  
  })
  })
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
    #print(addedData$df$ids)
    leafletProxy("mymap") %>%
      addGeoJSON(geojson,layerId ='geojsonlayer',smoothFactor=1) 
    updateSelectizeInput(session, 'setstyle', choices = addedData$df$ids, server = TRUE)
    updateSelectizeInput(session, 'removefeature', choices = addedData$df$ids, server = TRUE)
    updateSelectizeInput(session, 'addfeature', choices = NULL, server = TRUE)
    print('addGeojson complete')
  })
  
  observeEvent(input$setstyle, {
    print('setstyle')
    print(input$setstyle)
    leafletProxy("mymap") %>%
      styleFeatureGeoJSON(layerId ='geojsonlayer', featureId = input$setstyle, style = '{"fillColor" :"red"}')
  })
  
  observeEvent(input$prefpts, {
    observeEvent(input$Sex, {
      observeEvent(input$Residency, {
        #determine which units are in the list that need to be removed
        if (input$prefpointsdisplay==TRUE) {prefpts <- input$prefpts[2]}
        if (input$prefpointsdisplay==FALSE) {prefpts <- 50}
        print('remove pref points')
        print(prefpts)
        RemoveIds <- dplyr:::filter(ElkPrefPts, Type == input$means
                                       & Season_Type == "General"
                                       & Season == input$season
                                       & Year == input$year
                                       & Residency == input$Residency
                                       & Sex == input$Sex
                                       & PrefPoints <= prefpts)
        RemoveIds <- unique(RemoveIds$ids)
        print(input$prefpointsdisplay)
        if(input$prefpointsdisplay==TRUE & length(RemoveIds>0)) {
          print('remove Units')
          print(RemoveIds)
          for (i in 1:length(RemoveIds)) {
            leafletProxy("mymap") %>%
              removeFeatureGeoJSON(layerId ='geojsonlayer', featureId = RemoveIds[i])
          }
#           leafletProxy("mymap") %>%
# #             removeFeatureGeoJSON(layerId ='geojsonlayer', featureId = input$removefeature)
#             removeFeatureGeoJSON(layerId ='geojsonlayer', featureId = RemoveIds)
          
#           if (length(addedData$df$ids) > 1) {
#             addedData$df <- addedData$df[-c(which(addedData$df$ids==input$removefeature)),]
#           }
#           removedData$df <- rbind(removedData$df,allUnits[which(allUnits$ids==input$removefeature),])
#           updateSelectizeInput(session, 'setstyle', choices = addedData$df$ids, server = TRUE, selected=NULL)
#           updateSelectizeInput(session, 'removefeature', choices = addedData$df$ids, server = TRUE, selected=NULL)
#           updateSelectizeInput(session, 'addfeature', choices = removedData$df$ids, server = TRUE, selected=NULL)
        }
      })
    })  
  })
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
                            style = list(weight=2,color="#555555", fillOpacity = .5))
    
  })
}
shinyApp(ui, server)

