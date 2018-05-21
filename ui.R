# ## ui.R

#library(ggplot2)
require(rCharts)
#library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  navbarPage(title="Where to Plan Your Next Hunt - Colorado",
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
                          column(1,
                                 #actionButton("addGeojson", "Draw Hunt Units",selected=TRUE),
                                 conditionalPanel(condition = "input.means == 'Test'",
                                                 checkboxInput(inputId="addGeojson", "Draw Hunt Units", value = TRUE),
                                                 actionButton("clearGeojson", "Clear Hunt Units")
                                 )
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
                                              label= h4("Color Map by"),
                                              choices=c("Success","Hunters","Bulls","Cows","Calves","Harvest","Days Hunted"="Days","Harvest * Success"="Harvest_X_Success","none"),
                                              selected = "None")
                          ),
                          column(2,
                                 conditionalPanel(condition = "input.means == 'Rifle'",
                                                   radioButtons('year',
                                                                label= h4("Year"),
                                                                choices = c('Predicted 2015'='2015','Last 5','Last 3','2014','2013','2012','2011','2010'),
                                                                selected = "Last 5")
                                 ),
                                 conditionalPanel(condition = "input.means != 'Rifle'",
                                                  radioButtons('year2',
                                                               label= h4("Year"),
                                                               choices = c('2014','2013','2012','2011','2010'),
                                                               selected = '2014')
                                 )
                          ),
                          column(2,
                                 # Display this only if Rifle is selected
                                 conditionalPanel(condition = "input.means == 'Rifle'",
                                                  radioButtons("season",
                                                               label = h4("Season"),
                                                               choices = c('1'=1,'2'=2,'3'=3,'4'=4),
                                                               selected = 2)
                                 )
                          )
                        )
                      )
             )
  )
))

