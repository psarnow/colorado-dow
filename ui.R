# ## ui.R

#library(ggplot2)
require(rCharts)
#library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  title = "Colorado DOW Elk Statistics",

  fluidRow(
    column(8,
           leafletOutput("map",height=600)
    ),
    column(4,
           br(),
           br(),
           showOutput("myChart", "highcharts")
    )
  ),
  fluidRow( 
    column(2,
           radioButtons(inputId="means", 
                        label = h3("Hunt by"),
                        choices = list("Rifle", "Archery", "Muzzleloader"), 
                        selected = "Rifle")
    ),
    column(2,
           checkboxInput(inputId="prefpointsdisplay", h3("Preference\nPts?"), value = FALSE),
           
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
           ),
    column(2,
           radioButtons("var",
                        label = h3("Stat"),
                        choices = c('Bulls','Cows','Calves','Harvest','Hunters','Success','Days'),
                        selected = "Success")
    ),
    column(2,
           radioButtons("year",
                        label = h3("Year"),
                        choices = c('Last 5','Last 3','2014','2013','2012','2011','2010'),
                        selected = "Last 5")
    ),
    column(2,
           radioButtons("season",
                        label = h3("Season"),
                        choices = c('1'=1,'2'=2,'3'=3,'4'=4),
                        selected = 2)
    )

  )
))

