#' ---
#' title: "Predict Future Elk Harvests"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Initial Questions to Explore
#' * I'm wondering how many elk are harvested in each of the units, 
#' I would expect that CPW associates the number of Harvest to how many elk are in each unit.
#' * I am also curious to know if the number harvested has changed from year to year.
#' 
#' Will want to view all elk, antlered, antlerless, and the ratio of each for insights into these questions.
#'
#' *__NOTICE__ that I am only looking at the general rifle hunting seasons on public land. There are also 
#' hunters for Archery, Muzzleloader, Private Land, Ranching for Wildlife, etc.*
#' 
#' ***
#' ## Setup
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T) # data wrangling
library(dplyr,quietly = T) # data wrangling
library(ggplot2, quietly = T) # charting
library(ggthemes,quietly = T) # so I can add the highcharts theme and palette
library(scales,quietly = T) # to load the percent function when labeling plots
library(caret,quietly = T) # classification and regression training

#' Set our preferred charting theme
theme_set(theme_minimal()+theme_hc()+theme(legend.key.width = unit(1.5, "cm")))
#' Run script to get harvest data
#+ source_harvest, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)

#' Table of the harvest data
COElkRifleAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)


#' Weather data
load("weatherdata5.RData")
#' ***
#' ## Organize data
#' Group weatherdata by year
UnitWeather <- summarise(group_by(weatherdata5,Year,Unit),
                         daily.temperatureHigh = max(daily.temperatureHigh,na.rm = T),
                         daily.temperatureLow = min(daily.temperatureLow,na.rm = T),
                         daily.temperatureMean = mean(daily.temperatureMean,na.rm = T),
                         daily.precipAccumulation = mean(daily.precipAccumulation,na.rm = T),
                         daily.precipType = mean(daily.precipType,na.rm = T),
                         daily.windSpeed = mean(daily.windSpeed,na.rm = T),
                         daily.FullmoonPhase = mean(daily.FullmoonPhase,na.rm = T))

UnitWeather <- subset(UnitWeather, !is.na(daily.temperatureHigh))
# Group seasons
COElkHarvestStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                   Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T))
# Group Units
COElkHarvestStatewide <- summarise(group_by(COElkHarvestStatewide,Year),
                                   Harvest = sum(Harvest))

ggplot(COElkHarvestStatewide, aes(Year,Harvest)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(20000,35000)) +
  labs(title="Statewide Elk Harvest", caption="source: cpw.state.co.us")
