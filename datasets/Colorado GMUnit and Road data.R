#' ---
#' title: "Colorado Game Management Unit boundaries and Road Data"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Description
#' I am going to use maps to display data for each Unit. I will need their boundaries in
#' order to draw those maps.
#' Will also draw the major highways for geographical references.
#' 
#' ***
#' ## Setup
library(rgdal,quietly = T) #for reading/writing geo files
library(rgeos,quietly = T) #for simplification and true centroid
library(magrittr,quietly = T)
library(ggplot2,quietly = T) #for the fortify function
library(dplyr,quietly = T)

#' ***
#' ## Download Big Game GMU Boundaries from CPW
# https://hub.arcgis.com/items/93c485b0f27e4ff68c066a4b23cbaad3
#' I'm using a 2015 version, and I've saved it in the datasets directory
shapefile <- rgdal::readOGR("~/_code/colorado-dow/datasets/CPW_GMUBoundaries/BigGameGMUBoundaries03172015.shp")
shapefile$GMUID <- as.character(shapefile$GMUID)

# convert utm to longlat
shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))

#' ### Define unit boundaries
#' Will use when drawing maps
Unitboundaries <- shapefile %>% fortify(region = "GMUID")
Unitboundaries2 <- merge(Unitboundaries, shapefile@data, by.x = 'id', by.y = 'GMUID')
Unitboundaries2$Unit <- as.character(Unitboundaries2$id)

#' ### Calculate the centroid of each unit
#' Will use for labeling units, and looking up historical Unit weather
# use gCentroid in the rgeos package to calculate the true centroid
shapefile_centroids <- shapefile
shapefile_centroids@data$centroids <- as.data.frame(coordinates(shapefile_centroids))
shapefile_centroids2 <- shapefile_centroids %>% fortify(region = "GMUID")
shapefile_centroids2 <- merge(shapefile_centroids2, shapefile_centroids@data, by.x = 'id', by.y = 'GMUID')
shapefile_centroids2$Unit <- as.character(shapefile_centroids2$id)
shapefile_centroids2$longitude <- shapefile_centroids2$centroids$V1
shapefile_centroids2$latitude <- shapefile_centroids2$centroids$V2
data_centroids <- select(shapefile_centroids2,Unit, group,longitude,latitude)
data_centroids <- data_centroids[!duplicated(data_centroids$Unit),]
data_centroids <- select(data_centroids, longitude, latitude, group, Unit)
data_centroids$Unit <- as.character(data_centroids$Unit)
data_centroids

#' ***
#' ## Colorado Highways
#' Will display these on maps for visual references  
# Download a statemap with some roads on it (I have saved off into datasets directory)
roaddata <- rgdal::readOGR("~/_code/colorado-dow/datasets/ne_10m_roads/ne_10m_roads.shp")
USAroads <- roaddata %>% subset(.,sov_a3 == "USA" & type == "Major Highway")
# Convert to data frames so that I can use the data with ggplot2
USAroads <- fortify(USAroads)

# Get min/max of long/lat and zoom to Colorado
longset <- c(min(Unitboundaries2$long),max(Unitboundaries2$long))
latset <- c(min(Unitboundaries2$lat),max(Unitboundaries2$lat))
COroads <- filter(USAroads, long > longset[1] & long < longset[2])
COroads <- filter(COroads, lat > latset[1] & lat < latset[2])
COroads
