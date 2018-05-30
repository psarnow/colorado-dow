# setwd("/Users/psarnow/Dropbox/DOW/CPW_GMUBoundaries/")
# prettier chart formatting
defaulttheme <- theme(
  axis.text=element_text(colour="#606060",size=20, family="Muli-Regular"),
  plot.title=element_text(hjust = 0.5,size=30, colour="#333333", family="Muli-Bold"),
  panel.grid.major = element_line(colour = "#d8d8d8"),
  panel.background = element_rect(fill="#ffffff"),
  plot.background = element_rect(fill = "#ffffff"),
  axis.title=element_text(size=20, colour="#707070", family="Muli-Regular"),
  axis.title.x=element_text(vjust=-.3),
  legend.text=element_text(color="#333333",size=20, family="Muli-Regular"),
  legend.background = element_rect(fill='#ffffff'),
  legend.direction = "horizontal", 
  legend.position = "top",
  legend.key = element_rect(fill='#ffffff',colour='#ffffff'),
  panel.grid.minor= element_blank(),
  strip.text = element_text(size = 20,family="Muli-Regular", colour="#333333"),
  strip.background=element_rect(fill="#ffffff", colour="#ffffff")
)
# palette from highcharts
hcpalette <- c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9', '#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1')

library(maptools)
library(gpclib)
library(sp)
library(ggplot2)
library(ggmap)
library(rgdal)
require(plyr)
library(dplyr)
library(magrittr)
require(scales)
require(RColorBrewer)

NCPalette <- c("#643165", "#8FC44E", "#F29632", "#BE8CBE", "#BAD892", "#EDAE68", "#BE8CBE", "#342D6C", "#BE8C44", "#BAD892")
hcpalette <- c("#7cb5ec", "#434348","#beaed4","#fdc086","#90ed7d")
# require(devtools)
# install_github('rCharts', 'ramnathv')

gpclibPermit()

# read harvest data
source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)

# COElkRifleAll
COElkRifleAll$season <- as.character(COElkRifleAll$season)
COElkRifleAll$Unit <- as.character(COElkRifleAll$Unit)
COElkRifleAll$Success <- as.numeric(COElkRifleAll$Success)

ElkHarvestRifle2014 <- filter(COElkRifleAll, year == 2014)

# shapefile <- readShapeSpatial("~/_code/colorado-dow/datasets/CPW_GMUBoundaries/BigGameGMUBoundaries03172015.shp", proj4string = CRS("+proj=utm +zone=13")) # original
# shapefile <- sf::st_read("~/_code/colorado-dow/datasets/CPW_GMUBoundaries/BigGameGMUBoundaries03172015.shp", proj4string = CRS("+proj=utm +zone=13"))
shapefile <- rgdal::readOGR("~/_code/colorado-dow/datasets/CPW_GMUBoundaries/BigGameGMUBoundaries03172015.shp")

# converting utm to longlat
shapefile2 <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
######
# Centroids of GMUID
# use gCentroid in the rgeos package if you want to label at the true centroid
shapefile_centroids <- shapefile2
shapefile_centroids@data$centroids <- as.data.frame(coordinates(shapefile_centroids))
shapefile_centroids2 <- shapefile_centroids %>% fortify(region = "GMUID")

shapefile_centroids2 <- merge(shapefile_centroids2, shapefile_centroids@data, by.x = 'id', by.y = 'GMUID')
shapefile_centroids2$Unit <- as.character(shapefile_centroids2$id)
shapefile_centroids2$longitude <- shapefile_centroids2$centroids$V1
shapefile_centroids2$latitude <- shapefile_centroids2$centroids$V2
data_centroids <- select(shapefile_centroids2,Unit, group,longitude,latitude)
data_centroids <- data_centroids[!duplicated(data_centroids$Unit),]
######
### Step 1: shapefile becomes SpatialLinesDataFrame.
# roaddata <- readShapeLines("~/_code/colorado-dow/datasets/ne_10m_roads/ne_10m_roads.shp")
roaddata <- rgdal::readOGR("~/_code/colorado-dow/datasets/ne_10m_roads/ne_10m_roads.shp")

### Step 2: If necessary, subset data before I use fortify().
### dplyr does not work with SpatialLinesDataFrame at this point.
# COroads <- roaddata %>% subset(.,sov_a3 == "USA" & type == "Major Highway")
# ### Step 3: I need to convert foo to data frame so that I can use the data
# ### with ggplot2.
# COroads1 <- fortify(COroads)
# 
# # get min/max of long/lat for zooming
# # longset <- c(min(data3$long),max(data3$long))
# latset <- c(min(COroads1$lat),max(COroads1$lat))
# longset <- c(min(COroads1$long),max(COroads1$long))
# 
# COroads2 <- filter(COroads1, long > longset[1] & long < longset[2])
# COroads2 <- filter(COroads2, lat > latset[1] & lat < latset[2])
#######
# data <- shapefile2 %>% fortify(region = "COUNTY")
data <- shapefile2 %>% fortify(region = "GMUID")

data2 <- merge(data, shapefile2@data, by.x = 'id', by.y = 'GMUID')
data2$Unit <- as.character(data2$id)

#determine breaks for all of 2014 charts
# RAll <- filter(ElkHarvestRifleAll, Season == "All" & Type == "Rifle" & Year == 2014)
RAll <- ElkHarvestRifle2014
#data3 <- left_join(data2,RAll, by=c("GMUID"))
fill_field_percentiles <- quantile(RAll$Success, c(.125, .25, .375, .5, .625, .75, .875),na.rm=TRUE) 
breaks <- as.vector(c(0,fill_field_percentiles,max(RAll$Success,na.rm=TRUE)))
fill_field_max <- max(RAll$Success,na.rm=TRUE)
fill_field_min <- min(RAll$Success,na.rm=TRUE)
plot_limits <- c(fill_field_min, fill_field_max)



# Season Charts
#filter season
R1 <- filter(ElkHarvestRifle2014, season == "1")
data3 <- left_join(data2,R1, by=c("Unit"))

## zoom in roadmap to just colorado
# COroads <- roaddata %>% subset(.,sov_a3 == "USA" & type == "Major Highway")
COroads <- roaddata %>% subset(.,sov_a3 == "USA" & type == "Major Highway")

### Step 3: I need to convert to data frame so that I can use the data
### with ggplot2.
COroads1 <- fortify(COroads)

# get min/max of long/lat for zooming
latset <- c(min(data3$lat),max(data3$lat))
longset <- c(min(data3$long),max(data3$long))

COroads2 <- filter(COroads1, long > longset[1] & long < longset[2])
COroads2 <- filter(COroads2, lat > latset[1] & lat < latset[2])

fill_field <- "Success"
data4 <- data3[,c("id","long","lat","order","hole","piece","group","Unit","season",fill_field)]
colnames(data4) <- c("id","long","lat","order","hole","piece","group","Unit","season","fill_field")

data4$fill_field[(data4$fill_field > breaks[8])] <- fill_field_max
data4$fill_field[(breaks[7] < data4$fill_field) & (data4$fill_field < breaks[8])] <- breaks[8]
data4$fill_field[(breaks[6] < data4$fill_field) & (data4$fill_field < breaks[7])] <- breaks[7]
data4$fill_field[(breaks[5] < data4$fill_field) & (data4$fill_field < breaks[6])] <- breaks[6]
data4$fill_field[(breaks[4] < data4$fill_field) & (data4$fill_field < breaks[5])] <- breaks[5]
data4$fill_field[(breaks[3] < data4$fill_field) & (data4$fill_field < breaks[4])] <- breaks[4]
data4$fill_field[(breaks[2] < data4$fill_field) & (data4$fill_field < breaks[3])] <- breaks[3]
data4$fill_field[(breaks[1] < data4$fill_field) & (data4$fill_field < breaks[2])] <- breaks[2]

myPalette <- c("#d73027", "#f46d43", "#fdae61", 
               "#fee090", "#ffffbf", "#e0f3f8",
               "#abd9e9", "#74add1", "#4575b4")

plot_R1 <- ggplot(data4, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = fill_field),colour = "black", size = .1) +
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) +
  geom_path(data = COroads2,aes(x = long, y = lat, group = group), color="#3878C7",size=2) +
  #facet_grid(. ~ Season) +
  scale_fill_gradientn(limits = plot_limits,
                       values = rescale(breaks),
                       breaks = breaks, 
                       colours = myPalette, 
                       guide = "legend", 
                       name = fill_field,
                       na.value = "grey") +
  xlab("") + 
  ylab("") +
  # theme(axis.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  # theme(axis.title.x=element_text()) +
  # theme(axis.text.x=element_text(size=0, family="ApexRounded-Medium")) +
  # theme(axis.text.y=element_text(size=0, family="ApexRounded-Medium")) +
  # theme(plot.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  # theme(legend.text=element_text(size=20, family="ApexRounded-Medium")) +
  # theme(legend.title=element_text(size=20, colour="#434348", family="ApexRounded-Medium")) +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_line(color='white')) +
  theme(panel.grid.minor= element_line(color='white')) +
  ggtitle("First Season Success")




####################################################################################################################




#filter season
R2 <- filter(ElkHarvestRifle2014, Season == "Second")
data3 <- left_join(data2,R2, by=c("GMUID"))

fill_field <- "Success"
data4 <- data3[,c("id","long","lat","order","hole","piece","group","GMUID","Season",fill_field)]
colnames(data4) <- c("id","long","lat","order","hole","piece","group","GMUID","Season","fill_field")

data4$fill_field[(data4$fill_field > breaks[8])] <- fill_field_max
data4$fill_field[(breaks[7] < data4$fill_field) & (data4$fill_field < breaks[8])] <- breaks[8]
data4$fill_field[(breaks[6] < data4$fill_field) & (data4$fill_field < breaks[7])] <- breaks[7]
data4$fill_field[(breaks[5] < data4$fill_field) & (data4$fill_field < breaks[6])] <- breaks[6]
data4$fill_field[(breaks[4] < data4$fill_field) & (data4$fill_field < breaks[5])] <- breaks[5]
data4$fill_field[(breaks[3] < data4$fill_field) & (data4$fill_field < breaks[4])] <- breaks[4]
data4$fill_field[(breaks[2] < data4$fill_field) & (data4$fill_field < breaks[3])] <- breaks[3]
data4$fill_field[(breaks[1] < data4$fill_field) & (data4$fill_field < breaks[2])] <- breaks[2]

plot_R2 <- ggplot(data4, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = fill_field),colour = "black", size = .1) +
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = GMUID),size=3) +
  geom_path(data = COroads2,aes(x = long, y = lat, group = group), color="#3878C7",size=2) +
  #facet_grid(. ~ Season) +
  scale_fill_gradientn(limits = plot_limits,
                       values = rescale(breaks),
                       breaks = breaks, 
                       colours = myPalette, 
                       guide = "legend", 
                       name = fill_field,
                       na.value = "grey") +
  #scale_fill_gradient(low="red",high = "blue") +
  xlab("") + 
  ylab("") +
  theme(axis.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  theme(axis.title.x=element_text()) +
  theme(axis.text.x=element_text(size=0, family="ApexRounded-Medium")) +
  theme(axis.text.y=element_text(size=0, family="ApexRounded-Medium")) +
  theme(plot.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  theme(legend.text=element_text(size=20, family="ApexRounded-Medium")) +
  theme(legend.title=element_text(size=20, colour="#434348", family="ApexRounded-Medium")) +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_line(color='white')) +
  theme(panel.grid.minor= element_line(color='white')) +
  ggtitle("Second Season")


#filter season
R3 <- filter(ElkHarvestRifle2014, Season == "Third")
data3 <- left_join(data2,R3, by=c("GMUID"))

fill_field <- "Success"
data4 <- data3[,c("id","long","lat","order","hole","piece","group","GMUID","Season",fill_field)]
colnames(data4) <- c("id","long","lat","order","hole","piece","group","GMUID","Season","fill_field")

data4$fill_field[(data4$fill_field > breaks[8])] <- fill_field_max
data4$fill_field[(breaks[7] < data4$fill_field) & (data4$fill_field < breaks[8])] <- breaks[8]
data4$fill_field[(breaks[6] < data4$fill_field) & (data4$fill_field < breaks[7])] <- breaks[7]
data4$fill_field[(breaks[5] < data4$fill_field) & (data4$fill_field < breaks[6])] <- breaks[6]
data4$fill_field[(breaks[4] < data4$fill_field) & (data4$fill_field < breaks[5])] <- breaks[5]
data4$fill_field[(breaks[3] < data4$fill_field) & (data4$fill_field < breaks[4])] <- breaks[4]
data4$fill_field[(breaks[2] < data4$fill_field) & (data4$fill_field < breaks[3])] <- breaks[3]
data4$fill_field[(breaks[1] < data4$fill_field) & (data4$fill_field < breaks[2])] <- breaks[2]

plot_R3 <- ggplot(data4, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = fill_field),colour = "black", size = .1) +
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = GMUID),size=3) +
  geom_path(data = COroads2,aes(x = long, y = lat, group = group), color="#3878C7",size=2) +
  #facet_grid(. ~ Season) +
  scale_fill_gradientn(limits = plot_limits,
                       values = rescale(breaks),
                       breaks = breaks, 
                       colours = myPalette, 
                       guide = "legend", 
                       name = fill_field,
                       na.value = "grey") +
  #scale_fill_gradient(low="red",high = "blue") +
  xlab("") + 
  ylab("") +
  theme(axis.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  theme(axis.title.x=element_text()) +
  theme(axis.text.x=element_text(size=0, family="ApexRounded-Medium")) +
  theme(axis.text.y=element_text(size=0, family="ApexRounded-Medium")) +
  theme(plot.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  theme(legend.text=element_text(size=20, family="ApexRounded-Medium")) +
  theme(legend.title=element_text(size=20, colour="#434348", family="ApexRounded-Medium")) +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_line(color='white')) +
  theme(panel.grid.minor= element_line(color='white')) +
  ggtitle("Third Season")


#filter season
R4 <- filter(ElkHarvestRifle2014, Season == "Fourth")
data3 <- left_join(data2,R4, by=c("GMUID"))

fill_field <- "Success"
data4 <- data3[,c("id","long","lat","order","hole","piece","group","GMUID","Season",fill_field)]
colnames(data4) <- c("id","long","lat","order","hole","piece","group","GMUID","Season","fill_field")

data4$fill_field[(data4$fill_field > breaks[8])] <- fill_field_max
data4$fill_field[(breaks[7] < data4$fill_field) & (data4$fill_field < breaks[8])] <- breaks[8]
data4$fill_field[(breaks[6] < data4$fill_field) & (data4$fill_field < breaks[7])] <- breaks[7]
data4$fill_field[(breaks[5] < data4$fill_field) & (data4$fill_field < breaks[6])] <- breaks[6]
data4$fill_field[(breaks[4] < data4$fill_field) & (data4$fill_field < breaks[5])] <- breaks[5]
data4$fill_field[(breaks[3] < data4$fill_field) & (data4$fill_field < breaks[4])] <- breaks[4]
data4$fill_field[(breaks[2] < data4$fill_field) & (data4$fill_field < breaks[3])] <- breaks[3]
data4$fill_field[(breaks[1] < data4$fill_field) & (data4$fill_field < breaks[2])] <- breaks[2]

plot_R4 <- ggplot(data4, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = fill_field),colour = "black", size = .1) +
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = GMUID),size=3) +
  geom_path(data = COroads2,aes(x = long, y = lat, group = group), color="#3878C7",size=2) +
  #facet_grid(. ~ Season) +
  scale_fill_gradientn(limits = plot_limits,
                       values = rescale(breaks),
                       breaks = breaks, 
                       colours = myPalette, 
                       guide = "legend", 
                       name = fill_field,
                       na.value = "grey") +
  #scale_fill_gradient(low="red",high = "blue") +
  xlab("") + 
  ylab("") +
  theme(axis.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  theme(axis.title.x=element_text()) +
  theme(axis.text.x=element_text(size=0, family="ApexRounded-Medium")) +
  theme(axis.text.y=element_text(size=0, family="ApexRounded-Medium")) +
  theme(plot.title=element_text(size=20, colour="#434348", family="ApexRounded-Bold")) +
  theme(legend.text=element_text(size=20, family="ApexRounded-Medium")) +
  theme(legend.title=element_text(size=20, colour="#434348", family="ApexRounded-Medium")) +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_line(color='white')) +
  theme(panel.grid.minor= element_line(color='white')) +
  ggtitle("Fourth Season")


# show all 4 maps in the same image
library(gridExtra)

z <- arrangeGrob(plot_R1, plot_R2, plot_R3, plot_R4, main = textGrob("2014 Colorado Rifle Season Success Rates", 
                                                                     gp = gpar(fontsize = 25, face = "bold", col = "black")))

z