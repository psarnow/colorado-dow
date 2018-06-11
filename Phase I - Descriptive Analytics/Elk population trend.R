#' ---
#' title: "What is the trend of the elk population"
#' author: "Pierre Sarnow"
#' ---

#' I am curious to know if the herd sizes have changed from year to year.
#' I would also like to know where the elk are distributed across the state, which units have
#' more elk, which units have less elk.

setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")

#' Load required libraries for wrangling data, charting, and mapping
library(dplyr,quietly = T)
library(ggplot2, quietly = T)

#' Prettier chart theme
prettytheme <- theme(
  axis.text=element_text(colour="#606060",family="Muli-Regular"),
  plot.title=element_text(hjust = 0.5,colour="#333333", family="Muli-Bold"),
  panel.grid.major = element_line(colour = "#d8d8d8"),
  panel.background = element_rect(fill="#ffffff"),
  plot.background = element_rect(fill = "#ffffff"),
  axis.title=element_text(colour="#707070", family="Muli-Regular"),
  axis.title.x=element_text(vjust=-.3),
  legend.text=element_text(color="#333333",family="Muli-Regular"),
  legend.background = element_rect(fill='#ffffff'),
  legend.direction = "horizontal", 
  legend.position = "top",
  legend.key = element_rect(fill='#ffffff',colour='#ffffff'),
  panel.grid.minor= element_blank(),
  strip.text = element_text(family="Muli-Regular", colour="#333333"),
  strip.background=element_rect(fill="#ffffff", colour="#ffffff")
)

#' Palette from highcharts
hcpalette <- c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9', '#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1')

#' Run script to get elk population data
source('~/_code/colorado-dow/datasets/read colorado dow population estimates.R', echo=F)
head(COElkPopulationAll)

#' First lets look at the entire state as a whole
# Population of the Data Analysis Units (DAU) -- Herd IDs
COElkPopulationStatewide <- summarise(group_by(COElkPopulationAll,Year,DAU),
                                      Population = mean(Population))

COElkPopulationStatewide <- summarise(group_by(COElkPopulationStatewide,Year),
                                      Population = sum(Population))

ggplot(COElkPopulationStatewide, aes(Year,Population)) +
  geom_bar(stat="identity") +
  prettytheme +
  coord_cartesian(ylim = c(250000,300000)) +
  ggtitle("Statewide Elk Population")

#' In recent years it appears the state's elk population has stabalized and is close to the 10 year median.

#' ## Population by Unit
#' I'd like to know where the elk are distributed across the state. I could look at each year
#' but in this case I am only interested in the most recent year with population data.
# run script to get unit boundaries so we can draw them on a map
source('~/_code/colorado-dow/datasets/coordinate locations of cpw hunt units.R', echo=F)
# library(maptools, quietly = T)

#' Get a statemap with some roads on it
roaddata <- rgdal::readOGR("~/_code/colorado-dow/datasets/ne_10m_roads/ne_10m_roads.shp")
USAroads <- roaddata %>% subset(.,sov_a3 == "USA" & type == "Major Highway")
#' I need to convert to data frames so that I can use the data with ggplot2.
USAroads <- fortify(USAroads)
Unitboundaries <- shapefile %>% fortify(region = "GMUID")

Unitboundaries2 <- merge(Unitboundaries, shapefile@data, by.x = 'id', by.y = 'GMUID')
Unitboundaries2$Unit <- as.character(Unitboundaries2$id)

# Population of the Data Analysis Units (DAU) -- Herd IDs
COElkUnitPopulation <- summarise(group_by(COElkPopulationAll,Year,Unit),
                                 Population = mean(Unit_Pop))

Year2016 <- filter(COElkUnitPopulation, Year == "2016")
PopulationtoPlot <- left_join(Unitboundaries2,Year2016, by=c("Unit"))

# get min/max of long/lat for zooming
longset <- c(min(PopulationtoPlot$long),max(PopulationtoPlot$long))
latset <- c(min(PopulationtoPlot$lat),max(PopulationtoPlot$lat))
COroads <- filter(USAroads, long > longset[1] & long < longset[2])
COroads <- filter(COroads, lat > latset[1] & lat < latset[2])

ggplot(PopulationtoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Population),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=4) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  ggtitle("2016 Colorado Elk Population")



# devtools::install_github("hrbrmstr/ggalt")

# load packages
library(ggplot2)
library(ggmap)
# library(ggalt)


colorado_stamen_map <- qmap(location=c(lon=-105.4,lat=39.1438), zoom=7, source = "stamen",maptype = "toner")   

colorado_stamen_map +
  geom_polygon(data = PopulationtoPlot, aes(long,lat,group=group,fill = Population),colour = "grey50", size = .2,alpha=.2) + #Unit boundaries
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=4) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  ggtitle("2016 Colorado Elk Population")
##############################################################################################################
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' I might average the population of the last few years since they tend to move around
#' a little
PopulationRanklast3 <- filter(COElkUnitPopulation, as.numeric(Year) >= 2014)
PopulationRanklast3 <- summarise(group_by(PopulationRanklast3,Unit),
                                 Population = mean(Population,na.rm = T))
PopulationRanklast3$PopulationRank = rank(-PopulationRanklast3$Population)

PopulationRanklast3 <- filter(PopulationRanklast3, PopulationRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
PopulationRanklast3 <- PopulationRanklast3[order(-PopulationRanklast3$Population), ]  # sort
PopulationRanklast3$Unit <- factor(PopulationRanklast3$Unit, levels = PopulationRanklast3$Unit)  # to retain the order in plot.


# Plot
ggplot(PopulationRanklast3, aes(x=Unit, y=Population)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Population)) + 
  prettytheme +
  labs(title="Average Elk Population 2014-2016\nTop 50 Units", subtitle="Population by Unit", caption="source: CPW")


