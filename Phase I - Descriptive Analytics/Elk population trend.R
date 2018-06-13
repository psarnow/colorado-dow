#' ---
#' title: "What is the trend of the elk population"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' # Initial Questions to Explore
#' I would like to know where the elk are distributed across the state, which units have
#' more elk, which units have less elk?
#' 
#' I am also curious to know if the herd sizes have changed from year to year.
#'
#' ### Setup
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T)
library(dplyr,quietly = T)
library(ggplot2, quietly = T)
#' Set our preferred charting theme
theme_set(theme_minimal())
#' Run script to get elk population data
#+ source population, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow population estimates.R', echo=F)
#' Table of the data
COElkPopulationAll

#' # Statewide Elk Population
#' First lets look at the entire state as a whole
# Population of the Data Analysis Units (DAU) -- Herd IDs
COElkPopulationStatewide <- summarise(group_by(COElkPopulationAll,Year,DAU),
                                      Population = mean(Population))

COElkPopulationStatewide <- summarise(group_by(COElkPopulationStatewide,Year),
                                      Population = sum(Population))

ggplot(COElkPopulationStatewide, aes(Year,Population)) +
  geom_bar(stat="identity") +
  # prettytheme +
  coord_cartesian(ylim = c(250000,300000)) +
  ggtitle("Statewide Elk Population")

#' In recent years it appears the state's elk population has stabalized and is close to the 10 year median.
#'
#' ## Population by Unit
#' I'd like to know where the elk are distributed across the state. I could look at each year
#' but in this case I am only interested in the most recent year with population data.
#' 
#' run script to get unit boundaries so we can draw them on a map
#+ source hunt units, message=F, warning=F
source('~/_code/colorado-dow/datasets/coordinate locations of cpw hunt units.R', echo=F)

#' Get a statemap with some roads on it
#+ roaddata, message=F, warning=F
roaddata <- rgdal::readOGR("~/_code/colorado-dow/datasets/ne_10m_roads/ne_10m_roads.shp")
USAroads <- roaddata %>% subset(.,sov_a3 == "USA" & type == "Major Highway")
# I need to convert to data frames so that I can use the data with ggplot2.
USAroads <- fortify(USAroads)
Unitboundaries <- shapefile %>% fortify(region = "GMUID")

Unitboundaries2 <- merge(Unitboundaries, shapefile@data, by.x = 'id', by.y = 'GMUID')
Unitboundaries2$Unit <- as.character(Unitboundaries2$id)

# get min/max of long/lat for zooming
longset <- c(min(Unitboundaries2$long),max(Unitboundaries2$long))
latset <- c(min(Unitboundaries2$lat),max(Unitboundaries2$lat))
COroads <- filter(USAroads, long > longset[1] & long < longset[2])
COroads <- filter(COroads, lat > latset[1] & lat < latset[2])

# Population of the Data Analysis Units (DAU) -- Herd IDs
COElkUnitPopulation <- summarise(group_by(COElkPopulationAll,Year,Unit),
                                 Population = mean(Unit_Pop))

Year2016 <- filter(COElkUnitPopulation, Year == "2016")
PopulationtoPlot <- left_join(Unitboundaries2,Year2016, by=c("Unit"))

#+ Population-Map, fig.width=10, fig.height=7
ggplot(PopulationtoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Population),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2016 Colorado Elk Population", caption="source: cpw.state.co.us")

#' I considered using some real maps, but they get a little busy with more labeling.
#+ load package, message=F, warning=F
library(ggmap)

#+ download maps, message=F, warning=F
colorado_stamen_map <- qmap(location=c(lon=-105.54,lat=39.1438), zoom=7, source = "stamen",maptype = "toner")   

colorado_stamen_map +
  geom_polygon(data = PopulationtoPlot, aes(long,lat,group=group,fill = Population),colour = "grey50", size = .2,alpha=.5) + #Unit boundaries
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=2) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2016 Colorado Elk Population", caption="source: cpw.state.co.us")

#' ## Year to Year Population Trends
#' Lets look at the population changes from year to year.  While we could use some facetting
#' or even grid tools for each year's map, I think it would be easier to visualize with
#' some animation from year to year.

png(file="PopMap%02d.png", width=1400, height=1000)
icounter <- 0
for (imap in unique(COElkUnitPopulation$Year)){
  yearplot <- filter(COElkUnitPopulation, Year == imap)
  PopulationtoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(PopulationtoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Population),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Greens",
                         direction = 1,
                         na.value = 'grey',
                         breaks=c(1000,2000,3000,4000,5000,6000,7000),
                         limits = c(0,max(COElkUnitPopulation$Population))) + #fix so each year chart has same color breaks
    xlab("") + 
    ylab("") +
    theme(panel.background = element_rect(fill='white')) +
    theme(panel.grid.major= element_blank()) +
    theme(panel.grid.minor= element_blank()) +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitPopulation$Year)))) +
    labs(title="Colorado Elk Population", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  icounter <- icounter + 1
}
#+ device, message=F, warning=F
dev.off()

#' Convert the .png files to one .gif file using ImageMagick. 
#' The system() function executes the command as if it was done in the terminal. 
#' The -delay flag sets the time between showing the frames, i.e. the speed of the animation.
system("convert -delay 150 *.png Popmap.gif")

#+ cleanup, message=F, warning=F
file.remove(list.files(pattern=".png"))

#' ![Colorado Elk Population by Year](Popmap.gif)
#' 
#' ## Population Rank of the Units
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


#' Lollipop Chart
#' Conveys the same information as in bar charts. By reducing the thick bars into thin lines, it reduces the clutter and lays more emphasis on the value. 
#' It looks nice and modern.
ggplot(PopulationRanklast3, aes(x=Unit, y=Population)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Population)) + 
  labs(title="Average Elk Population 2014-2016\nTop 50 Units", subtitle="Population by Unit", caption="source: cpw.state.co.us")

#' ## Conclusion
#' There are sizable herds in Southern Colorado (80-82), as well as Southwestern Colorado.
#' The Northwestern herd is large too. The number of elk are less along the Front Range.
#' 
#' After looking at this data, I'm wondering how many hunters are in each of these areas.
#' I would expect that CPW associates the number of hunters to how many elk are in each unit.

