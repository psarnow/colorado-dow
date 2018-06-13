#' ---
#' title: "Where do people hunt elk in Colorado"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' # Initial Questions to Explore
#' I'm wondering how many hunters are in each of the units
#' I would expect that CPW associates the number of hunters to how many elk are in each unit.
#' 
#' I am also curious to know if the number of hunters has changed from year to year.
#'
#' *NOTICE* that I am only looking at the general rifle hunting seasons on public land. There are also
#' hunters in Archery, Muzzleloader, Private Land, Ranching for Wildlife, etc.
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
#' Run script to get hunter data
#+ source population, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)
#+ setdir, include=FALSE
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")

#' Table of the data
COElkRifleAll

#' # Statewide Elk Hunters
#' First lets look at the entire state as a whole
COElkHuntersStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                      Hunters = sum(Hunters,na.rm = T))

COElkHuntersStatewide <- summarise(group_by(COElkHuntersStatewide,Year),
                                   Hunters = sum(Hunters))

ggplot(COElkHuntersStatewide, aes(Year,Hunters)) +
  geom_bar(stat="identity") +
  coord_cartesian(ylim = c(120000,160000)) +
  labs(title="Statewide Elk Hunters", caption="source: cpw.state.co.us")

#' Besides 2009-2011 the number of hunters has been consistent. For those 3 years the number of hunters
#' dropped from about 145,000 to 125,000
#' 
#' ## Hunters by Unit
#' I'd like to know where the hunters are distributed across the state.
#' 
#' run script to get unit boundaries so we can draw them on a map
#+ source hunt units, message=F, warning=F
source('~/_code/colorado-dow/datasets/coordinate locations of cpw hunt units.R', echo=F)
#+ setdir1, include=FALSE
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
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

# Hunters in each unit (Combine the seasons)
COElkUnitHunters <- summarise(group_by(COElkRifleAll,Year,Unit),
                                 Hunters = sum(Hunters,na.rm = T))

Year2017 <- filter(COElkUnitHunters, Year == "2017")
HunterstoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ Hunters-Map, fig.width=10, fig.height=7
ggplot(HunterstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Hunters),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Oranges",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2017 Colorado Elk Hunters", caption="source: cpw.state.co.us")

#' ## Year to Year Hunter Trends
#' Lets look at the hunter changes from year to year.

png(file="HuntersMap%02d.png", width=1400, height=1000)
icounter <- 0
for (imap in unique(COElkUnitHunters$Year)){
  yearplot <- filter(COElkUnitHunters, Year == imap)
  HunterstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HunterstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Hunters),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Oranges",
                         direction = 1,
                         na.value = 'grey',
                         breaks=c(1000,2000,3000,4000,5000,6000,7000),
                         limits = c(0,max(COElkUnitHunters$Hunters))) + #fix so each year chart has same color breaks
    xlab("") + 
    ylab("") +
    theme(panel.background = element_rect(fill='white')) +
    theme(panel.grid.major= element_blank()) +
    theme(panel.grid.minor= element_blank()) +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitHunters$Year)))) +
    labs(title="Colorado Elk Hunters", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  icounter <- icounter + 1
}
#+ device, message=F, warning=F
dev.off()

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png Huntersmap.gif")

#+ cleanup, message=F, warning=F
file.remove(list.files(pattern=".png"))

#' ![Colorado Elk Hunters by Year](Huntersmap.gif)

#' ## Number of Hunters Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' I might average the number of hunters of the last few years since each year isn't consistent
HunterRanklast3 <- filter(COElkUnitHunters, as.numeric(Year) >= 2015)
HunterRanklast3 <- summarise(group_by(HunterRanklast3,Unit),
                                 Hunters = mean(Hunters,na.rm = T))
HunterRanklast3$HuntersRank = rank(-HunterRanklast3$Hunters)

HunterRanklast3 <- filter(HunterRanklast3, HuntersRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
HunterRanklast3 <- HunterRanklast3[order(-HunterRanklast3$Hunters), ]  # sort
HunterRanklast3$Unit <- factor(HunterRanklast3$Unit, levels = HunterRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(HunterRanklast3, aes(x=Unit, y=Hunters)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Hunters)) + 
  labs(title="Average Elk Hunters 2015-2017\nTop 50 Units", subtitle="Hunters by Unit", caption="source: cpw.state.co.us")

#' ## Conclusion
#' Its interesting that some of these units with high amount of hunters doesn't seem to coincide with
#' how many elk are in those units. The Northwestern herd seems to have the most amount of hunters,
#' but our last report indicated that units with more elk in them were in the South and Southwest.
#' 
#' Lets investigate that further. Is there a correlation of number of hunters to number of elk in each
#' unit?  If not, I would certainly be interested in which units have the highest ratio of Elk to Hunters

