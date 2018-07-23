#' ---
#' title: "Where do people hunt elk in Colorado"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Initial Questions to Explore
#' * I'm wondering how many hunters are in each of the units
#' * I would expect that CPW associates the number of hunters to how many elk are in each unit.
#' 
#' I am also curious to know if the number of hunters has changed from year to year.
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
#' Set our preferred charting theme
theme_set(theme_minimal()+theme_hc()+theme(legend.key.width = unit(1.5, "cm")))
#' Run script to get hunter data
#+ source_population, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)
#' Table of the hunter data
COElkRifleAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)
#' ***
#' ## Total Elk Harvest
#' ### Statewide
# Group seasons
COElkHuntersStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                   Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T))
# Group Units
COElkHuntersStatewide <- summarise(group_by(COElkHuntersStatewide,Year),
                                   Hunters = sum(Hunters))

ggplot(COElkHuntersStatewide, aes(Year,Hunters)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(120000,160000)) +
  labs(title="Statewide Elk Hunters", caption="source: cpw.state.co.us")
#' > At a highpoint in 2006 of ~151000, the number of hunters decreased to a low in 2009 of ~133000. 
#' Since 2009 the total number of hunters has slightly increased from year to year.
#' 
#' ***
#' 
#' ### Hunters by Unit
#' I'd like to know where the hunters are distributed across the state.
#' 
# Group seasons
COElkUnitHunters <- summarise(group_by(COElkRifleAll,Year,Unit),
                              Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T))
#' Last year's data
Year2017 <- filter(COElkUnitHunters, Year == "2017")
HunterstoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ Hunters-Map, fig.width=10, fig.height=8.46
ggplot(HunterstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Hunters),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Oranges",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2017 Colorado Elk Hunters", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' 
#' ### Year to Year Hunter Trends
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkUnitHunters$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("HuntersMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkUnitHunters, Year == imap)
  HunterstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HunterstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Hunters),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Oranges",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(COElkUnitHunters$Hunters))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitHunters$Year)))) +
    labs(title="Colorado Elk Hunters", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png Huntersmap.gif")

#' ![](Huntersmap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Number of Hunters Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the number of hunters of the last few years
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
#' > TODO - commentary
#' 
#' ***
#' ## Conclusion
#' > Its interesting that some of these units with high amount of hunters doesn't seem to coincide with
#' how many elk are in those units. The Northwestern herd seems to have the most amount of hunters,
#' but our last report indicated that units with more elk in them were in the South and Southwest.
#' 
#' > Lets investigate that further. Is there a correlation of number of hunters to number of elk in each
#' unit?  If not, I would certainly be interested in which units have the highest ratio of Elk to Hunters