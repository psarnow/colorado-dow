#' ---
#' title: "Hunter Success Rates in Hunting Units"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' # Initial Questions to Explore
#' Which units and seasons have the best hunt success?
#'
#' ### Setup
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T)
library(dplyr,quietly = T)
library(ggplot2, quietly = T)
library(scales, quietly = T)
#' Set our preferred charting theme
theme_set(theme_minimal())
#' Run script to get hunter data
#+ source hunter data, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)
#' Table of the data
COElkRifleAll

#' # Statewide Elk Hunter Success
#' ### By Year
#' First lets look at the entire state as a whole
COElkSuccessStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                   Success = mean(Success,na.rm = T))

COElkSuccessStatewide <- summarise(group_by(COElkSuccessStatewide,Year),
                                   Success = mean(Success)/100)

ggplot(COElkSuccessStatewide, aes(Year,Success)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.15,.25)) +
  labs(title="Statewide Elk Hunter Success", caption="source: cpw.state.co.us")

#' There is a general trend since 2006 of declining hunter success. 2009 Did have a 'reset' of 
#' success and with the exception of 2015 rates have been decreasing. I should note that 2017 was higher
#' than 2016, hopefully that positive trend continues for 2018.
#' 
#' **FUTURE** what happened in 2015 for hunter success rates to increase and match those from 2006, and 2009?
#' 
#' **FUTURE** why are success rates declining?
#' 
#' ### By Hunting Season
COElkSuccessStatewide.Season <- summarise(group_by(COElkRifleAll,Season,Unit),
                                   Success = mean(Success,na.rm = T))

COElkSuccessStatewide.Season <- summarise(group_by(COElkSuccessStatewide.Season,Season),
                                   Success = mean(Success)/100)

ggplot(COElkSuccessStatewide.Season, aes(Season,Success)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.15,.25)) +
  labs(title="Statewide Elk Hunter Success by Season", caption="source: cpw.state.co.us")

#' First and Fourth seasons have better success rates than the others.
#' 
#' ## Yearly Hunter Success by Unit
#' I'd like to know the distribution of Hunter success across the state.
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

COElkSuccess <- summarise(group_by(COElkRifleAll,Year,Unit),
                          Success = mean(Success,na.rm = T)/100)

Year2017 <- filter(COElkSuccess, Year == "2017")
HunterSuccesstoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ HunterSuccess-Map, fig.width=10, fig.height=7
ggplot(HunterSuccesstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Success),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "RdPu",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2017 Colorado Elk Hunter Success", caption="source: cpw.state.co.us")

#' ## Year to Year Hunter Success Trends
#' Lets look at the changes from year to year.
#+ include=F
dev.off()

png(file="HunterSuccessMap%02d.png", width=1400, height=1000)
icounter <- 0
for (imap in unique(COElkSuccess$Year)){
  yearplot <- filter(COElkSuccess, Year == imap)
  HunterSuccesstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HunterSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Success),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "RdPu",
                         direction = 1,
                         na.value = 'grey',
                         limits = c(0,max(COElkSuccess$Success))) + #fix so each year chart has same color breaks
    xlab("") + 
    ylab("") +
    theme(panel.background = element_rect(fill='white')) +
    theme(panel.grid.major= element_blank()) +
    theme(panel.grid.minor= element_blank()) +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkSuccess$Year)))) +
    labs(title="Colorado Elk Hunter Success by Year", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  icounter <- icounter + 1
}
#+ device, message=F, warning=F
dev.off()

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png HunterSuccessmap.gif")

#' cleanup
file.remove(list.files(pattern=".png"))

#' ![Colorado Elk Hunter Success by Year](HunterSuccessmap.gif)
#' There are pockets of units that are more successful than the rest of the state. See units
#' 2 and 40. Especially in recent years.
#' 
#' ## Seasonal Hunter Success Trends
#' Lets look at the changes for each season
COElkSuccessSeason <- summarise(group_by(COElkRifleAll,Season,Unit),
                          Success = mean(Success,na.rm = T)/100)
#+ include=F
dev.off()

png(file="HunterSuccessSeasonMap%02d.png", width=1400, height=1000)
icounter <- 0
for (imap in unique(COElkSuccessSeason$Season)){
  seasonplot <- filter(COElkSuccessSeason, Season == imap)
  HunterSuccesstoPlot <- left_join(Unitboundaries2,seasonplot, by=c("Unit"))
  p1 <- ggplot(HunterSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Success),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "RdPu",
                         direction = 1,
                         na.value = 'grey',
                         limits = c(0,max(COElkSuccessSeason$Success))) + #fix so each year chart has same color breaks
    xlab("") + 
    ylab("") +
    theme(panel.background = element_rect(fill='white')) +
    theme(panel.grid.major= element_blank()) +
    theme(panel.grid.minor= element_blank()) +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkSuccessSeason$Season)))) +
    labs(title="Colorado Elk Hunter Success by Season", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  icounter <- icounter + 1
}
#+ device1, message=F, warning=F
dev.off()

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png HunterSuccessSeasonmap.gif")

#' cleanup
file.remove(list.files(pattern=".png"))

#' ![Colorado Elk Hunter Success by Season](HunterSuccessSeasonmap.gif)
#' For most of the state the higher successes in the first and fourth seasons are apparent.
#' 
#' ## Rank Units by overall hunter success
#' Would also be beneficial to rank each unit so I can reference later.
#' I'll average the last few years since each year isn't consistent
HunterSuccessRanklast3 <- filter(COElkSuccess, as.numeric(Year) >= 2015)
HunterSuccessRanklast3 <- summarise(group_by(HunterSuccessRanklast3,Unit),
                                    Success = mean(Success,na.rm = T))
HunterSuccessRanklast3$SuccessRank = rank(-HunterSuccessRanklast3$Success)

HunterSuccessRanklast3 <- filter(HunterSuccessRanklast3, SuccessRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
HunterSuccessRanklast3 <- HunterSuccessRanklast3[order(-HunterSuccessRanklast3$Success), ]  # sort
HunterSuccessRanklast3$Unit <- factor(HunterSuccessRanklast3$Unit, levels = HunterSuccessRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(HunterSuccessRanklast3, aes(x=Unit, y=Success)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Success)) +
  scale_y_continuous(labels = percent) +
  labs(title="Average Hunter Success 2015-2017\nTop 50 Units", subtitle="Success by Unit", caption="source: cpw.state.co.us")
#' There are a handful of units that have much better success than the rest of  the state.
#' 
#' ## Rank Units with Seasons by overall hunter success
HunterSuccessSeasonRanklast3 <- filter(COElkRifleAll, as.numeric(Year) >= 2015)
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "1"] <- "1st"
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "2"] <- "2nd"
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "3"] <- "3rd"
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "4"] <- "4th"

HunterSuccessSeasonRanklast3$Unit_Season <- paste(HunterSuccessSeasonRanklast3$Unit,HunterSuccessSeasonRanklast3$Season,sep="\n")
HunterSuccessSeasonRanklast3 <- summarise(group_by(HunterSuccessSeasonRanklast3,Unit_Season),
                                    Success = mean(Success,na.rm = T)/100)

HunterSuccessSeasonRanklast3$SuccessRank = rank(-HunterSuccessSeasonRanklast3$Success)

HunterSuccessSeasonRanklast3 <- filter(HunterSuccessSeasonRanklast3, SuccessRank <= 50) # top 50 unit seasons
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
HunterSuccessSeasonRanklast3 <- HunterSuccessSeasonRanklast3[order(-HunterSuccessSeasonRanklast3$Success), ]  # sort
HunterSuccessSeasonRanklast3$Unit_Season <- factor(HunterSuccessSeasonRanklast3$Unit_Season, levels = HunterSuccessSeasonRanklast3$Unit_Season)  # to retain the order in plot.

#' Lollipop Chart
ggplot(HunterSuccessSeasonRanklast3, aes(x=Unit_Season, y=Success)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit_Season, 
                   xend=Unit_Season, 
                   y=0, 
                   yend=Success)) +
  scale_y_continuous(labels = percent) +
  labs(title="Average Hunter Success 2015-2017\nTop 50 Unit Seasons", subtitle="Success by Unit and Season", caption="source: cpw.state.co.us")


#' ## Conclusion
#' First and Fourth seasons have the best success overall.  There are a handful of units that have much better
#' recent success rates than the rest of the state, and for the top few it doesn't even seem season dependent.
#' I'd like to look at what it takes to get a license to hunt in those units. I can do that by reviewing past
#' draw results.
#' 
#' Look at draw results for all seasons of units 2, 40, 61, 9, 201, 20, 10. What is the likelihood of acquiring
#' a license for a Bull and Cow in each of their seasons? Is there a unit, season, or sex that has a better likelihood
#' of license success?