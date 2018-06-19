#' ---
#' title: "Hunter license draw success by unit"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' # Initial Questions to Explore
#' How difficult is it to get a license for each unit and season?
#' 
#' **NOTICE** I will initially ignore the differences between resident, nonresident and youth statuses.
#' I will also ignore the amount of preference points used when applying for a license in the draw.
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
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)
#' Table of the data
COElkDrawAll2

#' # Statewide Elk Hunter Success
#' ### By Year
#' First lets look at the entire state as a whole
# Remove Inf
COElkDraw <- filter(COElkDrawAll, is.finite(Draw_Success) & !is.na(Draw_Success))
# Cap success to 100%
COElkDraw$Draw_Success[COElkDraw$Draw_Success>1] <- 1.00
#' ### By Year
DrawSuccessStatewide <- dplyr::summarise(group_by(COElkDraw,Year),
                           Draw_Success = mean(Draw_Success,na.rm = T))

ggplot(DrawSuccessStatewide, aes(Year,Draw_Success)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.65,.90)) +
  labs(title="Statewide Elk Hunter Draw Success", caption="source: cpw.state.co.us")

#' Look at this trend. Definitely getting harder to draw each year.
#' 
#' ### By Hunting Season
DrawSuccessStatewide.Season <- dplyr::summarise(group_by(COElkDraw,Season),
                                         Draw_Success = mean(Draw_Success,na.rm = T))

ggplot(DrawSuccessStatewide.Season, aes(Season,Draw_Success)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.65,.90)) +
  labs(title="Statewide Elk Hunter Draw Success by Season", caption="source: cpw.state.co.us")

#' Fourth season has better draw success than the others.
#' 
#' Is this because there are less licenses? or more applicants?  Well we saw that the number of hunters
#' each year is fairly consistent.
#' # TODO
#' plot applicants per year
#' 
#' 
#' ## Yearly Draw Success by Unit
#' I'd like to know the distribution of Draw success across the state.
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

DrawSuccess <- dplyr::summarise(group_by(COElkDraw,Year, Unit),
                                Draw_Success = mean(Draw_Success,na.rm = T))

Year2017 <- filter(DrawSuccess, Year == "2017")
DrawSuccesstoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ HunterSuccess-Map, fig.width=10, fig.height=7
ggplot(DrawSuccesstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Draw_Success),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "YlOrBr",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2017 Colorado Hunter Draw Success", caption="source: cpw.state.co.us")

#' ## Year to Year Draw Success Trends
#' Lets look at the changes from year to year.
#+ include=F
dev.off()

png(file="DrawSuccessMap%02d.png", width=1400, height=1000)
icounter <- 0
for (imap in unique(DrawSuccess$Year)){
  yearplot <- filter(DrawSuccess, Year == imap)
  DrawSuccesstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(DrawSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Draw_Success),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "YlOrBr",
                         direction = 1,
                         na.value = 'grey',
                         limits = c(0,max(DrawSuccess$Draw_Success))) + #fix so each year chart has same color breaks
    xlab("") + 
    ylab("") +
    theme(panel.background = element_rect(fill='white')) +
    theme(panel.grid.major= element_blank()) +
    theme(panel.grid.minor= element_blank()) +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(DrawSuccess$Year)))) +
    labs(title="Colorado Elk Draw Success by Year", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  icounter <- icounter + 1
}
#+ device, message=F, warning=F
dev.off()

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png DrawSuccessmap.gif")

#' cleanup
file.remove(list.files(pattern=".png"))

#' ![Colorado Elk Draw Success by Year](DrawSuccessmap.gif)
#' Noticable difference in lower statewide draw success in recent years. The most successful
#' units for hunting may have lower draw success (more hunters are applying for those).
#' 
#' ## Seasonal Hunter Success Trends
#' Lets look at the changes for each season
#' 
DrawSuccessSeason <- summarise(group_by(COElkDraw,Season,Unit),
                               Draw_Success = mean(Draw_Success,na.rm = T))
#+ include=F
dev.off()

png(file="DrawSuccessSeasonMap%02d.png", width=1400, height=1000)
icounter <- 0
for (imap in unique(DrawSuccessSeason$Season)){
  seasonplot <- filter(DrawSuccessSeason, Season == imap)
  DrawSuccesstoPlot <- left_join(Unitboundaries2,seasonplot, by=c("Unit"))
  p1 <- ggplot(DrawSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Draw_Success),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "YlOrBr",
                         direction = 1,
                         na.value = 'grey',
                         limits = c(0,max(DrawSuccessSeason$Draw_Success))) + #fix so each year chart has same color breaks
    xlab("") + 
    ylab("") +
    theme(panel.background = element_rect(fill='white')) +
    theme(panel.grid.major= element_blank()) +
    theme(panel.grid.minor= element_blank()) +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(DrawSuccessSeason$Season)))) +
    labs(title="Colorado Elk Draw Success by Season", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  icounter <- icounter + 1
}
#+ device1, message=F, warning=F
dev.off()

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png DrawSuccessSeasonmap.gif")

#' cleanup
file.remove(list.files(pattern=".png"))

#' ![Colorado Elk Draw Success by Season](DrawSuccessSeasonmap.gif)
#' ## Rank Units by overall draw success
#' Would also be beneficial to rank each unit so I can reference later.
#' I'll average the last few years since each year isn't consistent
DrawSuccessRanklast3 <- filter(DrawSuccess, as.numeric(Year) >= 2016)
DrawSuccessRanklast3 <- summarise(group_by(DrawSuccessRanklast3,Unit),
                                  Draw_Success = mean(Draw_Success,na.rm = T))
DrawSuccessRanklast3$SuccessRank = rank(-DrawSuccessRanklast3$Draw_Success)

DrawSuccessRanklast3 <- filter(DrawSuccessRanklast3, SuccessRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
DrawSuccessRanklast3 <- DrawSuccessRanklast3[order(-DrawSuccessRanklast3$Draw_Success), ]  # sort
DrawSuccessRanklast3$Unit <- factor(DrawSuccessRanklast3$Unit, levels = DrawSuccessRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(DrawSuccessRanklast3, aes(x=Unit, y=Draw_Success)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Draw_Success)) +
  scale_y_continuous(labels = percent) +
  labs(title="Average Draw Success 2016-2018\nTop 50 Units", subtitle="Success by Unit", caption="source: cpw.state.co.us")
#'
#' 


#' ## Conclusion
#' 