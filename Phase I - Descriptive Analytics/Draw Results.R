#' ---
#' title: "Hunter license draw success by unit"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Initial Questions to Explore
#' * How difficult is it to get a license for each unit and season?
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
#' Run script to get draw data
#+ source_draw, message=F, warning=F
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)

#' Table of the data
COElkDrawAll2

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)
#' ***
#' ## Draw Success
#' ### Statewide By Year
#' First lets look at the entire state as a whole
# Remove Inf
COElkDraw <- filter(COElkDrawAll, is.finite(Draw_Success) & !is.na(Draw_Success))
# Cap success to 100%
COElkDraw$Draw_Success[COElkDraw$Draw_Success>1] <- 1.00
#' By Year
DrawSuccessStatewide <- summarise(group_by(COElkDraw,Year),
                           Draw_Success = mean(Draw_Success,na.rm = T))

ggplot(DrawSuccessStatewide, aes(Year,Draw_Success)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.65,.90)) +
  labs(title="Statewide Elk Hunter Draw Success", caption="source: cpw.state.co.us")

#' > Definitely getting harder to draw each year.
#' 
#' ***
#' ### Statewide by Hunting Season
DrawSuccessStatewide.Season <- dplyr::summarise(group_by(COElkDraw,Season),
                                         Draw_Success = mean(Draw_Success,na.rm = T))

ggplot(DrawSuccessStatewide.Season, aes(Season,Draw_Success)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.65,.90)) +
  labs(title="Statewide Elk Hunter Draw Success by Season", caption="source: cpw.state.co.us")

#' > Fourth season has better draw success than the others.  
#' Is this because there are less licenses? or more applicants?  Well we saw that the number of hunters
#' each year is fairly consistent.
#' 
#' ***
#' ## Applicants per year
DrawApplicantsStatewide <- summarise(group_by(COElkDraw,Year),
                                     Draw_Applicants = sum(Ttl_Chce_1,na.rm = T))

ggplot(DrawApplicantsStatewide, aes(Year,Draw_Applicants)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(75000,110000)) +
  labs(title="Statewide Elk Hunter Draw Applicants", caption="source: cpw.state.co.us")
#' > The number of applicants is now at the levels from 2006, after dropping off for several years by
#' 12,000. Could be economic related.
#' 
#' ### Quota per year
DrawQuotaStatewide <- summarise(group_by(COElkDraw,Year),
                                     Quota = sum(Orig_Quota,na.rm = T))

ggplot(DrawQuotaStatewide, aes(Year,Quota)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(100000,150000)) +
  labs(title="Statewide Elk Hunter Draw Quota", caption="source: cpw.state.co.us")

#' > TODO commentary
#' 
#' ***
#' ## Draw Success
#' ### By Unit
#' How draw success differs across the state
# Group seasons
DrawSuccess <- summarise(group_by(COElkDraw,Year, Unit),
                         Draw_Success = mean(Draw_Success,na.rm = T))
#' Last year's data
Year2017 <- filter(DrawSuccess, Year == "2017")
DrawSuccesstoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ DrawSuccess-Map, fig.width=10, fig.height=8.46
ggplot(DrawSuccesstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Draw_Success),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "YlOrBr",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2017 Colorado Hunter Draw Success", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' ### Year to Year Draw Success Trends
#' Create a png of each year
icounter <- 0
for (imap in unique(DrawSuccess$Year)){
  png(file=paste("DrawSuccessMap",imap,".png"), width=948, height=700)
  yearplot <- filter(DrawSuccess, Year == imap)
  DrawSuccesstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(DrawSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Draw_Success),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "YlOrBr",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(DrawSuccess$Draw_Success))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(DrawSuccess$Year)))) +
    labs(title="Colorado Elk Draw Success by Year", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off() 
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png DrawSuccessmap.gif")

#' ![](DrawSuccessmap.gif)
#' > Noticable difference in lower statewide draw success in recent years. The most successful
#' units for hunting may have lower draw success (more hunters are applying for those).
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Seasonal Hunter Success Trends
# Group Years
DrawSuccessSeason <- summarise(group_by(COElkDraw,Season,Unit),
                               Draw_Success = mean(Draw_Success,na.rm = T))
#' Create a png of each season
icounter <- 0
for (imap in unique(DrawSuccessSeason$Season)){
  png(file=paste("DrawSuccessSeasonMap",imap,".png"), width=948, height=700)
  seasonplot <- filter(DrawSuccessSeason, Season == imap)
  DrawSuccesstoPlot <- left_join(Unitboundaries2,seasonplot, by=c("Unit"))
  p1 <- ggplot(DrawSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Draw_Success),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "YlOrBr",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(DrawSuccessSeason$Draw_Success))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(DrawSuccessSeason$Season)))) +
    labs(title="Colorado Elk Draw Success by Season", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png DrawSuccessSeasonmap.gif")

#' ![](DrawSuccessSeasonmap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Rank Units by overall draw success
#' Would also be beneficial to rank each unit so I can reference later.
#' I'll average the last few years
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

#' > TODO - commentary
#' 
#' ***
#' ## Conclusion
#' > How about breakdowns for each sex? Bull, Cow, Either. 
#' > What about seeing these for Hunters, Success, Draw, etc