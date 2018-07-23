#' ---
#' title: "Hunter Success Rates in Hunting Units"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Initial Questions to Explore
#' * Which units and seasons have the best hunt success?
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
#' Run script to get success data
#+ source_success, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)

#' Table of the harvest success data
COElkRifleAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)
#' ***
#' ## Hunter Success
#' ### Statewide 
#' #### Success By Year
# Recalc success using all the hunter and harvest results
COElkSuccessStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                   Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T),
                                   Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T),
                                   Success = Harvest / Hunters)
# Group Units
COElkSuccessStatewide <- summarise(group_by(COElkSuccessStatewide,Year),
                                   Success = mean(Success))

ggplot(COElkSuccessStatewide, aes(Year,Success)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.16,.25)) +
  labs(title="Statewide Elk Hunter Success", caption="source: cpw.state.co.us")

#' > There is a general trend since 2006 of declining hunter success. 2009 Did have a 'reset' of 
#' success and with the exception of 2015 rates have been decreasing. I should note that 2017 was higher
#' than 2016, hopefully that positive trend continues for 2018.
#' 
#' > **FUTURE** what happened in 2009 and 2015 that increased the success rates from their preceeding years?
#' 
#' > **FUTURE** why are success rates declining overall?
#' 
#' ***
#' #### Success by Season
COElkSuccessStatewide.Season <- summarise(group_by(COElkRifleAll,Season,Unit),
                                          Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T),
                                          Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T),
                                          Success = Harvest / Hunters)
# Group Units
COElkSuccessStatewide.Season <- summarise(group_by(COElkSuccessStatewide.Season,Season),
                                   Success = mean(Success))

ggplot(COElkSuccessStatewide.Season, aes(Season,Success)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  scale_y_continuous(labels = percent) +
  coord_cartesian(ylim = c(.1,.25)) +
  labs(title="Statewide Elk Hunter Success by Season", caption="source: cpw.state.co.us")

#' > First and Fourth seasons have better success rates than the others.
#' 
#' ***
#' ### Hunter Success by Unit
# Recalc success using all the hunter and harvest results
COElkSuccess <- summarise(group_by(COElkRifleAll,Year,Unit),
                          Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T),
                          Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T),
                          Success = Harvest / Hunters)
#' Last year's data
Year2017 <- filter(COElkSuccess, Year == "2017")
HunterSuccesstoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ HunterSuccess-Map, fig.width=10, fig.height=8.46
ggplot(HunterSuccesstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Success),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "RdPu",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2017 Colorado Elk Hunter Success", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ### Year to Year Hunter Success Trends
#' Update fill max
COElkSuccess$fillcolor <- COElkSuccess$Success
maxfillcolor <- boxplot.stats(COElkSuccess$Success)$stats[5] #ignore outliers
#' For charting purposes make all extreme values the same as the statistical max
COElkSuccess$fillcolor[COElkSuccess$fillcolor>maxfillcolor] <- maxfillcolor
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkSuccess$Year)){
  png(file=paste("HunterSuccessMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkSuccess, Year == imap)
  HunterSuccesstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HunterSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = fillcolor),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "RdPu",
                         direction = 1,
                         na.value = 'light grey',
                         name = "Success",
                         limits = c(0,maxfillcolor)) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkSuccess$Year)))) +
    labs(title="Colorado Elk Hunter Success by Year", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}
#' Convert the .png files to one .gif file using ImageMagick 
system("convert -delay 150 *.png HunterSuccessmap.gif")

#' ![](HunterSuccessmap.gif)
#' 
#' > There are pockets of units that are more successful than the rest of the state. See units
#' 2 and 40. Especially in recent years.
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Seasonal Hunter Success Trends
#' Lets look at the changes for each season
COElkSuccessSeason <- summarise(group_by(COElkRifleAll,Season,Unit),
                                Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T),
                                Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T),
                                Success = Harvest / Hunters)

#' Update fill max
COElkSuccessSeason$fillcolor <- COElkSuccessSeason$Success
maxfillcolor <- boxplot.stats(COElkSuccessSeason$Success)$stats[5] #ignore outliers
#' For charting purposes make all extreme values the same as the statistical max
COElkSuccessSeason$fillcolor[COElkSuccessSeason$fillcolor>maxfillcolor] <- maxfillcolor
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkSuccessSeason$Season)){
  png(file=paste("HunterSuccessSeasonMap",imap,".png"), width=948, height=700)
  seasonplot <- filter(COElkSuccessSeason, Season == imap)
  HunterSuccesstoPlot <- left_join(Unitboundaries2,seasonplot, by=c("Unit"))
  p1 <- ggplot(HunterSuccesstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = fillcolor),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "RdPu",
                         direction = 1,
                         na.value = 'light grey',
                         name = 'Success',
                         limits = c(0,maxfillcolor)) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkSuccessSeason$Season)))) +
    labs(title="Colorado Elk Hunter Success by Season", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png HunterSuccessSeasonmap.gif")

#' ![](HunterSuccessSeasonmap.gif)
#' 
#' > For most of the state the higher successes in the first and fourth seasons are apparent.
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Rank Units by overall hunter success
#' Would also be beneficial to rank each unit so I can reference later.
#' I'll average the last few years
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
#' > There are a handful of units that have much better success than the rest of  the state.
#' 
#' ***
#' ### Rank Units with Seasons by overall hunter success
HunterSuccessSeasonRanklast3 <- filter(COElkRifleAll, as.numeric(Year) >= 2015)
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "1"] <- "1st"
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "2"] <- "2nd"
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "3"] <- "3rd"
HunterSuccessSeasonRanklast3$Season[HunterSuccessSeasonRanklast3$Season == "4"] <- "4th"

HunterSuccessSeasonRanklast3$Unit_Season <- paste(HunterSuccessSeasonRanklast3$Unit,HunterSuccessSeasonRanklast3$Season,sep="\n")
HunterSuccessSeasonRanklast3 <- summarise(group_by(HunterSuccessSeasonRanklast3,Unit_Season),
                                          Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T),
                                          Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T),
                                          Success = Harvest / Hunters)

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

#' > TODO - commentary
#' 
#' ***
#' ## Conclusion
#' > First and Fourth seasons have the best success overall.  There are a handful of units that have much better
#' recent success rates than the rest of the state, and for the top few it doesn't even seem season dependent.
#' 
#' > I'd like to look at what it takes to get a license to hunt in those units. I can do that by reviewing past
#' draw results.
#' 
#' > Look at draw results for all seasons of units 2, 40, 61, 9, 201, 20, 10. What is the likelihood of acquiring
#' a license for a Bull and Cow in each of their seasons? 
#' 
#' > Is there a unit, season, or sex that has a better likelihood
#' of license success?