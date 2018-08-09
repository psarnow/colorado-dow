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
#' * How difficult is it to get a license for each elk type per unit season?
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

test <- left_join(COElkRifleAll,COElkDrawAll)

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
#' ## License for each elk type per unit season
#' # Will need to determine if the hunter needed to draw, or acquire an over-the-counter license
#' 
#' # Join COElkRifleAll, and COElkDrawAll
Licenses <- left_join(COElkRifleAll,COElkDrawAll)
Licenses$Chcs_Drawn.Antlered <- NA
Licenses$Chcs_Drawn.Antlered[Licenses$Sex == "Bull" & !is.na(Licenses$Chcs_Drawn)] <- Licenses$Chcs_Drawn[Licenses$Sex == "Bull" & !is.na(Licenses$Chcs_Drawn)]
Licenses$Chcs_Drawn.Antlerless <- NA
Licenses$Chcs_Drawn.Antlerless[Licenses$Sex == "Cow" & !is.na(Licenses$Chcs_Drawn)] <- Licenses$Chcs_Drawn[Licenses$Sex == "Cow" & !is.na(Licenses$Chcs_Drawn)]
Licenses$Chcs_Drawn.Either <- NA
Licenses$Chcs_Drawn.Either[Licenses$Sex == "Either" & !is.na(Licenses$Chcs_Drawn)] <- Licenses$Chcs_Drawn[Licenses$Sex == "Either" & !is.na(Licenses$Chcs_Drawn)]

#' # For the units that are combined in limited draws, will need to split the draw up using the 
#' # same ratio of hunters in each of those units
#' 
# List of units that are combined in hunt codes
listnames <- c("77","64","NA")
list1 <- c("77","78","771")
list2 <- c("64","65","NA")
list3 <- c("57","58","NA")
list4 <- c("7","8","NA")
list5 <- c("28","37","NA")
list6 <- c("69","84","NA")
list7 <- c("59","581","NA")
list7 <- c("86","691","861")

CombinedUnits <- data.frame(list1,list2,list3,list4,list5,list6,list7)
uniquecombined <- colnames(CombinedUnits)
UnitSpreadAll <- NULL
for (iunit in uniquecombined) { 
  Units <- CombinedUnits[c(iunit)]
  UnitSpread <- Licenses[Licenses$Unit %in% as.character(levels(Units[1,])),]
  
  UnitSpread <- mutate(group_by(UnitSpread,Year, Season),
                       Unit_Spread.Antlered = Hunters.Antlered / sum(Hunters.Antlered,na.rm = T),
                       Unit_Spread.Antlerless = Hunters.Antlerless / sum(Hunters.Antlerless,na.rm = T),
                       Unit_Spread.Either = Hunters.Either / sum(Hunters.Either,na.rm = T),
                       Chcs_Drawn.Antlered = round(Unit_Spread.Antlered * max(Chcs_Drawn.Antlered,na.rm = T),0),
                       Chcs_Drawn.Antlerless = round(Unit_Spread.Antlerless * max(Chcs_Drawn.Antlerless,na.rm = T),0),
                       Chcs_Drawn.Either = round(Unit_Spread.Either * max(Chcs_Drawn.Either,na.rm = T),0)
  )
  # I bet there is a function that does this.. for now we'll use manual indexing
  UnitSpread$Chcs_Drawn.Spread <- NA
  UnitSpread$Chcs_Drawn.Spread[!is.na(UnitSpread$Chcs_Drawn.Antlered)] <- UnitSpread$Chcs_Drawn.Antlered[!is.na(UnitSpread$Chcs_Drawn.Antlered)] 
  UnitSpread$Chcs_Drawn.Spread[!is.na(UnitSpread$Chcs_Drawn.Antlerless)] <- UnitSpread$Chcs_Drawn.Antlerless[!is.na(UnitSpread$Chcs_Drawn.Antlerless)] 
  UnitSpread$Chcs_Drawn.Spread[!is.na(UnitSpread$Chcs_Drawn.Either)] <- UnitSpread$Chcs_Drawn.Either[!is.na(UnitSpread$Chcs_Drawn.Either)] 
  
  UnitSpread$Unit_Spread <- NA
  UnitSpread$Unit_Spread[!is.na(UnitSpread$Unit_Spread.Antlered)] <- UnitSpread$Unit_Spread.Antlered[!is.na(UnitSpread$Unit_Spread.Antlered)] 
  UnitSpread$Unit_Spread[!is.na(UnitSpread$Unit_Spread.Antlerless)] <- UnitSpread$Unit_Spread.Antlerless[!is.na(UnitSpread$Unit_Spread.Antlerless)] 
  UnitSpread$Unit_Spread[!is.na(UnitSpread$Unit_Spread.Either)] <- UnitSpread$Unit_Spread.Either[!is.na(UnitSpread$Unit_Spread.Either)] 
  
  UnitSpread <- select(UnitSpread, -Chcs_Drawn.Antlered:-Unit_Spread.Either)
  UnitSpread$Sex1 <- substring(UnitSpread$HuntCode, 2, 2)

  UnitSpread <- mutate(group_by(UnitSpread,Year, Season,Sex1),
                 Draw_Success.Spread = max(Draw_Success,na.rm=T))

  # Combine
  UnitSpreadAll <- rbind(UnitSpreadAll,UnitSpread)
}
UnitSpreadAll <- select(ungroup(UnitSpreadAll), -Sex1)
UnitSpreadAll$Chcs_Drawn.Spread[is.infinite(UnitSpreadAll$Chcs_Drawn.Spread)] <- NA
UnitSpreadAll$Draw_Success.Spread[is.infinite(UnitSpreadAll$Draw_Success.Spread)] <- NA


# Combine spead units with all of the rest.... filter out spread units
Licenses1 <- Licenses[!Licenses$Unit %in% as.character(levels(unlist(CombinedUnits))),]

Licenses2 <- rbind.fill(Licenses1,UnitSpreadAll)

#' Identify license type (Limited Draw, or Over the Counter (OTC))
Licenses2$License <- NA
Licenses2$License[(!is.na(Licenses2$Chcs_Drawn) | !is.na(Licenses2$Chcs_Drawn.Spread))] <- "Draw"
Licenses2$License[(is.na(Licenses2$Chcs_Drawn) & is.na(Licenses2$Chcs_Drawn.Spread))] <- "OTC"

#' Overall Success = LicenseSuccess * Hunter Success
Licenses2$Draw_Success[Licenses2$Draw_Success>1] <- 1
Licenses2$Draw_Success.Spread[Licenses2$Draw_Success.Spread>1] <- 1
Licenses2$Draw_Success.Spread[is.infinite(Licenses2$Draw_Success.Spread)] <- NA

OverallSuccess <- summarise(group_by(Licenses2,Year, Unit, Season, HuntCode, License),
                            Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T),
                            Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T),
                            HuntSuccess = Harvest / Hunters,
                            LicenseSuccess = mean(c(Draw_Success,Draw_Success.Spread),na.rm = T))

OverallSuccess$LicenseSuccess[OverallSuccess$License == "OTC"] <- 1
OverallSuccess <- mutate(group_by(OverallSuccess,Year, Unit, Season),
                            OverallSuccess = HuntSuccess * LicenseSuccess)
#' ## Conclusion
#' > How about breakdowns for each sex? Bull, Cow, Either. 
#' > What about seeing these for Hunters, Success, Draw, etc
#' This needs some refinements.  A few things come to mind...
#' Some units are combined (77,78,771), 771 doesn't have its own license.
#' Also, draw results don't include over the counter licenses... I think I can determine 
#' how many hunters are using that license based on the harvest tables (where I combined
#' Limited Seasons 'Draw', and success rates 'OTC'?)