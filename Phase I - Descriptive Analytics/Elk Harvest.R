#' ---
#' title: "Where are elk harvested in Colorado"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---

#' ## Initial Questions to Explore
#' * I'm wondering how many elk are harvested in each of the units, 
#' I would expect that CPW associates the number of Harvest to how many elk are in each unit.
#' * I am also curious to know if the number harvested has changed from year to year.
#' 
#' Will want to view all elk, antlered, antlerless, and the ratio of each for insights into these questions.
#'
#' *NOTICE* that I am only looking at the general rifle hunting seasons on public land. There are also
#' Harvest in Archery, Muzzleloader, Private Land, Ranching for Wildlife, etc.
#' 
#' ***
#' ### Setup
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T) # data wrangling
library(dplyr,quietly = T) # data wrangling
library(ggplot2, quietly = T) # charting
library(ggthemes,quietly = T) # so I can add the highcharts theme and palette
library(scales,quietly = T) # to load the percent function when labeling plots
#' Set our preferred charting theme
theme_set(theme_minimal()+theme_hc())
#' Run script to get harvest data
#+ source_population, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)

#' Table of the harvest data
COElkRifleAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)
#' ***
#' ## Total Elk Harvest
#' ### Statewide
# Group seasons
COElkHarvestStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                   Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T))
# Group Units
COElkHarvestStatewide <- summarise(group_by(COElkHarvestStatewide,Year),
                                   Harvest = sum(Harvest))

ggplot(COElkHarvestStatewide, aes(Year,Harvest)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(20000,35000)) +
  labs(title="Statewide Elk Harvest", caption="source: cpw.state.co.us")

#' > TODO commentary
#' 
#' ***
#' 
#' ### Harvest by Unit
#' How the Harvest is distributed across the state.
#' 
# Group seasons
COElkUnitHarvest <- summarise(group_by(COElkRifleAll,Year,Unit),
                              Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T))
#' Last year's data
Year2017 <- filter(COElkUnitHarvest, Year == "2017")
HarvesttoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ Harvest-Map, fig.width=10, fig.height=8.46
ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Harvest),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Reds",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2017 Colorado Elk Harvest", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' 
#' ### Year to Year Harvest Trends
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkUnitHarvest$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("HarvestMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkUnitHarvest, Year == imap)
  HarvesttoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Harvest),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Reds",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(COElkUnitHarvest$Harvest))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitHarvest$Year)))) +
    labs(title="Colorado Elk Harvest", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick 
system("convert -delay 150 *.png Harvestmap.gif")

#' ![](Harvestmap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Harvest Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the Harvest of the last few years
HarvestRanklast3 <- filter(COElkUnitHarvest, as.numeric(Year) >= 2015)
HarvestRanklast3 <- summarise(group_by(HarvestRanklast3,Unit),
                             Harvest = mean(Harvest,na.rm = T))
HarvestRanklast3$HarvestRank = rank(-HarvestRanklast3$Harvest)

HarvestRanklast3 <- filter(HarvestRanklast3, HarvestRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
HarvestRanklast3 <- HarvestRanklast3[order(-HarvestRanklast3$Harvest), ]  # sort
HarvestRanklast3$Unit <- factor(HarvestRanklast3$Unit, levels = HarvestRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(HarvestRanklast3, aes(x=Unit, y=Harvest)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Harvest)) + 
  labs(title="Average Elk Harvest 2015-2017\nTop 50 Units", subtitle="Harvest by Unit", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ## Antlered Elk Harvest
#' ### Statewide
# Group seasons
COElkAntHarvestStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                   Antlered_Harvest = sum(c(Harvest.Antlered),na.rm = T))
# Group Units
COElkAntHarvestStatewide <- summarise(group_by(COElkAntHarvestStatewide,Year),
                                      Antlered_Harvest = sum(Antlered_Harvest))

ggplot(COElkAntHarvestStatewide, aes(Year,Antlered_Harvest)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[1]) +
  coord_cartesian(ylim = c(10000,20000)) +
  labs(title="Statewide Antlered Elk Harvest", caption="source: cpw.state.co.us")

#' > TODO commentary
#' 
#' ***
#' 
#' ### Antlered Harvest by Unit
#' How the Antlered Harvest is distributed across the state.
#' 
# Group seasons
COElkUnitAntHarvest <- summarise(group_by(COElkRifleAll,Year,Unit),
                              Antlered_Harvest = sum(c(Harvest.Antlered),na.rm = T))
#' Last year's data
Year2017 <- filter(COElkUnitAntHarvest, Year == "2017")
HarvesttoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ AntHarvest-Map, fig.width=10, fig.height=8.46
ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Antlered_Harvest),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Blues",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2017 Colorado Antlered Elk Harvest", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' 
#' ### Year to Year Antlered Harvest Trends
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkUnitAntHarvest$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("AntHarvestMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkUnitAntHarvest, Year == imap)
  HarvesttoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Antlered_Harvest),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Blues",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(COElkUnitAntHarvest$Antlered_Harvest))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitAntHarvest$Year)))) +
    labs(title="Colorado Antlered Elk Harvest", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick 
system("convert -delay 150 *.png AntHarvestmap.gif")

#' ![](AntHarvestmap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' 
#' ### Antlered Harvest Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the Antlered Harvest of the last few years
AntHarvestRanklast3 <- filter(COElkUnitAntHarvest, as.numeric(Year) >= 2015)
AntHarvestRanklast3 <- summarise(group_by(AntHarvestRanklast3,Unit),
                                 Antlered_Harvest = mean(Antlered_Harvest,na.rm = T))
AntHarvestRanklast3$HarvestRank = rank(-AntHarvestRanklast3$Antlered_Harvest)

AntHarvestRanklast3 <- filter(AntHarvestRanklast3, HarvestRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
AntHarvestRanklast3 <- AntHarvestRanklast3[order(-AntHarvestRanklast3$Antlered_Harvest), ]  # sort
AntHarvestRanklast3$Unit <- factor(AntHarvestRanklast3$Unit, levels = AntHarvestRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(AntHarvestRanklast3, aes(x=Unit, y=Antlered_Harvest)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Antlered_Harvest)) + 
  labs(title="Average Antlered Elk Harvest 2015-2017\nTop 50 Units", subtitle="Harvest by Unit", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ## Antlerless Elk Harvest
#' ### Statewide
# Group seasons
COElkAntlessHarvestStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                      Antlerless_Harvest = sum(c(Harvest.Antlerless),na.rm = T))
# Group Units
COElkAntlessHarvestStatewide <- summarise(group_by(COElkAntlessHarvestStatewide,Year),
                                      Antlerless_Harvest = sum(Antlerless_Harvest))

ggplot(COElkAntlessHarvestStatewide, aes(Year,Antlerless_Harvest)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[3]) +
  # coord_cartesian(ylim = c(10000,20000)) +
  labs(title="Statewide Antlerless Elk Harvest", caption="source: cpw.state.co.us")

#' > TODO commentary
#' 
#' ***
#' 
#' ### Antlerless Harvest by Unit
#' How the Antlerless Harvest is distributed across the state.
#' 
# Group seasons
COElkUnitAntlessHarvest <- summarise(group_by(COElkRifleAll,Year,Unit),
                                 Antlerless_Harvest = sum(c(Harvest.Antlerless),na.rm = T))
#' Last year's data
Year2017 <- filter(COElkUnitAntlessHarvest, Year == "2017")
HarvesttoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ AntlessHarvest-Map, fig.width=10, fig.height=8.46
ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Antlerless_Harvest),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2017 Colorado Antlerless Elk Harvest", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' 
#' ### Year to Year Antlerless Harvest Trends
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkUnitAntlessHarvest$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("AntlessHarvestMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkUnitAntlessHarvest, Year == imap)
  HarvesttoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Antlerless_Harvest),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Greens",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(COElkUnitAntlessHarvest$Antlerless_Harvest))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitAntlessHarvest$Year)))) +
    labs(title="Colorado Antlerless Elk Harvest", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick 
system("convert -delay 150 *.png AntlessHarvestmap.gif")

#' ![](AntlessHarvestmap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' 
#' ### Antlerless Harvest Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the Antlerless Harvest of the last few years
AntlessHarvestRanklast3 <- filter(COElkUnitAntlessHarvest, as.numeric(Year) >= 2015)
AntlessHarvestRanklast3 <- summarise(group_by(AntlessHarvestRanklast3,Unit),
                                 Antlerless_Harvest = mean(Antlerless_Harvest,na.rm = T))
AntlessHarvestRanklast3$HarvestRank = rank(-AntlessHarvestRanklast3$Antlerless_Harvest)

AntlessHarvestRanklast3 <- filter(AntlessHarvestRanklast3, HarvestRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
AntlessHarvestRanklast3 <- AntlessHarvestRanklast3[order(-AntlessHarvestRanklast3$Antlerless_Harvest), ]  # sort
AntlessHarvestRanklast3$Unit <- factor(AntlessHarvestRanklast3$Unit, levels = AntlessHarvestRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(AntlessHarvestRanklast3, aes(x=Unit, y=Antlerless_Harvest)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Antlerless_Harvest)) + 
  labs(title="Average Antlerless Elk Harvest 2015-2017\nTop 50 Units", subtitle="Harvest by Unit", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ## Harvest Ratio (Antlered vs Antlerless)
#' ### Statewide
# Group seasons
COElkHarvestRatioStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                        Antlered_Harvest = sum(Harvest.Antlered,na.rm = T),
                                        Antlerless_Harvest = sum(Harvest.Antlerless,na.rm = T),
                                        AntleredRatio = Antlered_Harvest / (Antlered_Harvest + Antlerless_Harvest))
# Group Units
COElkHarvestRatioStatewide <- summarise(group_by(COElkHarvestRatioStatewide,Year),
                                        AntleredRatio = mean(AntleredRatio,na.rm = T))

ggplot(COElkHarvestRatioStatewide, aes(Year,AntleredRatio)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[6]) +
  scale_y_continuous(labels = percent) +
  ylab("Percent Antlered") +
  coord_cartesian(ylim = c(.5,.7)) +
  labs(title="Statewide Elk Harvest Ratio", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ### Harvest Ratio (Antlered vs Antlerless) by Unit
#' How the Harvest ratio is distributed across the state.
#' 
# Group seasons
COElkHarvestRatio <- summarise(group_by(COElkRifleAll,Year,Unit),
                               Antlered_Harvest = sum(Harvest.Antlered,na.rm = T),
                               Antlerless_Harvest = sum(Harvest.Antlerless,na.rm = T),
                               AntleredRatio = Antlered_Harvest / (Antlered_Harvest + Antlerless_Harvest))
#' Last year's data
Year2017 <- filter(COElkHarvestRatio, Year == "2017")
HarvesttoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))

#+ AntlessHarvestRatio-Map, fig.width=10, fig.height=8.46
ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = AntleredRatio),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Reds",direction = 1,na.value = 'light grey',name='Percent Antlered') +
  xlab("") + ylab("") +
  labs(title="2017 Colorado Elk Harvest Ratio", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' 
#' ### Year to Year Antlerless Harvest Trends
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkHarvestRatio$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("HarvestRatioMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkHarvestRatio, Year == imap)
  HarvesttoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HarvesttoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = AntleredRatio),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Reds",
                         direction = 1,
                         na.value = 'light grey',
                         name = "Percent Antlered",
                         limits = c(0,max(COElkHarvestRatio$AntleredRatio))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkHarvestRatio$Year)))) +
    labs(title="Colorado Elk Harvest Ratio", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick 
system("convert -delay 150 *.png HarvestRatiomap.gif")

#' ![](HarvestRatiomap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' 