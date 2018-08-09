#' ---
#' title: "What are the harvest ratios of Antlered vs Antlerless elk in Colorado"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---

#' ## Initial Questions to Explore
#' * TODO
#' #'
#' *__NOTICE__ that I am only looking at the general rifle hunting seasons on public land. There are also 
#' hunters for Archery, Muzzleloader, Private Land, Ranching for Wildlife, etc.*
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
theme_set(theme_minimal()+theme_hc()+theme(legend.key.width = unit(1.5, "cm")))
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
#' ## Conclusion
#' > TODO - commentary
#' > TODO - followup questions