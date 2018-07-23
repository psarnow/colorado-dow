#' ---
#' title: "Elk Populations vs Number of Hunters in Hunting Units"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Initial Questions to Explore
#' Is there a correlation of number of hunters to number of elk in each
#' unit?  If not, I would certainly be interested in which units have the highest ratio of Elk to Hunters
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
#+ source_hunter, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)

#' Table of the data
COElkRifleAll

#' Run script to get elk population data
#+ source_population, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow population estimates.R', echo=F)

#' Table of the data
COElkPopulationAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)
#' ***
#' ## Elk Hunters and Elk Population
#' ### Statewide
#' Summarise Hunters for each Season
COElkHunters <- summarise(group_by(COElkRifleAll,Year,Unit,Season),
                                    Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T))
#' Summarise Hunters for each Unit
COElkHunters.Unit <- summarise(group_by(COElkHunters,Year,Unit),
                          Hunters = sum(Hunters,na.rm = T))
#' Join, fills blanks with NA
COElkHuntersPopulation <- full_join(COElkHunters.Unit,COElkPopulationAll)
#' Remove years with no data
COElkHuntersPopulation <- filter(COElkHuntersPopulation, !is.na(Population.Unit) & !is.na(Hunters))
#' Yearly totals
COElk.Hunters.Population.Statewide <- summarise(group_by(COElkHuntersPopulation,Year),
                                                Hunters = sum(Hunters,na.rm = T),
                                                Population = sum(Population.Unit,na.rm = T))
# Ignore years with missing data
COElk.Hunters.Population.Statewide <- filter(COElk.Hunters.Population.Statewide, Hunters > 0 & Population > 0)

ggplot(COElk.Hunters.Population.Statewide, aes(as.numeric(Year),Population/Hunters)) +
  geom_point(size=2) +
  geom_smooth(span=.75,se=F,size=3,method='loess',color=ggthemes_data$hc$palettes$default[1]) +
  xlab("Year") +
  labs(title="Statewide Ratio of Elk to Hunters",subtitle="by Year", caption="source: cpw.state.co.us")

#' > Statewide there are three distinct trends of the Elk population to Hunters ratio. This could be the result
#' of licensing restrictions put in place by CPW after monitoring the elk population.  
#' * From 2006 to 2008 there were an increasing number of elk per hunter.  
#' * From 2009 to 2013 there were a decreasing number of elk per hunter.  
#' * And from 2013 to 2016 an increasing ratio, though at a different pace of increases from year to year.
#' 
#' ***
#' ### Elk per Hunter by Unit
#' I'd like to know the distribution of Elk per Hunter across the state.
COElkHuntersPopulation$Elk_per_Hunter <- COElkHuntersPopulation$Population.Unit / COElkHuntersPopulation$Hunters
#' Most recent year's data
Year2016 <- filter(COElkHuntersPopulation, Year == "2016")
ElksperHunterstoPlot <- left_join(Unitboundaries2,Year2016, by=c("Unit"))

#+ ElkHunter-Map, fig.width=10, fig.height=8.46
ggplot(ElksperHunterstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Elk_per_Hunter),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Purples",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2016 Colorado Elk per Hunter", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' ### Year to Year Elk per Hunter Trends

#' Update fill max
COElkHuntersPopulation$fillcolor <- COElkHuntersPopulation$Elk_per_Hunter
maxfillcolor <- boxplot.stats(COElkHuntersPopulation$Elk_per_Hunter)$stats[5] #ignore outliers
#' For charting purposes make all extreme values the same as the statistical max
COElkHuntersPopulation$fillcolor[COElkHuntersPopulation$fillcolor>maxfillcolor] <- maxfillcolor
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkHuntersPopulation$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("ElkperHunterMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkHuntersPopulation, Year == imap)
  ElksperHunterstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(ElksperHunterstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = fillcolor),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Purples",
                         direction = 1,
                         na.value = 'light grey',
                         name = "Elk per Hunter",
                         limits = c(0,maxfillcolor)) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkHuntersPopulation$Year)))) +
    labs(title="Colorado Elk per Hunter", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png ElkperHuntermap.gif")

#' ![](ElkperHuntermap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Number of Elk per Hunter Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the Harvest of the last few years
ElksperHunterRanklast3 <- filter(COElkHuntersPopulation, as.numeric(Year) >= 2014)
ElksperHunterRanklast3 <- summarise(group_by(ElksperHunterRanklast3,Unit),
                                    Elk_per_Hunter = mean(Elk_per_Hunter,na.rm = T))
ElksperHunterRanklast3$Elk_per_HunterRank = rank(-ElksperHunterRanklast3$Elk_per_Hunter)

ElksperHunterRanklast3 <- filter(ElksperHunterRanklast3, Elk_per_HunterRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
ElksperHunterRanklast3 <- ElksperHunterRanklast3[order(-ElksperHunterRanklast3$Elk_per_Hunter), ]  # sort
ElksperHunterRanklast3$Unit <- factor(ElksperHunterRanklast3$Unit, levels = ElksperHunterRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(ElksperHunterRanklast3, aes(x=Unit, y=Elk_per_Hunter)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Elk_per_Hunter)) + 
  labs(title="Average Elk per Hunter 2014-2016\nTop 50 Units", subtitle="Elk per Hunter by Unit", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ## Conclusion
#' > There are about a dozen units that have much higher ratios of Elk to Hunter. They could be edge cases though. Maybe
#' they are apart of a herd DAU but most of the herd resides in a neighboring hunt unit.  Maybe these ratios are legit,
#' and they are hard to get a license in (requires many preference points). Maybe many people apply to hunt there, but
#' the draw is limited and there is a low chance of success (requires preference points). Should investigate these top ranked 
#' units. 
#' 
#' > **FUTURE** Phase II Diagnostic question -- Why do Units 140,20,10,851,691,9,471,391,40 have higher Elk to Hunter
#' ratios than other units?
#' 
#' > After looking at this data, I'm also thinking that I'm probably more interested in the success of the hunters regardless
#' of how many elk or how many hunters are in the units. Let's investigate hunt success next.
#' Elk to Hunter ratio is a good indication of how 'busy' it will be out in the forest though. Low
#' ratios would indicate that it would be more likely all of the hunters would pile up at only a few spots to shoot the same elk.