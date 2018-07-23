#' ---
#' title: "Elk location and population trends"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Initial Questions to Explore
#' * I would like to know where the elk are distributed across the state, which units have
#' more elk, which units have less elk?
#' * I am also curious to know if the herd sizes have changed from year to year.
#'
#' ***
#' ## Setup
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T) # data wrangling
library(dplyr,quietly = T) # data wrangling
library(ggplot2, quietly = T) # charting
library(ggthemes,quietly = T) # so I can add the highcharts theme and palette
library(scales,quietly = T) # to load the percent function when labeling plots
#' Set our preferred charting theme
theme_set(theme_minimal()+theme_hc()+theme(legend.key.width = unit(1.5, "cm")))
#' Run script to get elk population data
#+ source_population, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow population estimates.R', echo=F)

#' Table of the elk herd data
COElkPopulationAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)
#' ***
#' ## Elk Population
#' ### Statewide
#' Population of the Data Analysis Units (DAU) -- Herd IDs
# Group DAUs
COElkPopulationStatewide <- summarise(group_by(COElkPopulationAll,Year,DAU),
                                      Population.DAU = mean(Population.DAU))
# Group Years
COElkPopulationStatewide <- summarise(group_by(COElkPopulationStatewide,Year),
                                      Population = sum(Population.DAU))

ggplot(COElkPopulationStatewide, aes(Year,Population)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(250000,300000)) +
  labs(title="Statewide Elk Population", caption="source: cpw.state.co.us")
#' > In recent years it appears the state's elk population has stabalized and is close to the 10 year median.
#' 
#' ***
#' 
#' ### Population by Unit
#' I'd like to know where the elk are distributed across the state. I could look at each year
#' but in this case I am only interested in the most recent year with population data.
#' 
# Group Units
COElkUnitPopulation <- summarise(group_by(COElkPopulationAll,Year,Unit),
                                 Population = mean(Population.Unit))

Year2016 <- filter(COElkUnitPopulation, Year == "2016")
PopulationtoPlot <- left_join(Unitboundaries2,Year2016, by=c("Unit"))

#+ Population-Map, fig.width=10, fig.height=8.46
ggplot(PopulationtoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Population),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2016 Colorado Elk Population", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' #### Alternative mapping option
#+ load package, message=F, warning=F
library(ggmap)
theme_set(theme_classic())

#+ download maps, message=F, warning=F
colorado_stamen_map <- qmap(location=c(lon=-105.54,lat=39.1438), zoom=7, source = "stamen",maptype = "toner")   
#+ Population-MapB, fig.width=10, fig.height=8.46
colorado_stamen_map +
  geom_polygon(data = PopulationtoPlot, aes(long,lat,group=group,fill = Population),colour = "grey50", size = .2,alpha=.5) + #Unit boundaries
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=2) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'grey') +
  xlab("") + ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2016 Colorado Elk Population", caption="source: cpw.state.co.us")

#' > I considered using some real maps like this one, but they get a little busy with more labeling.
#' 
#' Back to our theme
theme_set(theme_minimal()+theme_hc()+theme(legend.key.width = unit(1.5, "cm")))
#' 
#' ***
#' 
#' ### Year to Year Population Trends
#' Lets look at the population changes from year to year.  While we could use some facetting
#' or even grid tools for each year's map, I think it would be easier to visualize with
#' some animation from year to year.  
#' 
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkUnitPopulation$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("PopMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkUnitPopulation, Year == imap)
  PopulationtoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(PopulationtoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Population),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Greens",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(COElkUnitPopulation$Population))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitPopulation$Year)))) +
    labs(title="Colorado Elk Population", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}
#' Convert the .png files to one .gif file using ImageMagick. 
#' The system() function executes the command as if it was done in the terminal. 
#' The -delay flag sets the time between showing the frames, i.e. the speed of the animation.
system("convert -delay 150 *.png Popmap.gif")

#' ![](Popmap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Population Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the population of the last few years
PopulationRanklast3 <- filter(COElkUnitPopulation, as.numeric(Year) >= 2014)
PopulationRanklast3 <- summarise(group_by(PopulationRanklast3,Unit),
                                 Population = mean(Population,na.rm = T))
PopulationRanklast3$PopulationRank = rank(-PopulationRanklast3$Population)

PopulationRanklast3 <- filter(PopulationRanklast3, PopulationRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
PopulationRanklast3 <- PopulationRanklast3[order(-PopulationRanklast3$Population), ]  # sort
PopulationRanklast3$Unit <- factor(PopulationRanklast3$Unit, levels = PopulationRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
#' Conveys the same information as in bar charts. By reducing the thick bars into thin lines, it reduces the clutter and lays more emphasis on the value. 
#' It looks nice and modern.
ggplot(PopulationRanklast3, aes(x=Unit, y=Population)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Population)) + 
  labs(title="Average Elk Population 2014-2016\nTop 50 Units", subtitle="Population by Unit", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ## Conclusion
#' > There are sizable herds in Southern Colorado (80-82), as well as Southwestern Colorado.
#' The Northwestern herd is large too. The number of elk are less along the Front Range.
#' 
#' > After looking at this data, I'm wondering how many hunters are in each of these areas.
#' I would expect that CPW associates the number of hunters to how many elk are in each unit.