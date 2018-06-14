#' ---
#' title: "Elk Populations vs Number of Hunters in Hunting Units"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' # Initial Questions to Explore
#' Is there a correlation of number of hunters to number of elk in each
#' unit?  If not, I would certainly be interested in which units have the highest ratio of Elk to Hunters
#'
#' *NOTICE* that I am only looking at the general rifle hunting seasons on public land. There are also
#' hunters in Archery, Muzzleloader, Private Land, Ranching for Wildlife, etc.
#' 
#' ### Setup
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T)
library(dplyr,quietly = T)
library(ggplot2, quietly = T)
#' Set our preferred charting theme
theme_set(theme_minimal())
#' Run script to get hunter data
#+ source hunter data, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)
#' Table of the data
COElkRifleAll
#' Run script to get elk population data
#+ source population, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow population estimates.R', echo=F)
#' Table of the data
COElkPopulationAll

#' Join, fill blanks with NA
COElkHuntersPopulation <- full_join(COElkRifleAll,COElkPopulationAll)
#' Remove years with no data
COElkHuntersPopulation <- filter(COElkHuntersPopulation, !is.na(Population) & !is.na(Hunters))
#' # Statewide Elk Hunters and Elk Population
#' First lets look at the entire state as a whole
COElk.Hunters.Population.Statewide <- summarise(group_by(COElkHuntersPopulation,Year,Unit),
                                   Hunters = sum(Hunters,na.rm = T),
                                   Population = sum(Unit_Pop,na.rm = T))

COElk.Hunters.Population.Statewide <- summarise(group_by(COElkHuntersPopulation,Year),
                                   Hunters = sum(Hunters,na.rm = T),
                                   Population = sum(Population,na.rm = T))

# ignore years with missing data
COElk.Hunters.Population.Statewide <- filter(COElk.Hunters.Population.Statewide, Hunters > 0 & Population > 0)

ggplot(COElk.Hunters.Population.Statewide, aes(Hunters,Population,label=Year)) +
  geom_point(size=1) +
  geom_text(hjust=-.2,size=4) +
  geom_smooth(method = 'lm',se=F) +
  labs(title="Statewide Elk Hunters vs Elk Population",subtitle="by Year", caption="source: cpw.state.co.us")

#' Statewide there is a general relationship to the number of hunters per Elk population. This would make sense
#' as one of the roles of CPW is to regulate the number of hunters to maintain some target Elk Population.
#' 
#' ## Elk per Hunter by Unit
#' I'd like to know the distribution of Elk per Hunter across the state.
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

COElk.Hunters.Population.Unit <- summarise(group_by(COElkHuntersPopulation,Year,Unit),
                                           Hunters = sum(Hunters,na.rm = T),
                                           Population = sum(Unit_Pop,na.rm = T),
                                           Elk_per_Hunter = Population / Hunters)

Year2016 <- filter(COElk.Hunters.Population.Unit, Year == "2016")
ElksperHunterstoPlot <- left_join(Unitboundaries2,Year2016, by=c("Unit"))

#+ ElkHunter-Map, fig.width=10, fig.height=7
ggplot(ElksperHunterstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Elk_per_Hunter),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Purples",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2016 Colorado Elk per Hunter", caption="source: cpw.state.co.us")

#' ## Year to Year Elk per Hunter Trends
#' Lets look at the changes from year to year.

png(file="ElkperHunterMap%02d.png", width=1400, height=1000)
icounter <- 0
for (imap in unique(COElk.Hunters.Population.Unit$Year)){
  yearplot <- filter(COElk.Hunters.Population.Unit, Year == imap)
  ElksperHunterstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(ElksperHunterstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Elk_per_Hunter),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Purples",
                         direction = 1,
                         na.value = 'grey',
                         # breaks=c(1000,2000,3000,4000,5000,6000,7000),
                         limits = c(0,max(COElk.Hunters.Population.Unit$Elk_per_Hunter))) + #fix so each year chart has same color breaks
    xlab("") + 
    ylab("") +
    theme(panel.background = element_rect(fill='white')) +
    theme(panel.grid.major= element_blank()) +
    theme(panel.grid.minor= element_blank()) +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElk.Hunters.Population.Unit$Year)))) +
    labs(title="Colorado Elk per Hunter", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  icounter <- icounter + 1
}
#+ device, message=F, warning=F
dev.off()

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png ElkperHuntermap.gif")

#' cleanup
file.remove(list.files(pattern=".png"))

#' ![Colorado Elk per Hunter by Year](ElkperHuntermap.gif)

#' ## Number of Elk per Hunter Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later.
#' I'll average the last few years since each year isn't consistent
ElksperHunterRanklast3 <- filter(COElk.Hunters.Population.Unit, as.numeric(Year) >= 2014)
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

#' ## Conclusion
#' There are about a dozen units that have much higher ratios of Elk to Hunter. They could be edge cases though. Maybe
#' they are apart of a herd DAU but most of the herd resides in a neighboring hunt unit.  Maybe these ratios are legit,
#' and they are hard to get a license in (requires many preference points). Maybe many people apply to hunt there, but
#' the draw is limited and there is a low chance of success (requires preference points). Should investigate these top ranked 
#' units. 
#' 
#' **FUTURE** Phase II Diagnostic question -- Why do Units 140,20,10,8,471,691,391,851,40 have higher Elk to Hunter
#' ratios than other units?
#' 
#' After looking at this data, I'm also thinking that I'm probably more interested in the success of the hunters regardless
#' of how many elk or how many hunters are in the units. Let's investigate hunt success next.
#'