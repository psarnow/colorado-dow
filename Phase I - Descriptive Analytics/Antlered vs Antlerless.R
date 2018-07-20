#' ---
#' title: "Hunting Statistics of Antlered vs Anterless Elk"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' # Initial Questions to Explore
#' Season, Unit, and Year differences between Antlered and Antlerless Elk. Consider...
#' Ability to acquire licenses, Success rates, Hunter density
#' 
#' ### Setup
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T)
library(dplyr,quietly = T)
library(ggplot2,quietly = T)
library(ggthemes,quietly = T)
library(scales,quietly = T)
library(tidyr,quietly = T)
#' Set our preferred charting theme
theme_set(theme_minimal())
#' Run script to get hunter data
#+ source hunter data, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)
#' Table of the data
COElkRifleAll
#' Run script to get draw data
#+ source draw data, message=F, warning=F
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)
#' Table of the data
COElkDrawAll2

#' # Statewide Elk Hunter Success (Antlered, Antlerless, Either)
#' ### By Year
#' First lets look at the entire state as a whole
COElkSuccessStatewideAnttype <- summarise(group_by(COElkRifleAll,Year,Unit),
                                   Antlered = mean(Success.Antlered,na.rm = T),
                                   Antlerless = mean(Success.Antlerless,na.rm = T),
                                   Either = mean(Success.Either,na.rm = T))

COElkSuccessStatewideAnttype <- gather(COElkSuccessStatewideAnttype,"AntlerType",Success,Antlered,Antlerless,Either)

COElkSuccessStatewideAnttype <- summarise(group_by(COElkSuccessStatewideAnttype,Year,AntlerType),
                                   Success = mean(Success,na.rm = T))

ggplot(COElkSuccessStatewideAnttype, aes(Year,Success/100,group=AntlerType,fill=AntlerType)) +
  theme_hc() +
  geom_bar(stat="identity",position='dodge') +
  ylab("Success") +
  scale_fill_hc(name="") +
  scale_y_continuous(labels = percent) +
  labs(title="Statewide Elk Hunter Success", caption="source: cpw.state.co.us")

#' TODO a geo map that utilizes fillcolor and perhaps some other chart types
#' to display the breakdowns of Bull vs Cow vs Either (Hunters,Harvest,Success,DrawSuccess)
COElkSuccessStatewideAnttype <- summarise(group_by(COElkRifleAll,Year,Unit),
                                          Antlered = mean(Success.Antlered,na.rm = T),
                                          Antlerless = mean(Success.Antlerless,na.rm = T),
                                          Either = mean(Success.Either,na.rm = T))

COElkSuccessStatewideAnttype <- gather(COElkSuccessStatewideAnttype,"AntlerType",Success,Antlered,Antlerless,Either)

Year2017 <- filter(COElkSuccess, Year == "2017")
HunterSuccesstoPlot <- left_join(Unitboundaries2,Year2017, by=c("Unit"))
data_centroids_Success <- left_join(data_centroids,COElkSuccessStatewideAnttype, by=c("Unit"))

#+ HunterSuccess-Map, fig.width=10, fig.height=7
ggplot(HunterSuccesstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Success),colour = "grey50", size = .2) + #Unit boundaries
  # geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  # geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  geom_point(data=data_centroids_Success,aes(x=longitude,y=latitude,size=Success,group=AntlerType,color=AntlerType),position = 'dodge') +
  scale_fill_distiller(palette = "RdPu",direction = 1,na.value = 'grey') +
  xlab("") + 
  ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2017 Colorado Elk Hunter Success", caption="source: cpw.state.co.us")





# Remove Inf
COElkDraw <- filter(COElkDrawAll, is.finite(Draw_Success) & !is.na(Draw_Success))
# Cap success to 100%
COElkDraw$Draw_Success[COElkDraw$Draw_Success>1] <- 1.00
#' ### By Year
DrawSuccessStatewide <- summarise(group_by(COElkDraw,Year),
                                  Draw_Success = mean(Draw_Success,na.rm = T))
