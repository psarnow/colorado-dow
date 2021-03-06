#' ---
#' title: "Use CPW elk hunt statistics to determine which season to hunt in next"
#' author: "Pierre Sarnow"
#' ---

#' ### Question 1
#' I would like to hunt next year and would like to know which season will provide me the 
#' best chance of success for a certain Unit (Unit 77).  How did things go in past years?

setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")

#' Load required libraries for wrangling data and charting
library(dplyr,quietly = T)
library(ggplot2, quietly = T)

#' Prettier chart theme
prettytheme <- theme(
  axis.text=element_text(colour="#606060",family="Muli-Regular"),
  plot.title=element_text(hjust = 0.5,colour="#333333", family="Muli-Bold"),
  panel.grid.major = element_line(colour = "#d8d8d8"),
  panel.background = element_rect(fill="#ffffff"),
  plot.background = element_rect(fill = "#ffffff"),
  axis.title=element_text(colour="#707070", family="Muli-Regular"),
  axis.title.x=element_text(vjust=-.3),
  legend.text=element_text(color="#333333",family="Muli-Regular"),
  legend.background = element_rect(fill='#ffffff'),
  legend.direction = "horizontal", 
  legend.position = "top",
  legend.key = element_rect(fill='#ffffff',colour='#ffffff'),
  panel.grid.minor= element_blank(),
  strip.text = element_text(family="Muli-Regular", colour="#333333"),
  strip.background=element_rect(fill="#ffffff", colour="#ffffff")
)

#' Palette from highcharts
hcpalette <- c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9', '#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1')

#' Run script to get hunt tables
source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)
COElkRifleInspect <- COElkRifleAll

#' First lets look at the entire state as a whole
COElkRifleSuccess <- summarise(group_by(COElkRifleInspect,Year),
                               Success = mean(Success),
                               Harvest_Effort = mean(Harvest_Effort,na.rm = T))
COElkRifleSuccess

ggplot(COElkRifleSuccess, aes(Year,Success)) +
  geom_bar(stat="identity") +
  prettytheme +
  ggtitle("Statewide Rifle Elk Hunting Success")

#' Overall I should expect a success rate of ~20%, and besides 2016 things appear to be consistent.
#' 
#' **FUTURE** question, what happened in 2016 that caused success rates to drop statewide?
#'
#' How about statewide per season?
COElkRifleSuccess1 <- summarise(group_by(COElkRifleInspect,Season),
                               Success = mean(Success),
                               Harvest_Effort = mean(Harvest_Effort,na.rm = T))
COElkRifleSuccess1

ggplot(COElkRifleSuccess1, aes(Season,Success)) +
  geom_bar(stat="identity") +
  prettytheme +
  ggtitle("Statewide Rifle Elk Hunting Success")
#' Looks like the first season has the best success over all of the years,
#' and success drops through the main seasons before improving a bit for the last season.
#'
#' Now to our question. What about Unit 77?
COElkRifleSuccess77 <- filter(COElkRifleInspect, Unit == 77) # filter out Unit 77

ggplot(COElkRifleSuccess77, aes(Season,Success)) +
  geom_bar(stat="identity",position = 'dodge') +
  scale_fill_manual(values = hcpalette) +
  prettytheme +
  ggtitle("Unit 77 Rifle Elk Hunting Success\n2006-2017")

#' Unit 77 has the same trend thru First to Third Seasons, but the Fourth season is the best
#' 
#' Lets see if this is always the case

ggplot(COElkRifleSuccess77, aes(Year,Success,group=Season,fill=Season)) +
  geom_bar(stat="identity",position = 'dodge') +
  scale_fill_manual(values = hcpalette) +
  prettytheme +
  ggtitle("Unit 77 Rifle Elk Hunting Success by Year")

#' Recently, the first and fourth seasons have the highest success. Last year the Fourth Season
#' had the best success rate.
#' 
#' Conversely, the Third season has had the highest success only once, in 2009.
#' 

#' At this point I believe my choice is between First and Fourth season. One of the points of consideration
#' could be the type of weather I enjoy to hunt in. Subjectively, early October is generally much nicer than late November.
#'
#' **FUTURE** question, what weather data can we attach to the hunt units in past years?
#'
#' The hunt tables provide more data as well. Maybe I want to avoid the busy seasons with lots of hunters.
#' Maybe I want to be sure I have access to this unit for all seasons regulated by preference points from CPW.
#'
#' **FUTURE** question, do I have access to preference points required for hunting in certain units per season?
#'
#' Lets look at the how many hunters are in each of these seasons
ggplot(COElkRifleSuccess77, aes(Season,Hunters)) +
  geom_bar(stat="identity",position = 'dodge') +
  scale_fill_manual(values = hcpalette) +
  prettytheme +
  ggtitle("Unit 77 Number of Rifle Hunters")
#' The fourth and first seasons definately have less hunters, but also a shorter Seasons, so its not a true
#' measure of density (busy). Will need to populate the table with Season Duration somehow
#' 
#' **FUTURE** add Season Durations
#' 
#' Success from the CPW tables is merely Harvest / Hunters. But a truer measure might have to do with effort.
#' Recreation Days is an estimate from CPW on how many hunter days were put in the field...regardless of how
#' long they were out there.
#'
#' Maybe a good measure would be how much effort it takes to have a successful result,
#' or Rec Days / Harvest... how much effort to harvest
# Adding this field to the data acquisition script
#' Again, lets start with a statewide summary to view expected results
ggplot(COElkRifleSuccess, aes(Year,Harvest_Effort)) +
  geom_bar(stat="identity") +
  prettytheme +
  ggtitle("Statewide Rifle Elk Hunting Effort Required")
#' Not sure if this provides any important info towards my question. I do note that 2016 required an 
#' unusually amount of extra effort.
#'
#' Unit 77 by season for all years can tell me overall the differences in seasonal effort
ggplot(COElkRifleSuccess77, aes(Season,Harvest_Effort)) +
  geom_bar(stat="identity",position = 'dodge') +
  scale_fill_manual(values = hcpalette) +
  prettytheme +
  ggtitle("Unit 77 Rifle Elk Hunting Effort Required\n2006-2017")
#' In Unit 77, it is clear that the first season will usually take the fewest amount of hunting days to have success.

ggplot(COElkRifleSuccess77, aes(Year,Harvest_Effort,group=Season,fill=Season)) +
  geom_bar(stat="identity",position = 'dodge') +
  scale_fill_manual(values = hcpalette) +
  prettytheme +
  ggtitle("Unit 77 Rifle Elk Hunting Effort Required")
#' In recent years the trend is similar. First has been easier, and then fourth season. 
#' Last year the fourth season required the least amount of effort.
#' 
#' # Conclusion
#' Using the data from these tables it appears that the first season should be my first preference for my 2018 hunt
#' in Unit 77.
#' 
#' However, I have thought of additional questions or pieces to investigate.
#' 
#' Why does the first season have the highest success rate? Why is the required effort to be successful lower in 
#' the first season?

