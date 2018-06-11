setwd("~/_code/colorado-dow/Phase II - Diagnostic Analytics")

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
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)
# COElkDrawAll2

# Unit 77
Draw77 <- filter(COElkDrawAll2, Unit == "77")
Draw77Success <- summarise(group_by(Draw77,Year, Season),
                           Draw_Success = mean(c(Cow.Draw_Success,Either.Draw_Success,Bull.Draw_Success),na.rm = T),
                           Quota = sum(c(Cow.Orig_Quota,Either.Orig_Quota,Bull.Orig_Quota),na.rm = T))

Draw77Success$Draw_Success[Draw77Success$Draw_Success > 1] <- 1

ggplot(Draw77Success, aes(Year,Draw_Success,group=Season,fill=Season)) +
  geom_bar(stat="identity",position='dodge') +
  # geom_point() +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = hcpalette) +
  prettytheme +
  ggtitle("Unit 77 Draw Success")


ggplot(Draw77Success, aes(Year,Quota,group=Season,fill=Season)) +
  geom_bar(stat="identity") +
  # geom_point() +
  # scale_y_continuous(labels = percent) +
  scale_fill_manual(values = hcpalette) +
  prettytheme +
  ggtitle("Unit 77 Draw Quota")


