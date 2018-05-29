require(dplyr)
require(ggplot2)
# View hunt data 

# prettier chart formatting
defaulttheme <- theme(
  axis.text=element_text(colour="#606060",size=20, family="Muli-Regular"),
  plot.title=element_text(hjust = 0.5,size=30, colour="#333333", family="Muli-Bold"),
  panel.grid.major = element_line(colour = "#d8d8d8"),
  panel.background = element_rect(fill="#ffffff"),
  plot.background = element_rect(fill = "#ffffff"),
  axis.title=element_text(size=20, colour="#707070", family="Muli-Regular"),
  axis.title.x=element_text(vjust=-.3),
  legend.text=element_text(color="#333333",size=20, family="Muli-Regular"),
  legend.background = element_rect(fill='#ffffff'),
  legend.direction = "horizontal", 
  legend.position = "top",
  legend.key = element_rect(fill='#ffffff',colour='#ffffff'),
  panel.grid.minor= element_blank(),
  strip.text = element_text(size = 20,family="Muli-Regular", colour="#333333"),
  strip.background=element_rect(fill="#ffffff", colour="#ffffff")
)
# palette from highcharts
hcpalette <- c('#7cb5ec', '#434348', '#90ed7d', '#f7a35c', '#8085e9', '#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1')


source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)

COElkRifleInspect <- data.frame(COElkRifleAll)
COElkRifleInspect$Success <- as.numeric(COElkRifleInspect$Success)
COElkRifleInspect$Harvest <- as.numeric(COElkRifleInspect$Harvest)

# some basic views

# overall state success by year
COElkRifleYearSuccess <- summarise(group_by(COElkRifleInspect,year),
                                success = mean(Success),
                                harvest = sum(Harvest))

ggplot(COElkRifleYearSuccess, aes(as.character(year),success)) +
  geom_bar(stat="identity") +
  xlab("year") +
  defaulttheme +
  ggtitle("Statewide Rifle Elk Hunting Success")

ggplot(COElkRifleYearSuccess, aes(as.character(year),harvest)) +
  geom_bar(stat="identity") +
  xlab("year") +
  defaulttheme +
  ggtitle("Statewide Rifle Elk Hunting Harvest")

# overall state success by season
COElkRifleSeason <- summarise(group_by(COElkRifleInspect,season),
                                   success = mean(Success),
                                   harvest = sum(Harvest))

ggplot(COElkRifleSeason, aes(as.character(season),success)) +
  geom_bar(stat="identity") +
  xlab("season") +
  defaulttheme +
  ggtitle("Statewide Rifle Elk Hunting Success")

# overall state harvest by season
ggplot(COElkRifleSeason, aes(as.character(season),harvest)) +
  geom_bar(stat="identity") +
  xlab("season") +
  defaulttheme +
  ggtitle("Statewide Rifle Elk Hunting Harvest")

# 