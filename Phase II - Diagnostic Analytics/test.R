#' ---
#' title: "Predict Number of Future Elk Hunters"
#' author: "Pierre Sarnow"
#' output:
#'   html_notebook:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Description
#' Use historical draw results, and number of hunters to train a model we can use to 
#' predict the number of hunters in future years.
#' 
#' TODO - Include other potential inputs that could impact how many hunters get a license
#' and show up. Those could include economic indicators, and costs associated with hunting
#' (transportation, lodging).
#'
#' *__NOTICE__ that I am only looking at the general rifle hunting seasons on public land. There are also 
#' hunters for Archery, Muzzleloader, Private Land, Ranching for Wildlife, etc.*
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
library(caret,quietly = T) # classification and regression training
library(foreach,quietly = T) # parallel processing to speed up the model training
library(doMC,quietly = T) # parallel processing to speed up the model training
library(lubridate,quietly = T) # for timing models
#' Set our preferred charting theme
theme_set(theme_minimal()+theme_hc()+theme(legend.key.width = unit(1.5, "cm")))
#' Run script to get hunter data
#+ source_harvest, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)

#' Table of the harvest data
COElkRifleAll

#' Run script to get draw data
#+ source_draw, message=F, warning=F
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)

#' Table of the data
COElkDrawAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)

#' Set to predictive analytics directory
setwd("~/_code/colorado-dow/phase III - predictive analytics")
#' ***
#' ## Organize data
#' Will start by grouping all of the seasons together, and modeling the number of hunters per
#' Year and Unit
#' Group Draw results data by Year and Unit
COElkDraw <- summarise(group_by(COElkDrawAll,Year,Unit),
                       Quota = sum(Orig_Quota,na.rm = T),
                       Drawn = sum(Chcs_Drawn,na.rm = T))

#' Appropriate field classes for model training
COElkDraw$Year <- as.numeric(COElkDraw$Year)

#' Group Hunter data by Year and Unit
COElkHunters <- summarise(group_by(COElkRifleAll,Year,Unit),
                          Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T))

COElkHunters$Year <- as.numeric(COElkHunters$Year)

#' Join in Hunter and Draw data together
COElkHunters <- left_join(COElkHunters, COElkDraw, by = c("Year","Unit"))

#' Replace the draw data that don't have entries with 0
COElkHunters$Drawn[is.na(COElkHunters$Drawn)] <- 0
COElkHunters$Quota[is.na(COElkHunters$Quota)] <- 0

#' Split into train and test sets. Will use 75% of the data to train on. Be sure to include
#' each unit in the split. ... so do the split for each unit, first make sure each Unit has
#' at least three entries
#' 
COElkHunters <- mutate(group_by(COElkHunters,Unit),
                       numentries = n())
COElkHunters <- filter(COElkHunters, numentries >= 3)
COElkHunters$UnitYear <- paste(COElkHunters$Unit, COElkHunters$Year)

traindata <- COElkHunters %>% group_by(Unit) %>% sample_frac(size = .75, replace = F)
testdata <- COElkHunters[!COElkHunters$UnitYear %in% traindata$UnitYear,]

COElkHunters <- select(COElkHunters, -UnitYear, -numentries)

traindata <- select(traindata, -UnitYear, -numentries)
testdata <- select(testdata, -UnitYear, -numentries)

#' Save off for importing into AzureML
write.csv(COElkHunters,file = "~/_code/colorado-dow/datasets/COElkHunters.csv",row.names = F)

#' notice that the number of hunters data is skewed.
ggplot(COElkHunters, aes(Hunters)) + 
  geom_density() +
  xlab("Hunters in Unit") +
  ylab("Number of Units") +
  theme(axis.text.y = element_blank()) +
  labs(title="Distribution of Hunters in each Unit", subtitle="2006-2017", caption="source: cpw.state.co.us")

#' A general rule of thumb to consider is that skewed data whose ratio of the highest value to the 
#' lowest value is greater than 20 have significant skewness. Also, the skewness statistic can be 
#' used as a diagnostic. If the predictor distribution is roughly symmetric, the skewness values 
#' will be close to zero. As the distribution becomes more right skewed, the skewness statistic 
#' becomes larger. Similarly, as the distribution becomes more left skewed, the value becomes negative.
#' Replacing the data with the log, square root, or inverse may help to remove the skew.

#' Example of how BoxCox can redistribute the data
preProcValues2 <- preProcess(as.data.frame(traindata), method = "BoxCox")
trainBC <- predict(preProcValues2, as.data.frame(traindata))

ggplot(trainBC, aes(Hunters)) + 
  geom_density() +
  xlab("BoxCox Hunters in Unit") +
  ylab("Number of Units") +
  theme(axis.text.y = element_blank()) +
  labs(title="BoxCox Distribution of Hunters in each Unit", subtitle="2006-2017", caption="source: cpw.state.co.us")


