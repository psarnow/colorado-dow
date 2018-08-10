#' ---
#' title: "Predict Number of Future Elk Hunters"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Description
#' Use historical Draw Results, and number of hunters to train a model we can use to 
#' predict the number of future hunters.
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

#' Move back to predictive analytics directory
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

#' Split into train and test sets.. this is a time series dataset (years).
#' Consider using createTimeSlices

#' Do we need to organize this per unit? train a model for each unit?
# traindata <- filter(COElkPopulation, Year != 2017)
# testdata <- filter(COElkPopulation, Year == 2017)

data_index <- sample(1:nrow(COElkHunters),size = .75*nrow(COElkHunters),replace = F)
traindata <- COElkHunters[ data_index, ]
testdata <- COElkHunters[-data_index, ]

#' Save off for importing into AzureML
save(COElkHunters,file="~/_code/colorado-dow/datasets/COElkHunters.RData")
write.csv(COElkHunters,file = "~/_code/colorado-dow/datasets/COElkHunters.csv",row.names = F)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 4, #4
  repeats = 10, #20
  # classProbs = TRUE,
  # savePred = TRUE,
  allowParallel = TRUE,
  summaryFunction = defaultSummary)

HuntersModel = train(Hunters ~ ., data = COElkHunters,
                     method = "cubist", #cubist
                     # preProc = c("center", "scale"), 
                     tuneLength = 10,
                     #tuneGrid = svmTuneGrid,
                     trControl = fitControl)

HuntersModel

#' Important predictors
ImpPred <- varImp(HuntersModel,scale = T)

# check performance
predictdata <- predict(HuntersModel, testdata)

postResample(pred = predictdata, obs = testdata$Hunters)
# svmRadial RMSE=427
# svmLinear RMSE=363
# cubist RMSE=104

#' We can iterate the above model by tweaking preprocessing parameters, and model algorithms.

#' Chart performance of predicted
chartperformance <- data.frame(predicted = predictdata, observed = testdata$Hunters)

ggplot(chartperformance, aes(predicted,observed)) +
  geom_point() +
  labs(title="Performance of Number of Hunters Prediction", caption="source: cpw.state.co.us")

#' Finalize model with full dataset to train on
FinalHuntersmodel = train(Hunters ~ ., data = COElkHunters,
                          method = "cubist",
                          # preProc = c("center", "scale"), 
                          tuneLength = 10,
                          #tuneGrid = svmTuneGrid,
                          trControl = fitControl)

FinalHuntersmodel

#' Important predictors
ImpPred <- varImp(FinalHuntersmodel,scale = T)

#' Use the 2018 Draw data to predict the number of hunters in 2018
COElkDraw2018 <- filter(COElkDraw, Year == 2018)
COElkHunters2018 <- COElkDraw2018[, colnames(COElkDraw2018) %in% c("Unit",FinalHuntersmodel$coefnames)]

COElkHunters2018$Hunters <- predict(FinalHuntersmodel, COElkHunters2018)

COElkHunters2018$Hunters[COElkHunters2018$Hunters<0] <- 0

#' Save off so we don't have to recreate the model everytime we want the results
save(COElkHunters2018,file="COElkHunters2018.RData")
