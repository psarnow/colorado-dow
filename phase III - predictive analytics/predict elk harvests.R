#' ---
#' title: "Predict Future Elk Harvests"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Description
#' Use historical harvests, number of hunters, and weather data to predict the harvest for the upcoming hunting seasons.
#'
#' *__NOTICE__ that I am only looking at the general rifle hunting seasons on public land. There are also 
#' harvests for Archery, Muzzleloader, Private Land, Ranching for Wildlife, etc.*
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

#' Run script to get harvest and hunter data
#+ source_harvest, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)

#' Table of the harvest and hunter data
COElkRifleAll

#+ source_geodata, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado GMUnit and Road data.R', echo=F)
#' Take a peak at the boundary data
head(Unitboundaries2)

#' Weather data
load("weatherdata5.RData")
head(weatherdata5)

#' Move back to predictive analytics directory
setwd("~/_code/colorado-dow/phase III - predictive analytics")
#' ***
#' ## Organize data
#' I could also use the harvest and population estimates to create another elk population number, might be useful
#' in predicting. 
#' Because there are some elk not accounted for in DecemberPopulation = JanuaryPopulation - FallHarvest

#' Group weatherdata by year
UnitWeather <- summarise(group_by(weatherdata5,Year,Unit),
                         daily.temperatureHigh = max(daily.temperatureHigh,na.rm = T),
                         daily.temperatureLow = min(daily.temperatureLow,na.rm = T),
                         daily.temperatureMean = mean(daily.temperatureMean,na.rm = T),
                         daily.precipAccumulation = mean(daily.precipAccumulation,na.rm = T),
                         daily.precipType = mean(daily.precipType,na.rm = T),
                         daily.windSpeed = mean(daily.windSpeed,na.rm = T),
                         daily.FullmoonPhase = mean(daily.FullmoonPhase,na.rm = T))

#' Appropriate field classes for model training
UnitWeather$Year <- as.numeric(UnitWeather$Year)

#' Group Harvest and Hunter data by Year and Unit
COElkHarvest <- summarise(group_by(COElkRifleAll,Year,Unit),
                          Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T),
                          Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T))

COElkHarvest$Year <- as.numeric(COElkHarvest$Year)

#' Add dummy variables... previous year's Population, previous year's Harvest
#' The first year of data, won't have anything..oh, but it will if we add the vars before
#' mergining in the weatherdata

#' Join Harvest, Hunter and Weather data
COElkHarvest <- full_join(COElkHarvest, UnitWeather, by = c("Year","Unit"))

#' Remove rows with missing data
COElkHarvest <- filter(COElkHarvest, !is.na(Harvest) & !is.na(daily.temperatureMean) & Year != 2018)

#' Split into train and test sets will use 75% of the data to train on
data_index <- sample(1:nrow(COElkHarvest),size = .75*nrow(COElkHarvest),replace = F)
traindata <- COElkHarvest[ data_index, ]
testdata <- COElkHarvest[-data_index, ]

#' Save off for importing into AzureML
write.csv(COElkHarvest,file = "~/_code/colorado-dow/datasets/COElkHarvest.csv",row.names = F)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 4, #4
  repeats = 10, #20
  # classProbs = TRUE,
  # savePred = TRUE,
  allowParallel = TRUE,
  summaryFunction = defaultSummary)

HarvestModel = train(Harvest ~ ., data = traindata,
                     method = "svmLinear", #svmRadial
                     # preProc = c("center", "scale"), 
                     tuneLength = 13,
                     #tuneGrid = svmTuneGrid,
                     trControl = fitControl)

HarvestModel

#' Important predictors
ImpPred <- varImp(HarvestModel,scale = T)

# check performance
testdata <- filter(testdata, Unit != "128")
predictdata <- predict(HarvestModel, testdata)

postResample(pred = predictdata, obs = testdata$Harvest)
#include UnitPopulations. svmRadial RMSE=57 svmLinear=50 cubist=62
#' We can iterate the above model by tweaking preprocessing parameters, and model algorithms.

#' Chart performance of predicted
chartperformance <- data.frame(predicted = predictdata, observed = testdata$Harvest)

ggplot(chartperformance, aes(predicted,observed)) +
  geom_point() +
  labs(title="Performance of Harvest Prediction", caption="source: cpw.state.co.us")

#' Finalize model with full dataset to train on
FinalHarvestmodel = train(Harvest ~ ., data = COElkHarvest,
                          method = "svmLinear",
                          # preProc = c("center", "scale"), 
                          tuneLength = 13,
                          #tuneGrid = svmTuneGrid,
                          trControl = fitControl)

FinalHarvestmodel
# svmLinear RMSE=80.7
#' Important predictors
ImpPred <- varImp(FinalHarvestmodel,scale = T)

#' Use the forecasted weather data, and the trained model to predict the harvest for 2018
COElkHarvest2018 <- full_join(COElkHunters2018,filter(UnitWeather,Year==2018))
#' Organize
COElkHarvest2018 <- filter(COElkHarvest2018, !is.na(daily.temperatureMean) & !is.na(Hunters))
COElkHarvest2018 <- COElkHarvest2018[, colnames(COElkHarvest2018) %in% c("Unit",FinalHarvestmodel$coefnames)]

COElkHarvest2018$Harvest <- predict(FinalHarvestmodel, COElkHarvest2018)

COElkHarvest2018$Harvest[COElkHarvest2018$Harvest<0] <- 0

#' Save off so we don't have to recreate the model everytime we want the results
save(COElkHarvest2018,file="COElkHarvest2018.RData")