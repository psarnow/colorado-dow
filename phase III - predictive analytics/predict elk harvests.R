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
#' ## Initial Questions to Explore
#' * I'm wondering how many elk are harvested in each of the units, 
#' I would expect that CPW associates the number of Harvest to how many elk are in each unit.
#' * I am also curious to know if the number harvested has changed from year to year.
#' 
#' Will want to view all elk, antlered, antlerless, and the ratio of each for insights into these questions.
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
#' Run script to get elk population data
#+ source_population, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Population Estimates.R', echo=F)

#' Table of the elk herd data
COElkPopulationAll

#' Run script to get harvest data
#+ source_harvest, message=F, warning=F
source('~/_code/colorado-dow/datasets/Colorado Elk Harvest Data.R', echo=F)

#' Table of the harvest data
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
#' * Should I use DAUs or Units to group the elk herds?
#' * Do I need to group by seasons or years?
#' * When I attach weather data, should I group by season or make the weather data summarized by year?
#' 
#' I could group by Unit and Year. So I will need to summarize the harvest accordingly. 
#' I will also group weather data by year, but will calculate some statistics using season data. Such as
#' maximums, minimums, standard deviations, etc for more descriptive yearly weather data by Unit.
#' 
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
COElkPopulationAll$Year <- as.numeric(COElkPopulationAll$Year)
COElkPopulationAll$Unit <- as.character(COElkPopulationAll$Unit)
#' Join Population and weather data
COElkPopulation <- full_join(COElkPopulationAll, UnitWeather, by = c("Year","Unit"))

#' Group Harvest data by Year and Unit
COElkHarvest <- summarise(group_by(COElkRifleAll,Year,Unit),
                          Harvest = sum(c(Harvest.Antlered,Harvest.Antlerless),na.rm = T))
COElkHarvest$Year <- as.numeric(COElkHarvest$Year)
# COElkHarvest$Unit <- as.factor(COElkHarvest$Unit)
#' Add dummy variables... previous year's Population, previous year's Harvest
#' The first year of data, won't have anything..oh, but it will if we add the vars before
#' mergining in the weatherdata

#' Join in Harvest data
COElkPopulation <- left_join(COElkPopulation, COElkHarvest, by = c("Year","Unit"))

#' save off the 2018 data for predicting after we have a trained model
COElkHarvest2018 <- filter(COElkPopulation, Year == 2018)

#' Remove rows with missing data
COElkPopulation <- filter(COElkPopulation, !is.na(Harvest) & !is.na(Population.Unit) & !is.na(daily.temperatureMean) & Year != 2018)
COElkPopulation <- select(COElkPopulation,-DAU,-Population.DAU,-Bull_Ratio,-Num_GMUnits)

#' 
#' Split into train and test sets.. this is a time series dataset (years).
#' Consider using createTimeSlices

#' Do we need to organize this per unit? train a model for each unit?
# traindata <- filter(COElkPopulation, Year != 2017)
# testdata <- filter(COElkPopulation, Year == 2017)

data_index <- sample(1:nrow(COElkPopulation),size = .75*nrow(COElkPopulation),replace = F)
traindata <- COElkPopulation[ data_index, ]
testdata <- COElkPopulation[-data_index, ]

#' Save off for importing into AzureML
save(COElkPopulation,file="~/_code/colorado-dow/datasets/COElkPopulation.RData")
write.csv(COElkPopulation,file = "~/_code/colorado-dow/datasets/COElkPopulation.csv",row.names = F)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 4, #4
  repeats = 10, #20
  # classProbs = TRUE,
  # savePred = TRUE,
  allowParallel = TRUE,
  summaryFunction = defaultSummary)

HarvestModel = train(Harvest ~ ., data = select(traindata,-Population.Unit),
                     method = "svmRadial", #svmRadial
                     # preProc = c("center", "scale"), 
                     tuneLength = 10,
                     #tuneGrid = svmTuneGrid,
                     trControl = fitControl)

HarvestModel

#' Important predictors
ImpPred <- varImp(HarvestModel,scale = T)

# check performance
predictdata <- predict(HarvestModel, testdata)

postResample(pred = predictdata, obs = testdata$Harvest)
#include UnitPopulations. svmRadial RMSE=57
#' We can iterate the above model by tweaking preprocessing parameters, and model algorithms.

#' Chart performance of predicted
chartperformance <- data.frame(predicted = predictdata, observed = testdata$Harvest)

ggplot(chartperformance, aes(predicted,observed)) +
  geom_point() +
  labs(title="Performance of Harvest Prediction", caption="source: cpw.state.co.us")

#' Finalize model with full dataset to train on
FinalHarvestmodel = train(Harvest ~ ., data = select(COElkPopulation,-Population.Unit),
                          method = "svmRadial",
                          # preProc = c("center", "scale"), 
                          tuneLength = 10,
                          #tuneGrid = svmTuneGrid,
                          trControl = fitControl)

FinalHarvestmodel

#' Important predictors
ImpPred <- varImp(FinalHarvestmodel,scale = T)

#' Use the forecasted weather data, and the trained model to predict the harvest for 2018
COElkHarvest2018 <- COElkHarvest2018[, colnames(COElkHarvest2018) %in% c("Unit",FinalHarvestmodel$coefnames)]

#' For some reason we didn't have historic weather for these units, so remove them from the 2018 forecast
COElkHarvest2018 <- filter(COElkHarvest2018, Unit != "106" & Unit != "121" & Unit != "122" & Unit != "123" & Unit != "128"
                           & Unit != "138" & Unit != "682" & Unit != "88" & Unit != "94")

COElkHarvest2018$Harvest <- predict(FinalHarvestmodel, COElkHarvest2018)

COElkHarvest2018$Harvest[COElkHarvest2018$Harvest<0] <- 0

#' Save off so we don't have to recreate the model everytime we want the results
save(COElkHarvest2018,file="COElkHarvest2018.RData")