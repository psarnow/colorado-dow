#' ---
#' title: "Predict future Elk populations"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     toc: true
#'     fig_width: 10
#'     df_print: paged
#' ---
#' ***
#' ## Description
#' * I will use historic herd population estimates provided by CPW, historical elk harvest,
#' and as well as weather data  (historical and forecasted), to predict future elk populations
#' by Unit.
#' 
#' The models will be first built in RStudio, and then ported into Microsoft AzureML for greater automation and web servicing.
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

COElkHarvest2018$Harvest[COElkHarvest2018$Harvest<0] <- 0

#' ***
#' Add forecasted 2018 Harvest to dataset so that we can build and train a model for Population.  
#' Then we will have the ability to predict the Post Harvest Population for 2018



PopModel = train(Population.Unit ~ ., data = traindata,
                  method = "svmRadial",
                  # preProc = c("center", "scale"), 
                  tuneLength = 10,
                  #tuneGrid = svmTuneGrid,
                  trControl = fitControl)

PopModel

#' Important predictors
ImpPred <- varImp(PopModel,scale = T)

# check performance
predictdata <- predict(PopModel, testdata)

postResample(pred = predictdata, obs = testdata$Population.Unit)

#' Chart performance of predicted
chartperformance <- data.frame(predicted = predictdata, observed = testdata$Population.Unit)

ggplot(chartperformance, aes(predicted,observed)) +
  geom_point() +
  labs(title="Performance of Population Prediction", caption="source: cpw.state.co.us")


#' What are some ways we can improve?
#' Different models
#' Tune model parameters
#' Introduce dummy variables
#' Other preprocessing functions
#' Add more historic weather data (we have hunt data back to 2005)


#' Lets move on... retrain with all of the data so we can predict 2018
#' Note that Harvest is the most important predictor, use the predicted Harvest for 2018 from the Harvest model
#' 

FinalPopmodel = train(Population.Unit ~ ., data = COElkPopulation,
                  method = "svmRadial",
                  # preProc = c("center", "scale"), 
                  tuneLength = 10,
                  #tuneGrid = svmTuneGrid,
                  trControl = fitControl)

FinalPopmodel

#' Important predictors
ImpPred <- varImp(FinalPopmodel,scale = T)

#' either source the script that will predict 2018 Harvest, or load the output
if (is.null(COElkHarvest2018)) {
  source('~/_code/colorado-dow/Phase III - predictive analytics/predict elk harvests.R', echo=F)
} else {
  load("~/_code/colorado-dow/Phase III - predictive analytics/COElkHarvest2018.RData")
}

#' Use the forecasted weather data, predicted harvest, and the trained model to predict the population for 2018
COElkPopulation2018 <- COElkHarvest2018[, colnames(COElkHarvest2018) %in% c("Unit",FinalPopmodel$coefnames)]

#' For some reason we didn't have historic weather for these units, so remove them from the 2018 forecast
# COElkPopulation2018 <- filter(COElkPopulation2018, Unit != "106" & Unit != "121" & Unit != "122" & Unit != "123" & Unit != "128"
#                            & Unit != "138" & Unit != "682" & Unit != "88" & Unit != "94")

COElkPopulation2018$Population.Unit <- predict(FinalPopmodel, COElkPopulation2018)

COElkPopulation2018$Population.Unit[COElkPopulation2018$Population.Unit<0] <- 0


