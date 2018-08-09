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
# UnitWeather$Unit <- as.factor(UnitWeather$Unit)
COElkPopulationAll$Year <- as.numeric(COElkPopulationAll$Year)
# COElkPopulationAll$Unit <- as.factor(COElkPopulationAll$Unit)
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

COElkPopulation$Unit <- as.factor(COElkPopulation$Unit)
COElkPopulation2018 <- filter(COElkPopulation, Year == 2018)

#' Remove rows with missing data
COElkPopulation <- filter(COElkPopulation, !is.na(Harvest) & !is.na(Population.Unit) & !is.na(daily.temperatureMean) & Year != 2018)
COElkPopulation <- select(COElkPopulation,-DAU,-Population.DAU,-Bull_Ratio,-Num_GMUnits)
COElkPopulation$Unit <- as.character(COElkPopulation$Unit)
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

#' Chart performance of predicted
chartperformance <- data.frame(predicted = predictdata, observed = testdata$Harvest)

ggplot(chartperformance, aes(predicted,observed)) +
  geom_point() +
  labs(title="Performance of Harvest Prediction", caption="source: cpw.state.co.us")

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
test <- COElkPopulation2018[, colnames(COElkPopulation2018) %in% c("Unit",FinalHarvestmodel$coefnames)]

test$Harvest <- predict(FinalHarvestmodel, test)


#' ***
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
#' Note that Harvest is the most important predictor, but we can't predict 2018 population
#' now as we don't have 2018 Harvest results.
#' 
#' We will need to predict 2018 Population or Harvest first... here is where we should probably
#' introduce some dummy variables to make this more possible.

Finalmodel = train(Population.Unit ~ ., data = COElkPopulation,
                  method = "svmRadial",
                  # preProc = c("center", "scale"), 
                  tuneLength = 10,
                  #tuneGrid = svmTuneGrid,
                  trControl = fitControl)

Finalmodel

#' Important predictors
ImpPred <- varImp(Finalmodel,scale = T)




#' ### Statewide
#' Population of the Data Analysis Units (DAU) -- Herd IDs
# Group DAUs
COElkPopulationStatewide <- summarise(group_by(COElkPopulationAll,Year,DAU),
                                      Population.DAU = mean(Population.DAU))
# Group Years
COElkPopulationStatewide <- summarise(group_by(COElkPopulationStatewide,Year),
                                      Population = sum(Population.DAU))

ggplot(COElkPopulationStatewide, aes(Year,Population)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(250000,300000)) +
  labs(title="Statewide Elk Population", caption="source: cpw.state.co.us")
#' > In recent years it appears the state's elk population has stabalized and is close to the 10 year median.
#' 
#' ***
#' 
#' ### Population by Unit
#' I'd like to know where the elk are distributed across the state. I could look at each year
#' but in this case I am only interested in the most recent year with population data.
#' 
# Group Units
COElkUnitPopulation <- summarise(group_by(COElkPopulationAll,Year,Unit),
                                 Population = mean(Population.Unit))

Year2016 <- filter(COElkUnitPopulation, Year == "2016")
PopulationtoPlot <- left_join(Unitboundaries2,Year2016, by=c("Unit"))

#+ Population-Map, fig.width=10, fig.height=8.46
ggplot(PopulationtoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Population),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="2016 Colorado Elk Population", caption="source: cpw.state.co.us")

#' > TODO - commentary
#' 
#' ***
#' #### Alternative mapping option
#+ load package, message=F, warning=F
library(ggmap)
theme_set(theme_classic())

#+ download maps, message=F, warning=F
colorado_stamen_map <- qmap(location=c(lon=-105.54,lat=39.1438), zoom=7, source = "stamen",maptype = "toner")   
#+ Population-MapB, fig.width=10, fig.height=8.46
colorado_stamen_map +
  geom_polygon(data = PopulationtoPlot, aes(long,lat,group=group,fill = Population),colour = "grey50", size = .2,alpha=.5) + #Unit boundaries
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=2) + #Unit labels
  scale_fill_distiller(palette = "Greens",direction = 1,na.value = 'grey') +
  xlab("") + ylab("") +
  theme(panel.background = element_rect(fill='white')) +
  theme(panel.grid.major= element_blank()) +
  theme(panel.grid.minor= element_blank()) +
  labs(title="2016 Colorado Elk Population", caption="source: cpw.state.co.us")

#' > I considered using some real maps like this one, but they get a little busy with more labeling.
#' 
#' Back to our theme
theme_set(theme_minimal()+theme_hc()+theme(legend.key.width = unit(1.5, "cm")))
#' 
#' ***
#' 
#' ### Year to Year Population Trends
#' Lets look at the population changes from year to year.  While we could use some facetting
#' or even grid tools for each year's map, I think it would be easier to visualize with
#' some animation from year to year.  
#' 
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkUnitPopulation$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("PopMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkUnitPopulation, Year == imap)
  PopulationtoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(PopulationtoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Population),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Greens",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(COElkUnitPopulation$Population))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkUnitPopulation$Year)))) +
    labs(title="Colorado Elk Population", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}
#' Convert the .png files to one .gif file using ImageMagick. 
#' The system() function executes the command as if it was done in the terminal. 
#' The -delay flag sets the time between showing the frames, i.e. the speed of the animation.
system("convert -delay 150 *.png Popmap.gif")

#' ![](Popmap.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Population Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the population of the last few years
PopulationRanklast3 <- filter(COElkUnitPopulation, as.numeric(Year) >= 2014)
PopulationRanklast3 <- summarise(group_by(PopulationRanklast3,Unit),
                                 Population = mean(Population,na.rm = T))
PopulationRanklast3$PopulationRank = rank(-PopulationRanklast3$Population)

PopulationRanklast3 <- filter(PopulationRanklast3, PopulationRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
PopulationRanklast3 <- PopulationRanklast3[order(-PopulationRanklast3$Population), ]  # sort
PopulationRanklast3$Unit <- factor(PopulationRanklast3$Unit, levels = PopulationRanklast3$Unit)  # to retain the order in plot.

#' Lollipop Chart
#' Conveys the same information as in bar charts. By reducing the thick bars into thin lines, it reduces the clutter and lays more emphasis on the value. 
#' It looks nice and modern.
ggplot(PopulationRanklast3, aes(x=Unit, y=Population)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Population)) + 
  labs(title="Average Elk Population 2014-2016\nTop 50 Units", subtitle="Population by Unit", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ## Conclusion
#' > There are sizable herds in Southern Colorado (80-82), as well as Southwestern Colorado.
#' The Northwestern herd is large too. The number of elk are less along the Front Range.
#' 
#' > After looking at this data, I'm wondering how many hunters are in each of these areas.
#' I would expect that CPW associates the number of hunters to how many elk are in each unit.