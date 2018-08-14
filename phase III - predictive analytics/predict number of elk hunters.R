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

#' Split into train and test sets will use 75% of the data to train on
data_index <- sample(1:nrow(COElkHunters),size = .75*nrow(COElkHunters),replace = F)
traindata <- COElkHunters[ data_index, ]
testdata <- COElkHunters[-data_index, ]

#' Save off for importing into AzureML
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
                     trControl = fitControl)

HuntersModel

#' Important predictors
ImpPred <- varImp(HuntersModel,scale = T)
ImpPred

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

#' After final model type and tuning params have been identified, run the full dataset to utilize in 
#' predicting future data.

#' Finalize model with full dataset to train on
FinalHuntersmodel = train(Hunters ~ ., data = COElkHunters,
                          method = "cubist",
                          # preProc = c("center", "scale"), 
                          tuneLength = 10,
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

#' ***
#' ## Total Elk Harvest
#' ### Statewide
# Group seasons
COElkHuntersStatewide <- summarise(group_by(COElkRifleAll,Year,Unit),
                                  Hunters = sum(c(Hunters.Antlered,Hunters.Antlerless,Hunters.Either),na.rm = T))
COElkHunters2018b <- COElkHunters2018
COElkHunters2018b$Year <- as.character(COElkHunters2018b$Year)

#' Join 2018 to historic data
COElkHuntersAll <- rbind.fill(COElkHuntersStatewide,COElkHunters2018b)

# Group Units
COElkHuntersStatewide <- summarise(group_by(COElkHuntersAll,Year),
                                   Hunters = sum(Hunters))

ggplot(COElkHuntersStatewide, aes(Year,Hunters)) +
  geom_bar(stat="identity",fill=ggthemes_data$hc$palettes$default[2]) +
  coord_cartesian(ylim = c(110000,160000)) +
  labs(title="Statewide Elk Hunters", caption="source: cpw.state.co.us")

#' > TODO commentary
#' 
#' ***
#' 
#' ### Hunters by Unit
#' I'd like to know where the hunters are distributed across the state.
#' 
#' Next year's data
Year2018 <- filter(COElkHuntersAll, Year == "2018")
HunterstoPlot <- left_join(Unitboundaries2,Year2018, by=c("Unit"))

#+ Hunters-Map, fig.width=10, fig.height=8.46
ggplot(HunterstoPlot, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = Hunters),colour = "grey50", size = .2) + #Unit boundaries
  geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
  geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=3) + #Unit labels
  scale_fill_distiller(palette = "Oranges",direction = 1,na.value = 'light grey') +
  xlab("") + ylab("") +
  labs(title="Predicted 2018 Colorado Elk Hunters", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' 
#' ### Year to Year Hunter Trends
#' Create a png of each year
icounter <- 0
for (imap in unique(COElkHuntersAll$Year)){
  # Colorado aspect ratio = 1087w x 800h -> 1.35875
  # Use trial and error to determine which width and height to define for png files that will retain the correct aspect ratio
  png(file=paste("HuntersMap",imap,".png"), width=948, height=700)
  yearplot <- filter(COElkHuntersAll, Year == imap)
  HunterstoPlot <- left_join(Unitboundaries2,yearplot, by=c("Unit"))
  p1 <- ggplot(HunterstoPlot, aes(long, lat, group = group)) + 
    geom_polygon(aes(fill = Hunters),colour = "grey50", size = .2) + #Unit boundaries
    geom_path(data = COroads,aes(x = long, y = lat, group = group), color="#3878C7",size=2) + #Roads
    geom_text(data=data_centroids,aes(x=longitude,y=latitude,label = Unit),size=5) + #Unit labels
    scale_fill_distiller(palette = "Oranges",
                         direction = 1,
                         na.value = 'light grey',
                         limits = c(0,max(COElkHuntersAll$Hunters))) + #fix so each year chart has same color breaks
    xlab("") + ylab("") +
    theme(plot.title=element_text(hjust = .5)) +
    theme(plot.subtitle=element_text(hjust = icounter/length(unique(COElkHuntersAll$Year)))) +
    labs(title="Colorado Elk Hunters", subtitle=imap, caption="source: cpw.state.co.us")
  plot(p1)
  dev.off()
  icounter <- icounter + 1
}

#' Convert the .png files to one .gif file using ImageMagick. 
system("convert -delay 150 *.png HuntersmapPred.gif")

#' ![](HuntersmapPred.gif)
#' 
#' > TODO - commentary
#' 
#' Remove the .png files
file.remove(list.files(pattern=".png"))
#' ***
#' ### Number of Hunters Rank of the Units
#' Would also be beneficial to rank each unit so I can reference later. In this case
#' average the number of hunters of the last few years
HunterRank2018 <- filter(COElkHuntersAll, as.numeric(Year) == 2018)
HunterRank2018 <- summarise(group_by(HunterRank2018,Unit),
                             Hunters = mean(Hunters,na.rm = T))
HunterRank2018$HuntersRank = rank(-HunterRank2018$Hunters)

HunterRank2018 <- filter(HunterRank2018, HuntersRank <= 50) # top 50 units
#' In order for the chart to retain the order of the rows, the X axis variable (i.e. the categories) has to be converted into a factor.
HunterRank2018 <- HunterRank2018[order(-HunterRank2018$Hunters), ]  # sort
HunterRank2018$Unit <- factor(HunterRank2018$Unit, levels = HunterRank2018$Unit)  # to retain the order in plot.

#' Lollipop Chart
ggplot(HunterRank2018, aes(x=Unit, y=Hunters)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Unit, 
                   xend=Unit, 
                   y=0, 
                   yend=Hunters)) + 
  labs(title="Elk Hunters 2018\nTop 50 Units", subtitle="Hunters by Unit", caption="source: cpw.state.co.us")
#' > TODO - commentary
#' 
#' ***
#' ## Conclusion
#' > TODO
#'