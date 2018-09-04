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

#' Split into train and test sets will use 75% of the data to train on. Be sure to include
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
# TODO, chart of hunter population density/histogram

#' A general rule of thumb to consider is that skewed data whose ratio of the highest value to the 
#' lowest value is greater than 20 have significant skewness. Also, the skewness statistic can be 
#' used as a diagnostic. If the predictor distribution is roughly symmetric, the skewness values 
#' will be close to zero. As the distribution becomes more right skewed, the skewness statistic 
#' becomes larger. Similarly, as the distribution becomes more left skewed, the value becomes negative.
#' Replacing the data with the log, square root, or inverse may help to remove the skew.
#' caret has a preproccess function for correcting for skewness 'BoxCox'.

preProcValues2 <- preProcess(as.data.frame(traindata), method = "BoxCox")
trainBC <- predict(preProcValues2, as.data.frame(traindata))
testBC <- predict(preProcValues2, as.data.frame(testdata))
preProcValues2

#' This is quite an iterative process. It is important to document and save off data.
#' Run thru differing 'quick to train' methods
#' Which one performed the best?
#' Determine other similar methods and run them.
#' Which one performed the best?
#' Determine disimmilar methods.
#' Which one performed the best?
#' 
#' Now lets work on some refined tuning.
#' Assess preprocessing functions. center, scale, pca, boxcox, nzv, etc
#' #' some units have very little data, should we remove them? 'zero variances
#' 123, 791, 87, 94, 88
#' TODO, frequency plot of Unit

#' Run the list of other favorable methods with preprocessing in place
#' Further tune method's parameters
#' 
#' Is there a method that is best?  Are some methods better at part of the data
#' than others? maybe we combine?
#' 
#' Insert into AzureML to further inspections and create into a webservice
#' If the packages or methods are not yet supported in Azure, we will need to create an R Model
#' instead of just running an rscript.
#' 
#' Additonally, AzureML has some additional methods to consider, ensure we attempt to use those as well.
#' 
#' 
#' Step 1 - Loop through possible methods, utilizing the quicker 'adaptive_cv' parameter search from caret.
#' # Consider scripting this into AzureML to make it run much faster, though there is more setup and errors to 
#' control for
quickmethods <- c("lm",'svmLinear',"svmRadial","knn","cubist","kknn","glm.nb")

step1_all <- NULL
for (imethod in quickmethods) {
  step1 <- NULL
  start <- now()
  
  if (imethod == "lm") {
    controlmethod <- "repeatedcv"
  } else {controlmethod <- "adaptive_cv"}
  
  fitControl <- trainControl(
    method = controlmethod,
    # search = 'random',
    number = 4,
    repeats = 4,
    allowParallel = TRUE,
    summaryFunction = defaultSummary)
  
  registerDoSEQ()
  registerDoMC(cores = 6)
  
  HuntersModel_1 = train(Hunters ~ ., data = traindata,
                       method = imethod,
                       preProc = c("center","scale"), 
                       tuneLength = 15,
                       trControl = fitControl)
  
  HuntersModel_1
  
  # measure performance
  predictdata <- predict(HuntersModel_1, testdata)
  
  step1$method <- imethod
  step1$RMSE <- postResample(pred = predictdata, obs = testdata$Hunters)[1]
  step1$duration <- now() - start
  step1 <- as.data.frame(step1)
  step1_all <- rbind(step1_all,step1)
}
step1_all

top_two_models <- top_n(step1_all,2,-RMSE)$method
#' Take the top two and determine some additonal methods to try by maximizing the Jaccard
#' dissimilarity between sets of models
tag <- read.csv("tag_data.csv", row.names = 1)
tag <- as.matrix(tag)

## Select only models for regression
regModels <- tag[tag[,"Regression"] == 1,]

all <- 1:nrow(regModels)
dissimilarmethods_all <- NULL
for (itoptwo in 1:2) {
  ## Seed the analysis with the model of interest
  start <- grep(top_two_models[itoptwo], rownames(regModels), fixed = TRUE)
  pool <- all[all != start]
  
  ## Select 4 model models by maximizing the Jaccard
  ## dissimilarity between sets of models
  nextMods <- maxDissim(regModels[start,,drop = FALSE], 
                        regModels[pool, ], 
                        method = "Jaccard",
                        n = 4)
  
  rownames(regModels)[c(nextMods)]
  
  dissimilarmethods <- rownames(regModels)[nextMods]
  dissimilarmethods <- str_extract(string = dissimilarmethods,pattern = "[:alnum:]+(?=\\))")
  dissimilarmethods_all <- c(dissimilarmethods_all,dissimilarmethods)
}

dissimilarmethods_all <- unique(dissimilarmethods_all)

#' Now we have 8 more methods to try in the same manner

for (imethod in dissimilarmethods_all) {
  step1 <- NULL
  start_timer <- now()[1]
  
  if (imethod == "lm") {
    controlmethod <- "repeatedcv"
  } else {controlmethod <- "adaptive_cv"}
  
  fitControl <- trainControl(
    method = controlmethod,
    # search = 'random',
    number = 4,
    repeats = 4,
    allowParallel = TRUE,
    summaryFunction = defaultSummary)
  
  registerDoSEQ()
  registerDoMC(cores = 6)
  
  HuntersModel_1 = train(Hunters ~ ., data = traindata,
                         method = imethod,
                         preProc = c("center","scale"), 
                         tuneLength = 15,
                         trControl = fitControl)
  
  HuntersModel_1
  
  # measure performance
  predictdata <- predict(HuntersModel_1, testdata)
  
  step1$method <- imethod
  step1$RMSE <- postResample(pred = predictdata, obs = testdata$Hunters)[1]
  step1$duration <- now()[1] - start_timer[1]
  step1 <- as.data.frame(step1)
  step1_all <- rbind(step1_all,step1)
}
step1_all


#' Now lets work on some refined tuning on the top methods
#' Any valuable preprocessing steps?
preprocessfunctions <- c("BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range", "knnImpute", "bagImpute", "medianImpute", "pca", "ica", "spatialSign", "corr", "zv", "nzv")
topmethods <- top_n(step1_all,2,-RMSE)$method

fitControl <- trainControl(
  method = "adaptive_cv", #repeatedcv, 
  search = 'random',
  number = 10, #4
  repeats = 10, #10
  # classProbs = TRUE,
  # savePred = TRUE,
  allowParallel = TRUE,
  summaryFunction = defaultSummary)

PPperformance_all <- NULL
PPperformance <- NULL
for (imethod in topmethods) {
  for (ipreprocess in preprocessfunctions) {
    registerDoSEQ()
    registerDoMC(cores = 6)
    
    PreProcessModel = train(Hunters ~ ., data = traindata,
                         method = imethod,
                         preProc = ipreprocess, 
                         #tuneLength = 10,
                         #tuneGrid = kknnTuneGrid,
                         trControl = fitControl)
    
    print(PreProcessModel)
    
    # check performance
    predictdata <- predict(PreProcessModel, testdata)
    
    PPperformance$method <- imethod
    PPperformance$preprocess <- ipreprocess
    PPperformance$RMSE <- postResample(pred = predictdata, obs = testdata$Hunters)[1]
    PPperformance <- as.data.frame(PPperformance)
    PPperformance_all <- rbind(PPperformance_all,PPperformance)
  }
}
PPperformance_all
#' Output from AzureML
# [ModuleOutput]          method preprocess     RMSE
# [ModuleOutput] RMSE       kknn     BoxCox 130.7939
# [ModuleOutput] RMSE1      kknn YeoJohnson 130.9600
# [ModuleOutput] RMSE2      kknn     center 130.7331
# [ModuleOutput] RMSE3      kknn      scale 130.1818
# [ModuleOutput] RMSE4      kknn        pca 130.2071
# [ModuleOutput] RMSE5 svmRadial     BoxCox 154.0898
# [ModuleOutput] RMSE6 svmRadial YeoJohnson 169.9816
# [ModuleOutput] RMSE7 svmRadial     center 154.1891
# [ModuleOutput] RMSE8 svmRadial      scale 154.1000
# [ModuleOutput] RMSE9 svmRadial        pca 164.0881
#' svmRadial and kknn don't perform better with any of the preprocessing functions in place
  
#' Now we can review the predictors, there are only a few fields so I will manually test performance
#' while excluding each of them to monitor their importance.
#' Some of our fields are instinctively required (Year, Unit)
Predictors <- c("Quota","Drawn")
Predictorperformance_all <- NULL
Predictorperformance <- NULL
for (imethod in topmethods) {
  for (ipredictor in Predictors) {
    registerDoSEQ()
    registerDoMC(cores = 6)
    
    PredictorModel = train(Hunters ~ ., data = select(traindata,-ipredictor),
                            method = imethod,
                            tuneLength = 15,
                            trControl = fitControl)
    
    print(PredictorModel)
    
    # check performance
    predictdata <- predict(PredictorModel, testdata)
    
    Predictorperformance$method <- imethod
    Predictorperformance$missing_predictor <- ipredictor
    Predictorperformance$RMSE <- postResample(pred = predictdata, obs = testdata$Hunters)[1]
    Predictorperformance <- as.data.frame(Predictorperformance)
    Predictorperformance_all <- rbind(Predictorperformance_all,Predictorperformance)
  }
}
Predictorperformance_all

#       method    missing_predictor    RMSE
# RMSE  svmRadial             Quota 152.5027
# RMSE1 svmRadial             Drawn 153.7358
# RMSE2      kknn             Quota 131.1298
# RMSE3      kknn             Drawn 134.1082
# RMSE4      kknn   Quota and Drawn 130.3965
#' svMRadial will perform better with all of the predictors, while kknn performs
#' better with only Unit and Year fields

#' Use above information to test out various combinations of preprocessing and predictor sets
#' 
#' 
#' kknn without Quota and Drawn
fitControl <- trainControl(
  method = "adaptive_cv", #repeatedcv, 
  search = 'random',
  number = 10, #4
  repeats = 10, #10
  # classProbs = TRUE,
  # savePred = TRUE,
  allowParallel = TRUE,
  summaryFunction = defaultSummary)

registerDoSEQ()
registerDoMC(cores = 6)

kknnModel = train(Hunters ~ ., data = select(COElkHunters,-Quota, -Drawn),
                  method = "kknn",
                  tuneLength = 75,
                  trControl = fitControl)

print(kknnModel)

# Best RMSE, not sure why caret is selecting parameters with higher RMSE, lets select manually
RSMEkknn <- filter(kknnModel$results, RMSE == min(RMSE))
RSMEkknn$kernel <- as.character(RSMEkknn$kernel)
# run again with a tune grid
kknnTuneGrid <- data.frame(kmax = c(RSMEkknn$kmax,RSMEkknn$kmax,RSMEkknn$kmax,RSMEkknn$kmax,RSMEkknn$kmax),
                           distance = c(RSMEkknn$distance*.7,RSMEkknn$distance*.9,RSMEkknn$distance,RSMEkknn$distance*1.1,RSMEkknn$distance*1.3),
                           kernel = c(RSMEkknn$kernel,RSMEkknn$kernel,RSMEkknn$kernel,RSMEkknn$kernel,RSMEkknn$kernel))

fitControl <- trainControl(
  method = "repeatedcv", #repeatedcv, 
  number = 10, #4
  repeats = 10, #10
  allowParallel = TRUE,
  summaryFunction = defaultSummary)

registerDoSEQ()
registerDoMC(cores = 6)

kknnGridModel = train(Hunters ~ ., data = select(COElkHunters,-Quota, -Drawn),
                  method = "kknn",
                  tuneGrid = kknnTuneGrid,
                  trControl = fitControl)

print(kknnGridModel)

# Best RMSE, not sure why caret is selecting parameters with higher RMSE, lets select manually
RSMEkknn <- filter(kknnGridModel$results, RMSE == min(RMSE))
RSMEkknn$kernel <- as.character(RSMEkknn$kernel)
# run again with a tune grid
kknnTuneGrid2 <- data.frame(kmax = c(RSMEkknn$kmax*.7,RSMEkknn$kmax*.9,RSMEkknn$kmax,RSMEkknn$kmax*1.1,RSMEkknn$kmax*1.3),
                           distance = c(RSMEkknn$distance,RSMEkknn$distance,RSMEkknn$distance,RSMEkknn$distance,RSMEkknn$distance),
                           kernel = c(RSMEkknn$kernel,RSMEkknn$kernel,RSMEkknn$kernel,RSMEkknn$kernel,RSMEkknn$kernel))

registerDoSEQ()
registerDoMC(cores = 6)

kknnGridModel2 = train(Hunters ~ ., data = select(COElkHunters,-Quota, -Drawn),
                      method = "kknn",
                      tuneGrid = kknnTuneGrid2,
                      trControl = fitControl)

print(kknnGridModel2)

# One more time on final parameter (kernel)
# Best RMSE, not sure why caret is selecting parameters with higher RMSE, lets select manually
RSMEkknn <- filter(kknnGridModel2$results, RMSE == min(RMSE))[1,]
kernels <- levels(kknnModel$results$kernel)
# run again with a tune grid
kknnTuneGrid3 <- data.frame(kmax = rep(465.0,8),
                            distance = rep(0.1395586,8),
                            kernel = kernels)

registerDoSEQ()
registerDoMC(cores = 6)

kknnGridModel3 = train(Hunters ~ ., data = select(COElkHunters,-Quota, -Drawn),
                       method = "kknn",
                       tuneGrid = kknnTuneGrid3,
                       trControl = fitControl)

print(kknnGridModel3)
RSMEkknn <- filter(kknnGridModel3$results, RMSE == min(RMSE))

#' Best RMSE for kknn thus far
RSMEkknn <- filter(kknnModel$results, RMSE == min(RMSE))

#' Work thru some resampling methods with best kknn params
kknnTuneGrid4 <- data.frame(kmax = RSMEkknn$kmax,
                            distance = RSMEkknn$distance,
                            kernel = as.character(RSMEkknn$kernel))

trainmethods <- c("boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV", "none")
trainmethodperformance_all <- NULL
for (itrainmethod in trainmethods) {
  trainmethodperformance <- NULL
  fitControl <- trainControl(
    method = itrainmethod,
    number = 10, #4
    repeats = 10, #10
    allowParallel = TRUE,
    summaryFunction = defaultSummary)
  
  registerDoSEQ()
  registerDoMC(cores = 6)
  
  kknnTrainModel = train(Hunters ~ ., data = select(COElkHunters,-Quota, -Drawn),
                         method = "kknn",
                         tuneGrid = kknnTuneGrid4,
                         trControl = fitControl)
  
  print(kknnTrainModel)
  trainmethodperformance <- filter(kknnTrainModel$results, RMSE == min(RMSE))
  trainmethodperformance$trainmethod <- itrainmethod
  trainmethodperformance_all <- rbind.fill(trainmethodperformance_all,trainmethodperformance)
}





# check performance
predictdata <- predict(kknnModelgrid, testdata)

postResample(pred = predictdata, obs = testdata$Hunters)




kknnperformance_all <- NULL
#kmax = 16, distance = 1.063952 and kernel = triweight
kknnTuneGrid <- data.frame(kmax = c(211,211,211,211,211),
                           distance = c(2.25*.7,2.25*.9,2.25,2.25*1.1,2.25*1.3),
                           kernel = c("cos","cos","cos","cos","cos"))


for (ipredictor in Predictors) {
  kknnperformance <- NULL

  
  
  # check performance
  predictdata <- predict(kknnModel, testdata)
  
  kknnperformance$kmax <- imethod
  kknnperformance$missing_predictor <- ipredictor
  kknnperformance$RMSE <- postResample(pred = predictdata, obs = testdata$Hunters)[1]
  kknnperformance <- as.data.frame(kknnperformance)
  kknnperformance_all <- rbind(kknnperformance_all,kknnperformance)
}
#' Tune each model's parameters
#' 



# run again with a tune grid
kknnTuneGrid <- data.frame(kmax = c(36,36,36,36,36),
                           distance = c(0.5207858*.7,0.5207858*.9,0.5207858,0.5207858*1.1,0.5207858*1.3),
                           kernel = c("biweight","biweight","biweight","biweight","biweight"))

registerDoSEQ()
registerDoMC(cores = 6)

HuntersModel = train(Hunters ~ ., data = select(traindata,-Drawn,-Quota),
                     method = topmethod,
                     #preProc = c("center"), 
                     #tuneLength = 10,
                     tuneGrid = kknnTuneGrid,
                     trControl = fitControl)

HuntersModel

#' Important predictors
# ImpPred <- varImp(HuntersModel,scale = T)
# ImpPred

# check performance
predictdata <- predict(HuntersModel, testdata)

postResample(pred = predictdata, obs = testdata$Hunters)
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