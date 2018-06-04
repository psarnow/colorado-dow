#' ---
#' title: "Weather data for hunting units and seasons"
#' author: "Pierre Sarnow"
#' ---
#' 
require(plyr)
require(dplyr)
# Historic weather data for hunt units
# Reference some accessible APIs here: /datasets/investigate options to gather historic weather data.R

#' # Description
#' Use the historic data provided by Dark Sky to attach to each unit. We will need to make some basic
#' assumptions to get this started.
#' 
#' * Use the centroid coordinates of the Unit for the historic weather data, in some cases (due to elevation),
#' the weather estimates won't be that accurate for the unit. But it is a way to get this started.
#' * Hunt seasons span multiple days. I will use the mean, max, and average across all of the season dates
#' as the weather data points.
#'
#' ## Step 1 - Get Unit coordinates
#' I've created a separate script to calculate this
source("~/_code/colorado-dow/datasets/coordinate locations of cpw hunt units.R",echo = F)
head(data_centroids)
#' ## Step 2 - Get hunt season dates
#' I've created a separate script to populate per CPW
source("~/_code/colorado-dow/datasets/CO rifle elk hunt season dates.R",echo = F)
head(Seasondates1)
#' ## Step 3 - Connect to Dark Sky and request data
require(darksky)
#' API key
#' The API wrapper functions in this package all rely on a Dark Sky API key residing in the environment variable DARKSKY_API_KEY. 
#' The easiest way to accomplish this is to set it in the '.Renviron' file in your home directory.
DARKSKY_API_KEY = "3fc59f55a5f39d140240f6df606c2c19"

#' Will need to build this incrementally and save it off as we can only poll 1000 times a day
#' 185 units, 9 years, 4 seasons, ~7days per season or 46,620 calls
#' almost 4 units at a time...will need to poll for 3 each day
#################################
# weatherdata5 <- NULL # initialize before running loop for the first time
# load("weatherdata5.RData") #load weatherdata5 if we lost it

ununitall <- unique(data_centroids$Unit) # all Units
ununit <- ununitall[!ununitall %in% unique(weatherdata5$Unit)] # Units we don't have weather data for yet
ununit <-sample(ununit,3,T)

for (iunit in ununit) {
  location <- filter(data_centroids, Unit == iunit)
  unyear <- unique(Seasondates1$Year)
  weatherdata4 <- NULL
  for (iyear in unyear) {
    fyear <- filter(Seasondates1, Year == iyear)
    unseason <- unique(fyear$Season)
    weatherdata3 <- NULL
    for (iseason in unseason) {
      weatherdates <- filter(fyear, Season == iseason)
      weatherdata1 <- NULL
      for (idays in (1:weatherdates$Duration)) {
        weatherdata <- NULL
        weatherday <- weatherdates$Start - days(1) + days(idays)
        weatherdata0 <- get_forecast_for(location$latitude,location$longitude,weatherday)
        # APIcalls <- weatherdata0$`x-forecast-api-calls`
        # print(sprintf("Have used %s API calls.", APIcalls))
        weatherdata$daily <- weatherdata0$daily
        weatherdata <- as.data.frame(weatherdata)
        desiredcolumns <- c("daily.moonPhase","daily.precipAccumulation","daily.precipType","daily.temperatureHigh","daily.temperatureLow",
                            "daily.dewPoint","daily.humidity","daily.pressure","daily.windSpeed","daily.cloudCover","daily.precipIntensity","daily.precipProbability")
        weatherdata <- select(weatherdata, one_of(desiredcolumns)) # all columns aren't always present in the data
        weatherdata1 <- rbind.fill(weatherdata1, weatherdata)
      }
      if (!is.null(weatherdata1$daily.precipType)) { # change this to numeric so we can average it across days
        weatherdata1$daily.precipType[weatherdata1$daily.precipType=="rain"] <- 1
        weatherdata1$daily.precipType[weatherdata1$daily.precipType=="sleet"] <- 2
        weatherdata1$daily.precipType[weatherdata1$daily.precipType=="snow"] <- 3
        weatherdata1$daily.precipType <- as.numeric(weatherdata1$daily.precipType)
      }
      weatherdata2 <- data_frame_(unlist(list(colMeans(weatherdata1,na.rm = T)))) # calculate mean of each field through the season
      weatherdata2$Season <- iseason
      weatherdata3 <- rbind.fill(weatherdata3, weatherdata2)
    }
    weatherdata3$Year <- iyear
    weatherdata4 <- rbind.fill(weatherdata4, weatherdata3)
  }
  weatherdata4$Unit <- iunit
  weatherdata5 <- rbind.fill(weatherdata5, weatherdata4)
  save(weatherdata5,file="weatherdata5.RData")
}

head(weatherdata5)
