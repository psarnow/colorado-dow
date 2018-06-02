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
#' almost 4 units at a time.
#################################
ununit <- unique(data_centroids$Unit)
weatherdata5 <- NULL
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
        weatherdata$MaxTemp <- weatherdata0$daily$temperatureHigh
        weatherdata$MinTemp <- weatherdata0$daily$temperatureLow
        weatherdata$precipIntensity <- weatherdata0$daily$precipIntensity
        # weatherdata$precipType <- weatherdata0$daily$precipType
        weatherdata$moonPhase <- weatherdata0$daily$moonPhase
        weatherdata <- as.data.frame(weatherdata)
        weatherdata1 <- rbind.fill(weatherdata1, weatherdata)
      }
      weatherdata2 <- data_frame_(unlist(list(colMeans(weatherdata1)))) # calculate mean of each field through the season
      weatherdata2$Season <- iseason
      weatherdata3 <- rbind.fill(weatherdata3, weatherdata2)
    }
    weatherdata3$Year <- iyear
    weatherdata4 <- rbind.fill(weatherdata4, weatherdata3)
  }
  weatherdata4$Unit <- iunit
  weatherdata5 <- rbind.fill(weatherdata5, weatherdata4)
}

head(weatherdata5)
