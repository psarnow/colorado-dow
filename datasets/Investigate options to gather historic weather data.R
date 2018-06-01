# Import historical weather data associated with hunt units and hunt season dates.

# Consider using Weather Underground's API
# https://www.wunderground.com/weather/api/d/docs

# someone used WU API back in 2012
# http://allthingsr.blogspot.com/2012/04/getting-historical-weather-data-in-r.html

# There is a recent r package explicitly for this -- ‘rwunderground’

# step 1 get an API key from WU
# https://cran.r-project.org/web/packages/rwunderground/README.html
# http://www.wunderground.com/weather/api/d/login.html

#' Hmmm is this service no longer free?
#' To improve our services and enhance our relationship with our users, 
#' we will no longer provide free weather API keys as part of our program. 
#' If you have been directed to download our Weather Underground free API key by a 
#' third party provider, please contact your vendor for resolution.
#' 
#' 
#' Maybe this package instead
# weatherData
#' Don't think so, it was removed from CRAN in January
#' 
#' # What about NOAA
## Of course, there is a noaa package too! ‘rnoaa’, though it is a year old.
#' rnoaa is an R interface to many NOAA data sources. We don't cover all of them, 
#' but we include many commonly used sources, and add we are always adding new sources. 
#' We focus on easy to use interfaces for getting NOAA data, and giving back data in 
#' easy to use formats downstream. We currently don't do much in the way of plots or analysis.
#' 
#' Lets try it out!
#' ## Step 1 - Acquire API Key
# http://www.ncdc.noaa.gov/cdo-web/token
options(noaakey = "GOcUIkqwdMKZWvFOxcITVuOJCYjUQTbS") #alternatively, store permamently in your .Rprofile file
#' ## Step 2 - Install and load packages
#' You'll need GDAL installed first. You may want to use GDAL >= 0.9-1 since that version or 
#' later can read TopoJSON format files as well, which aren't required here, but may be useful. 
#' Install GDAL
#' Then when you install the R package rgdal (rgeos also requires GDAL), 
#' you'll most likely need to specify where you're gdal-config file is on your machine, 
#' as well as a few other things. I have an OSX Mavericks machine, and this works for me 
#' (there's no binary for Mavericks, so install the source version):
# install.packages("http://cran.r-project.org/src/contrib/rgdal_0.9-1.tar.gz", repos = NULL, type="source", configure.args = "--with-gdal-config=/Library/Frameworks/GDAL.framework/Versions/1.10/unix/bin/gdal-config --with-proj-include=/Library/Frameworks/PROJ.framework/unix/include --with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib")

require(rgdal)
require(rnoaa)

#' Lets see what we can do with this package....
#' 
#' Fetch list of city locations in descending order
ncdc_locs(locationcategoryid='CITY', sortfield='name', sortorder='desc')
#' Get info on a station by specifying a dataset, locationtype, location, and station
ncdc_stations(datasetid='GHCND', locationid='FIPS:12017', stationid='GHCND:USC00084289')
ncdc_stations(stationid='GHCND:USR0000CBLU') # closest to Unit 77

ncdc_datacats(stationid='GHCND:USR0000CBLU')

#' Search for data
out <- ncdc(datasetid='NORMAL_DLY', stationid='GHCND:USR0000CBLU', startdate = '2010-10-01', enddate = '2010-11-10')
#' See a data.frame
head( out$data )

homr(qid = 'GHCND:USR0000CBLU')

#' There might be limited data, using their webtool, it looks like there are only 
#' about 30 stations in all of Colorado
#' 
#' # What about Dark Sky?
#' free api and decades of historical data
require(darksky)
# https://darksky.net/dev/account
3fc59f55a5f39d140240f6df606c2c19 #API key

trailridge <- c(37.460183, -107.218220) # decimal degrees of an area I hunt in often (Unit 77)
#' Get current forecast for a location coordinate
now <- get_current_forecast(trailridge[1],trailridge[2])
print(now)

#' Historical (using Date objects):
seq(Sys.Date()-10, Sys.Date(), "1 day") %>% 
  map(~get_forecast_for(trailridge[1],trailridge[2], .x)) %>% 
  map_df("hourly") %>% 
  ggplot(aes(x=time, y=temperature)) +
  geom_line()

#' Historic forecasts
then <- get_forecast_for(trailridge[1],trailridge[2], "2013-10-10T12:00:00-0400", add_headers=TRUE)
print(then)

#' getting data for more than one location
more_than_one <- data.frame(loc=c("Maine", "Seattle"),
                            lat=c(43.2672, 47.6097),
                            lon=c(70.8617, 122.3331),
                            when=c("2013-05-06T12:00:00-0400",
                                   "2013-05-06T12:00:00-0400"),
                            stringsAsFactors=FALSE)

bigger_list <- pmap(list(more_than_one$lat, more_than_one$lon,
                         more_than_one$when),
                    get_forecast_for)
names(bigger_list) <- more_than_one$loc

bigger_list$Seattle

bigger_list$Maine

#' the free version limits the amount of api calls
print(sprintf("You have used %s API calls.", then$`x-forecast-api-calls`))

plot(now)

#' # DarkSky seems like the way to go.
#' * CRAN R package
#' * Free API, up to 1,000 free calls per day
#' * Access to historical data
#' * Estimates for each coordinate location
#' * LOTS of fields of data
#'
#' ## Historical validation
#' I remember hunting one year and it snowed many inches on us.
#' How accurate is their historical estimation of temp and precipitation accumulation?
#' Nov 5, 2011 on Sand Bench
#' 
sandbench <- c(37.474, -107.293)
then <- get_forecast_for(sandbench[1],sandbench[2], "2011-11-05T12:00:00-0400", add_headers=TRUE)
print(then)
plot(then)
#' I see that there is precipitation recorded, and it is labeled as 'snow'
#' The accumulation might be incorrect, but as long as we track the same coordinates
#' for each Unit, we will be able to make comparisons.
#' The temps appear to be what I would expect as well.
#' I like the 'daily' field. Provides some good info (35 variables in all)
# precipAccumulation
# precipType
# temperatureHigh
# temperatureLow
# windSpeed
# moonPhase

