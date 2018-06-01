#' ---
#' title: "Weather data for hunting units and seasons"
#' author: "Pierre Sarnow"
#' ---
#' 
# Historic weather data for hunt units
# Reference some accessible APIs here /datasets/investigate options to gather historic weather data.R

#' # Description
#' Use the historic data provided by Dark Sky to attach to each unit. We will need to make some basic
#' assumptions to get this started.
#' 
#' * Use the centroid coordinates of the Unit for the historic weather data, in some cases (due to elevation),
#' the weather estimates won't be that accurate for the unit. But it is a way to get this started.
#' * Hunt season are a span of days. I will use the mean, max, and average across all of the season dates
#' as the weather data points.
#'
#' ## Step 1 - Get Unit coordinates
#' I've created a separate script to calculate this
# source(myGMUcoordinatescript)

#' ## Step 2 - Get hunt season dates
#' I've created a separate script to populate per CPW
source("~/_code/colorado-dow/datasets/CO rifle elk hunt season dates.R",echo = F)

#' ## Step 3 - Connect to Dark Sky and request data
# API key

#' ## Step 4 - Attach weather data to Units and Seasons