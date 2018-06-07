
setwd("~/_code/colorado-dow/datasets")

# source all of our data
#' Run script to get hunt tables
source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)
# COElkRifleAll

#' Run script to get elk population estimates
source('~/_code/colorado-dow/datasets/read colorado dow population estimates.R', echo=F)
# COElkPopulationAll

#' Don't run script to get weather data (due to limited daily Dark Sky polling in free version)
# source('~/_code/colorado-dow/datasets/Get weather data for units and hunt seasons.R', echo=F)
#' Instead we will load the weatherdata file we have saved off
load("~/_code/colorado-dow/datasets/weatherdata5.RData")
# weatherdata5

#' Run script to get CPW elk drawing summaries
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)
# COElkDrawAll
filter(COElkRifleAll, Unit == "77" & Year == 2016)

# join appropriately, fill blanks with NA
COElkHuntingData <- full_join(COElkRifleAll,COElkPopulationAll)
head(filter(COElkHuntingData, Unit == "77" & Year > 2015))

COElkHuntingData <- full_join(COElkHuntingData,weatherdata5)
head(filter(COElkHuntingData, Unit == "77" & Year > 2015))

COElkHuntingData <- full_join(COElkHuntingData,COElkDrawAll2)
head(filter(COElkHuntingData, Unit == "77" & Year > 2015))

# save to directory for easy loading
save(COElkHuntingData,file="COElkHuntingData.RData")



