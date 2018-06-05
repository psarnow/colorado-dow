#' ---
#' title: "Define Colorado elk hunting season dates"
#' author: "Pierre Sarnow"
#' ---
#' 
#' I opted to do this without a lot of automation, as there aren't many pages.
#' I found the documents here, nothing before 2010
# http://cpw.state.co.us/Documents/Hunting/BigGame/5YearSeasonStructure/Final2010-2014BGSSdatesrevised.pdf
# https://cpw.state.co.us/Documents/Hunting/BigGame/5YearSeasonStructure/Big-Game-Season-Structure-Dates-2015-2019.pdf

#' I've included those pdfs in the datasets directory

# Regular Rifle Deer and Elk Seasons: four Seasons
# 1st Season – 5 day Season, followed by a 2 day break – Separate Limited Elk – Opening on the first
# Saturday after October 9 - Antlerless licenses may be offered in DAUs over population objective;
# 2nd Season – 9 day Season, followed by a 5 day break – Deer and Elk Combined – Opening on a
# Saturday;
# 3rd Season – 9 day Season, followed by a 2 day break – Deer and Elk Combined – Opening on a
# Saturday;
# 4th Season – 5 day Season – Deer and Elk Combined – Opening on a Wednesday - Elk: this Season
# is to be used as needed to meet DAU objectives. Antlerless harvest is emphasized. Antlered
# hunting is limited by draw, no over the counter bull licenses. Deer: by DAU buck and/or doe
# hunting consistent with DAU plans.
Seasondates1 <- NULL
Seasondates <- NULL
SeasondatesADD <- NULL
#'## 2010
# 1st Separate Limited Elk 10/16-10/20
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/16-10/20",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/16-10/20",split = "-"))[2]
SeasondatesADD <- as.data.frame(SeasondatesADD)
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/23-10/31
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/23-10/31",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/23-10/31",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/6-11/14
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/6-11/14",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/6-11/14",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/17-11/21
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/17-11/21",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/17-11/21",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
Seasondates$Year <- 2010
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

#'## 2011
Seasondates <- NULL
# 1st Separate Limited Elk 10/15-10/19
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/15-10/19",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/15-10/19",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/22-10/30
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/22-10/30",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/22-10/30",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/5-11/13
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/5-11/13",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/5-11/13",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/16-11/20
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/16-11/20",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/16-11/20",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2011
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

#'## 2012
Seasondates <- NULL
# 1st Separate Limited Elk 10/13-10/17
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/13-10/17",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/13-10/17",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/20-10/28
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/20-10/28",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/20-10/28",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/3-11/11
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/3-11/11",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/3-11/11",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/14-11/18
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/14-11/18",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/14-11/18",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2012
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

#'## 2013
Seasondates <- NULL
# 1st Separate Limited Elk 10/12-10/16
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/12-10/16",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/12-10/16",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/19-10/27
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/19-10/27",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/19-10/27",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/2-11/10
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/2-11/10",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/2-11/10",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/13-11/17
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/13-11/17",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/13-11/17",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2013
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

#'## 2014
Seasondates <- NULL
# 1st Separate Limited Elk 10/11-10/15
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/11-10/15",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/11-10/15",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/18-10/26
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/18-10/26",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/18-10/26",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/1-11/9
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/1-11/9",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/1-11/9",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/12-11/16
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/12-11/16",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/12-11/16",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2014
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

################################################################################################

# Regular Rifle Deer and Elk Seasons: four Seasons
# 1st Season – 5 day Season, followed by a 2 day break – Separate Limited Elk – Opening
# on the first Saturday after October 9 - Antlerless licenses may be offered in DAUs over
# population objective;
# 2nd Season – 9 day Season, followed by a 5 day break – Deer and Elk Combined –
# Opening on a Saturday;
# 3rd Season – 9 day Season, followed by a 2 day break – Deer and Elk Combined –
# Opening on a Saturday;
# 4th Season – 5 day Season – Deer and Elk Combined – Opening on a Wednesday.

#'## 2015
Seasondates <- NULL
# 1st Separate Limited Elk 10/10-10/14
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/10-10/14",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/10-10/14",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/17-10/25
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/17-10/25",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/17-10/25",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 10/31-11/8
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("10/31-11/8",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/31-11/8",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/11-11/15
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/11-11/15",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/11-11/15",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2015
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

#'## 2016
Seasondates <- NULL
# 1st Separate Limited Elk 10/15-10/19
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/15-10/19",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/15-10/19",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/22-10/30
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/22-10/30",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/22-10/30",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/5-11/13
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/5-11/13",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/5-11/13",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/16-11/20
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/16-11/20",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/16-11/20",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2016
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

#'## 2017
Seasondates <- NULL
# 1st Separate Limited Elk 10/14-10/18
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/14-10/18",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/14-10/18",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/21-10/29
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/21-10/29",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/21-10/29",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/4-11/12
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/4-11/12",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/4-11/12",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/15-11/19
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/15-11/19",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/15-11/19",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2017
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

#'## 2018
Seasondates <- NULL
# 1st Separate Limited Elk 10/13-10/17
SeasondatesADD$Season <- 1
SeasondatesADD$Start <- unlist(strsplit("10/13-10/17",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/13-10/17",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 2nd Combined Deer & Elk 10/20-10/28
SeasondatesADD$Season <- 2
SeasondatesADD$Start <- unlist(strsplit("10/20-10/28",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("10/20-10/28",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 3rd Combined Deer & Elk 11/3-11/11
SeasondatesADD$Season <- 3
SeasondatesADD$Start <- unlist(strsplit("11/3-11/11",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/3-11/11",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)
# 4th Combined Limited Deer & Elk 11/14-11/18
SeasondatesADD$Season <- 4
SeasondatesADD$Start <- unlist(strsplit("11/14-11/18",split = "-"))[1]
SeasondatesADD$End <- unlist(strsplit("11/14-11/18",split = "-"))[2]
Seasondates <- rbind(Seasondates,SeasondatesADD)

Seasondates$Year <- 2018
Seasondates$Start <- as.POSIXct(paste(Seasondates$Year,Seasondates$Start,sep = "/"))
Seasondates$End <- as.POSIXct(paste(Seasondates$Year,Seasondates$End,sep = "/"))
Seasondates1 <- rbind(Seasondates1,Seasondates)

Seasondates1$Duration <- as.integer(round((Seasondates1$End - Seasondates1$Start) + 1)) # Add season duration in days

#' Clean up some fields
Seasondates1$Season <- as.character(Seasondates1$Season)
Seasondates1$Year <- as.character(Seasondates1$Year)

Seasondates1