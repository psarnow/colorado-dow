#' ---
#' title: "Download and Read PDF Elk Population Estimates from Colorado CPW"
#' author: "Pierre Sarnow"
#' ---

#' ## Description
#' Colorado Parks and Wildlife (CPW) aka Colorado Department of Wildlife (CDOW)
#' provides historical elk herd population estimates. In recent years they even provide 
#' a breakdown of Bulls to Cows.
#' 
#' The estimated herds span multiple Game Management Units (Units), that they provide
#' hunting licenses for.  The herds are labeled by Data Analysis Units (DAU), and some
#' of them change Units slightly from year to year. In our simplest case we will split
#' the estimated population evenly across Units.
#'
setwd("~/_code/colorado-dow/datasets")

#' Load required libraries for acquiring data from pdf
library(pdftools,quietly = T)
library(stringr,quietly = T)
library(plyr,quietly = T)
library(dplyr,quietly = T)
library(tidyr,quietly = T)

# Identify the years that CPW will provide tables for in this pdf format
years <- c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)

#'  Loop through years
COElkPopulationAll <- NULL # Initialize
for (iyear in years) {
  
  # RUN ONCE to download files
  # if (iyear >= 2015) {
  #   download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
  #                       iyear,"ElkPopulationEstimates.pdf",sep=""),
  #                 paste(iyear,"COElkPopulation",sep=""))
  # } else {
  #   download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
  #                       iyear,"ElkPopulationEstimate.pdf",sep=""),
  #                 paste(iyear,"COElkPopulation",sep=""))
  # }
  
  # This function will directly export the raw text in a character vector with spaces to show 
  # the white space and \n to show the line breaks.
  COElkPop <- pdf_text(paste(iyear,"COElkPopulation",sep=""))
  
  # Having a full page in one element of a vector is not the most practical. Using strsplit 
  # will help separate lines from each other
  COElkPopa <- strsplit(COElkPop, "\n")
  COElkPopa <- COElkPopa[[1]]
  # remove rows with the table headers
  removeheaderrows <- grep(paste(iyear,"POST HUNT POPULATION"), COElkPopa)
  COElkPopb <- COElkPopa[-(1:removeheaderrows)]
  removeheaderrows <- grep("Post Hunt", COElkPopb)
  COElkPopb <- COElkPopb[-removeheaderrows]
  removefooterrows <- grep("Total", COElkPopb)
  COElkPopb <- COElkPopb[-(removefooterrows:length(COElkPopb))]
  
  # determine column names 
  # columnnames <- grep("([:alpha:])", COElkPopb)
  columnnames <- grep("[:punct:]", COElkPopb)
  columnnames1 <- COElkPopb[columnnames]
  columnnames1 <- columnnames1[length(columnnames1)] # use the max row of the columnnames
  columnnames2 <- str_trim(columnnames1) # remove any extra whitespace
  columnnames3 <- unlist(strsplit(columnnames2, split = "\\s{2,}")) # split on two or more white spaces
  columnnames3[grep("DAU", columnnames3)] <- "DAU" # clean up DAU label
  columnnames3[grep("MANAGEMENT", columnnames3)] <- "GMUnits" # clean up DAU label
  if (length(columnnames3 == 4)) {columnnames3[4] <- "Bull_Ratio"} # some years have an extra column
  
  COElkPopb1 <- COElkPopb[-columnnames]
  COElkPopb1 <- str_trim(COElkPopb1) # remove extra whitespace

  # now that it is cleaned up, use the white space to separate into columns
  COElkPopb2 <- str_split_fixed(COElkPopb1,pattern = "\\s{2,}", n=length(columnnames3))

  COElkPopb2 <- as.data.frame(COElkPopb2) # and convert into a dataframe
  colnames(COElkPopb2) <- columnnames3 # apply our column names
  
  COElkPopb2$Estimate <- as.numeric(gsub(",", "", COElkPopb2$Estimate)) # remove commas from Estimate
  # We have been operating by Units, so lets devide the population estimate across the Units the herd is in.... 
  # assume they are evenly distributed
  COElkPopb2$Num_GMUnits <- str_count(as.character(COElkPopb2$GMUnits), pattern = ",") + 1
  COElkPopb2$Population.Unit <- COElkPopb2$Estimate / COElkPopb2$Num_GMUnits

  # get the GMUnits out
  COElkPopb3 <- separate(COElkPopb2, GMUnits, sep = ",",LETTERS)
  COElkPopb3 <- gather(COElkPopb3,"ignore",Unit,A:Z)
  COElkPopb3 <- select(COElkPopb3, -ignore)
  COElkPopb3 <- filter(COElkPopb3, !is.na(Unit))
  COElkPopb3$Unit <- str_trim(COElkPopb3$Unit) # remove extra whitespace
  
  colnames(COElkPopb3)[colnames(COElkPopb3)=="Estimate"] <- "Population.DAU" #change label for clarification
  COElkPopb3$Year <- as.character(iyear)
  COElkPopulationAll <- rbind.fill(COElkPopulationAll,COElkPopb3)
  
}

#' Cleanup the footnotes placed on some of the Unit ids
COElkPopulationAll$Unit <- str_remove(COElkPopulationAll$Unit, "[:alpha:]") # remove the letters
COElkPopulationAll$Unit <- str_remove(COElkPopulationAll$Unit, "\\*+") # remove the *
COElkPopulationAll$Unit <- str_remove(COElkPopulationAll$Unit, "\\)+") # remove the )
COElkPopulationAll$Unit <- str_remove(COElkPopulationAll$Unit, "\\(+") # remove the (

#' Take a peak at the dataframe
head(COElkPopulationAll)
