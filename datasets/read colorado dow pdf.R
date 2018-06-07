#' ---
#' title: "Download and Read PDF Hunt Tables from Colorado CPW"
#' author: "Pierre Sarnow"
#' ---

#' ## Description
#' Colorado Parks and Wildlife (CPW) aka Colorado Department of Wildlife (CDOW)
#' provides historical statistics on Big Game hunts. The tables are organized by year
#' and available for download in pdf format.
#' In the case of the project we are solely interested in Elk Rifle Hunting on public
#' land.
#' The tables are organized by hunting seasons (First-Fourth), as well as by hunting regions (Units)
#' The hunting regions vary very slightly from year to year but for the most part have
#' been consistent for many years.
#'
setwd("~/_code/colorado-dow/datasets")

#' Load required libraries for acquiring data from pdf
library(pdftools,quietly = T)
library(stringr,quietly = T)

# Identify the years that CDOW will provide tables for in this pdf format
years <- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)

#'  Loop through years
COElkRifleAll <- NULL # Initialize
for (iyear in years) {
  
  # RUN ONCE to download
  # if (iyear >= 2014) {
  #   download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
  #                       iyear,"StatewideElkHarvest.pdf",sep=""),
  #                 paste(iyear,"COElkHarvest",sep=""))
  # } else {
  #   download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
  #                       iyear,"ElkHarvestSurvey.pdf",sep=""),
  #                 paste(iyear,"COElkHarvest",sep=""))
  # }
  
  # This function will directly export the raw text in a character vector with spaces to show 
  # the white space and \n to show the line breaks.
  COElk <- pdf_text(paste(iyear,"COElkHarvest",sep=""))
  
  # Having a full page in one element of a vector is not the most practical. Using strsplit 
  # will help separate lines from each other
  COElka <- strsplit(COElk, "\n")
  
  # years starting in 2014 have a cover page or table of contents
  if (iyear >= 2014) {
    COElka <- COElka[-1] # remove cover page (map, or table of contents)
  }
  
  # The document holds more information than we are after.
  # In our case we are looking for 
  tableheadings <- c(paste(iyear,"Elk Harvest, Hunters and Recreation Days for First Rifle Seasons"),
                     paste(iyear,"Elk Harvest, Hunters and Recreation Days for Second Rifle Seasons"),
                     paste(iyear,"Elk Harvest, Hunters and Recreation Days for Third Rifle Seasons"),
                     paste(iyear,"Elk Harvest, Hunters and Recreation Days for Fourth Rifle Seasons"))
  
  # Notice that the tables run across pages, there are some pages that will have
  # info we want and info we want to ignore
  
  # Identify pages with the table headings we identified
  rifle1 <- grep(tableheadings[1], COElka)
  rifle2 <- grep(tableheadings[2], COElka)
  rifle3 <- grep(tableheadings[3], COElka)
  rifle4 <- grep(tableheadings[4], COElka)
  
  rifleseasons <- unique(c(rifle1,rifle2,rifle3,rifle4))
  COElkb <- COElka[rifleseasons]
  
  # the first page has the end of a previous table, remove it so we can have consistent columns
  firsttable <- COElkb[[1]]
  # which row has the heading of our table?
  firsttablestart <- grep(tableheadings[1], firsttable)
  # drop all rows before that table
  firstpage <- firsttable[-(1:firsttablestart-1)]
  
  # the last page might have the beginning of a new table, remove it so we can have consistent columns
  lasttablepage <- length(COElkb) # what is the last page?
  lasttable <- COElkb[[lasttablepage]]
  # which rows have table headings? they all start with 'iyear Elk Harvest'
  lasttableend <- grep(paste(iyear,"Elk Harvest"), lasttable)
  lasttableend <- max(lasttableend) # the second entry is the start of the table to ignore
  if (lasttableend >1) {
    # drop all rows after our table
    lastpage <- lasttable[-(lasttableend:length(lasttable))]
  } else {lastpage <- lasttable}
  
  # replace updated first and last pages
  COElkc <- COElkb
  COElkc[[1]] <- firstpage
  COElkc[[lasttablepage]] <- lastpage
  
  # unlist page elements
  COElkd <- unlist(COElkc)
  
  ######## SEASON ONE ######## (could make a season loop as well)
  # identify season 1 data
  seasonONEstart <- grep(tableheadings[1], COElkd)[1]
  seasonONEend <- grep(tableheadings[2], COElkd)[1]
  seasonONE <- COElkd[((seasonONEstart+1):(seasonONEend-1))]
  
  # remove rows with the table headers
  removeheaderrows <- grep(paste(iyear,"Elk Harvest"), seasonONE)
  seasonONE <- seasonONE[-removeheaderrows]
  
  # determine column names 
  columnnames <- grep("([:alpha:])", seasonONE)
  seasonONE1 <- seasonONE[-columnnames]
  columnnames1 <- seasonONE[columnnames]
  columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
  columnnames2 <- columnnames1[length(columnnames1)]
  columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
  columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
  colremove <- grep("([.])",columnnames3)
  columnnames3 <- columnnames3[-colremove]
  
  seasonONE1 <- str_trim(seasonONE1) # remove extra whitespace
  pagenumbers <- grep("\\s+", seasonONE1) # remove page numbers
  seasonONE2 <- seasonONE1[pagenumbers]
  
  # now that it is cleaned up, use the white space to separate into columns
  seasonONE3 <- str_split_fixed(seasonONE2,pattern = "\\s+", n=length(columnnames3))
  seasonONE3 <- as.data.frame(seasonONE3) # and convert into a dataframe
  colnames(seasonONE3) <- columnnames3 # apply our column names
  
  # add the season column
  seasonONE3$Season <- 1
  
  ######## SEASON TWO ######## 
  # identify season 2 data
  seasonTWOstart <- grep(tableheadings[2], COElkd)[1]
  seasonTWOend <- grep(tableheadings[3], COElkd)[1]
  seasonTWO <- COElkd[((seasonTWOstart+1):(seasonTWOend-1))]
  
  # remove rows with the table headers
  removeheaderrows <- grep(" Elk Harvest", seasonTWO)
  seasonTWO <- seasonTWO[-removeheaderrows]
  
  # determine column names 
  columnnames <- grep("([:alpha:])", seasonTWO)
  seasonTWO1 <- seasonTWO[-columnnames]
  columnnames1 <- seasonTWO[columnnames]
  columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
  columnnames2 <- columnnames1[length(columnnames1)]
  columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
  columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
  colremove <- grep("([.])",columnnames3)
  columnnames3 <- columnnames3[-colremove]
  
  seasonTWO1 <- str_trim(seasonTWO1) # remove extra whitespace
  pagenumbers <- grep("\\s+", seasonTWO1) # remove page numbers
  seasonTWO2 <- seasonTWO1[pagenumbers]
  
  # now that it is cleaned up, use the white space to separate into columns
  seasonTWO3 <- str_split_fixed(seasonTWO2,pattern = "\\s+", n=length(columnnames3))
  seasonTWO3 <- as.data.frame(seasonTWO3) # and convert into a dataframe
  colnames(seasonTWO3) <- columnnames3 # apply our column names
  
  # add the season column
  seasonTWO3$Season <- 2
  
  ######## SEASON THREE ######## 
  # identify season 3 data
  seasonTHREEstart <- grep(tableheadings[3], COElkd)[1]
  seasonTHREEend <- grep(tableheadings[4], COElkd)[1]
  seasonTHREE <- COElkd[((seasonTHREEstart+1):(seasonTHREEend-1))]
  
  # remove rows with the table headers
  removeheaderrows <- grep(" Elk Harvest", seasonTHREE)
  seasonTHREE <- seasonTHREE[-removeheaderrows]
  
  # determine column names
  columnnames <- grep("([:alpha:])", seasonTHREE)
  seasonTHREE1 <- seasonTHREE[-columnnames]
  columnnames1 <- seasonTHREE[columnnames]
  columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
  columnnames2 <- columnnames1[length(columnnames1)]
  columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
  columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
  colremove <- grep("([.])",columnnames3)
  columnnames3 <- columnnames3[-colremove]
  
  seasonTHREE1 <- str_trim(seasonTHREE1) # remove extra whitespace
  pagenumbers <- grep("\\s+", seasonTHREE1) # remove page numbers
  seasonTHREE2 <- seasonTHREE1[pagenumbers]
  
  # now that it is cleaned up, use the white space to separate into columns
  seasonTHREE3 <- str_split_fixed(seasonTHREE2,pattern = "\\s+", n=length(columnnames3))
  seasonTHREE3 <- as.data.frame(seasonTHREE3) # and convert into a dataframe
  colnames(seasonTHREE3) <- columnnames3 # apply our column names
  
  # add the season column
  seasonTHREE3$Season <- 3
  
  
  ######## SEASON FOUR ######## 
  # identify season 4 data
  seasonFOURstart <- grep(tableheadings[4], COElkd)[1]
  seasonFOUR <- COElkd[((seasonFOURstart+1):(length(COElkd)))] # to the end
  
  # remove rows with the table headers
  removeheaderrows <- grep(" Elk Harvest", seasonFOUR)
  seasonFOUR <- seasonFOUR[-removeheaderrows]
  
  # determine column names 
  columnnames <- grep("([:alpha:])", seasonFOUR)
  seasonFOUR1 <- seasonFOUR[-columnnames]
  columnnames1 <- seasonFOUR[columnnames]
  columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
  columnnames2 <- columnnames1[length(columnnames1)]
  columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
  columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
  colremove <- grep("([.])",columnnames3)
  columnnames3 <- columnnames3[-colremove]
  
  seasonFOUR1 <- str_trim(seasonFOUR1) # remove extra whitespace
  pagenumbers <- grep("\\s+", seasonFOUR1) # remove page numbers
  seasonFOUR2 <- seasonFOUR1[pagenumbers]
  
  # now that it is cleaned up, use the white space to separate into columns
  seasonFOUR3 <- str_split_fixed(seasonFOUR2,pattern = "\\s+", n=length(columnnames3))
  seasonFOUR3 <- as.data.frame(seasonFOUR3) # and convert into a dataframe
  colnames(seasonFOUR3) <- columnnames3 # apply our column names
  
  # add the season column
  seasonFOUR3$Season <- 4
  
  #Combine
  COElkRifle <- rbind(seasonONE3,seasonTWO3,seasonTHREE3,seasonFOUR3)
  COElkRifle$Year <- as.character(iyear)
  COElkRifleAll <- rbind(COElkRifleAll,COElkRifle)
}

#' Clean up dataframe fields
#' It may happen that when reading numeric data into R (usually, when reading in a file), they come in as factors. 
#' If f is such a factor object, you can use
# as.numeric(as.character(f)) to get the numbers back. 
#' More efficient, but harder to remember, is
# as.numeric(levels(f))[as.integer(f)]
#' However, there are still implications in our data as there are commas present for values in the thousands.
#' As-is converting to numeric using the factor levels will coerce them to NAs
#' So we will need to convert to character, remove the commas, then convert to numeric
#' 
COElkRifleAll$Hunters <- as.numeric(gsub(",", "", COElkRifleAll$Hunters))
COElkRifleAll$Days <- as.numeric(gsub(",", "", COElkRifleAll$Days))
# TODO make the gsub search each column

COElkRifleAll$Success <- as.numeric(levels(COElkRifleAll$Success))[as.integer(COElkRifleAll$Success)]
COElkRifleAll$Harvest <- as.numeric(levels(COElkRifleAll$Harvest))[as.integer(COElkRifleAll$Harvest)]
COElkRifleAll$Season <- as.character(COElkRifleAll$Season)
COElkRifleAll$Unit <- as.character(COElkRifleAll$Unit)

#' Create new statistics based on investigation
# How much effort it takes to have a successful result
COElkRifleAll$Harvest_Effort <- COElkRifleAll$Days / COElkRifleAll$Harvest # From Phase I investigation
COElkRifleAll$Harvest_Effort[is.infinite(COElkRifleAll$Harvest_Effort)] <- NA # we get an inf where there was no harvest

#' Peek at the dataframe
head(COElkRifleAll)