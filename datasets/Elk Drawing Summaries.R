#' ---
#' title: "Download and Read PDF Draw Summaries from Colorado CPW"
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
COElkDrawAll <- NULL # Initialize
for (iyear in years) {
  
  if (iyear >= 2015) {
    download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
                        iyear,"StatewideElkHarvest.pdf",sep=""),
                  paste(iyear,"COElkDraw",sep=""))
  } else {
    download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
                        iyear,"ElkDrawSummary.pdf",sep=""),
                  paste(iyear,"COElkDraw",sep=""))
  }
  
  # This function will directly export the raw text in a character vector with spaces to show 
  # the white space and \n to show the line breaks.
  COElkDraw <- pdf_text(paste(iyear,"COElkDraw",sep=""))
  
  # Having a full page in one element of a vector is not the most practical. Using strsplit 
  # will help separate lines from each other
  COElkDraw <- strsplit(COElkDraw, "\n")

  # COElkDraw <- COElkDraw[[1]]# page one for starters, remove after we have figured this out
  # unlist page elements
  COElkDraw1 <- unlist(COElkDraw)
  
  # remove page headings # can skip if we extract the rows we want later
  pageheadings <- c(grep("Date", COElkDraw1), grep("Time", COElkDraw1), grep("Elk", COElkDraw1), grep("HntCde", COElkDraw1))
  # drop all rows with the page heading
  COElkDraw2 <- COElkDraw1[-pageheadings]
  
  rowsofinterest <- grep("Orig Quota|Chcs Drawn|Choice 1 % Success", COElkDraw2)
  COElkDraw3 <- COElkDraw2[rowsofinterest]
  COElkDraw3 <- str_trim(COElkDraw3)
  
  COElkDraw3a <- as.data.frame(COElkDraw3)
  colnames(COElkDraw3a) <- "FullString"
  
  # COElkDraw3a$Separators <- FALSE
  # COElkDraw3a$Separators[grep("-{2,}", COElkDraw3a$FullString)] <- TRUE
  COElkDraw3a$HuntCode <- FALSE
  COElkDraw3a$HuntCode[grep("E", COElkDraw3a$FullString)] <- TRUE

  COElkDraw3a$HuntCode2 <- substring(COElkDraw3a$FullString,1,8)
  COElkDraw3a$HuntCode[COElkDraw3a$HuntCode==TRUE] <- COElkDraw3a$HuntCode2[COElkDraw3a$HuntCode==TRUE]
  
  COElkDraw3a$Ttl_Chce_1 <- FALSE
  COElkDraw3a$Ttl_Chce_1 <- grepl("Ttl Chce 1",COElkDraw3a$FullString)
  COElkDraw3a$Ttl_Chce_1a <- "NA"
  COElkDraw3a$Ttl_Chce_1a[COElkDraw3a$Ttl_Chce_1==TRUE] <- str_trim(gsub(pattern = "(.*Ttl Chce 1)(.*)( |.*)",
                                  replacement = "\\2",
                                  x = COElkDraw3a$FullString))[COElkDraw3a$Ttl_Chce_1==TRUE]
  COElkDraw3a$Ttl_Chce_1a[COElkDraw3a$Ttl_Chce_1==TRUE] <- sub(" .*$", "", COElkDraw3a$Ttl_Chce_1a)[COElkDraw3a$Ttl_Chce_1==TRUE]# deletes everything after first space
  
  COElkDraw3a$Orig_Quota <- FALSE
  COElkDraw3a$Orig_Quota <- grepl("Orig Quota",COElkDraw3a$FullString)
  COElkDraw3a$Orig_Quotaa <- "NA"
  COElkDraw3a$Orig_Quotaa[COElkDraw3a$Orig_Quota==TRUE] <- str_trim(gsub(pattern = "(.*Orig Quota)(.*)( Ttl Chce 1.*)",
                                                                         replacement = "\\2",
                                                                         x = COElkDraw3a$FullString))[COElkDraw3a$Orig_Quota==TRUE]
  COElkDraw3a$Orig_Quotaa[COElkDraw3a$Orig_Quota==TRUE] <- sub(" .*$", "", COElkDraw3a$Orig_Quotaa)[COElkDraw3a$Orig_Quota==TRUE]# deletes everything after first space
  
  COElkDraw3a$Chcs_Drawn <- FALSE
  COElkDraw3a$Chcs_Drawn <- grepl("Chcs Drawn",COElkDraw3a$FullString)
  COElkDraw3a$Chcs_Drawna <- "NA"
  COElkDraw3a$Chcs_Drawna[COElkDraw3a$Chcs_Drawn==TRUE] <- str_trim(gsub(pattern = "(.*Chcs Drawn)(.*)(|.*)",
                                                                         replacement = "\\2",
                                                                         x = COElkDraw3a$FullString))[COElkDraw3a$Chcs_Drawn==TRUE]
  COElkDraw3a$Chcs_Drawna[COElkDraw3a$Chcs_Drawn==TRUE] <- sub(" .*$", "", COElkDraw3a$Chcs_Drawna)[COElkDraw3a$Chcs_Drawn==TRUE]# deletes everything after first space
  
  
  # drop the columns not needed
  COElkDraw3b <- select(COElkDraw3a, -FullString, -HuntCode2,-Ttl_Chce_1,-Orig_Quota,-Chcs_Drawn)
  COElkDraw3b$Ttl_Chce_1 <- "NA"
  COElkDraw3b$Ttl_Chce_1[which(COElkDraw3b$HuntCode!=FALSE)] <- COElkDraw3b$Ttl_Chce_1a[which(COElkDraw3b$HuntCode!=FALSE)-1]
  COElkDraw3b$Orig_Quota <- "NA"
  COElkDraw3b$Orig_Quota[which(COElkDraw3b$HuntCode!=FALSE)] <- COElkDraw3b$Orig_Quotaa[which(COElkDraw3b$HuntCode!=FALSE)-1]
  COElkDraw3b$Chcs_Drawn <- "NA"
  COElkDraw3b$Chcs_Drawn[which(COElkDraw3b$HuntCode!=FALSE)] <- COElkDraw3b$Chcs_Drawna[which(COElkDraw3b$HuntCode!=FALSE)]
  
  COElkDraw3c <- select(COElkDraw3b, HuntCode, Orig_Quota, Ttl_Chce_1, Chcs_Drawn)
  COElkDraw3c <- filter(COElkDraw3c, HuntCode != FALSE)
  
  #fill down if multiple hunt codes for a Ttl Chce 1
  #TODO, do we copy? or divide in half??
  COElkDraw3c$Ttl_Chce_1[which(COElkDraw3c$Ttl_Chce_1=="NA")] <- COElkDraw3c$Ttl_Chce_1[which(COElkDraw3c$Ttl_Chce_1=="NA")-1]
  COElkDraw3c$Orig_Quota[which(COElkDraw3c$Orig_Quota=="NA")] <- COElkDraw3c$Orig_Quota[which(COElkDraw3c$Orig_Quota=="NA")-1]
  COElkDraw3c$Chcs_Drawn[which(COElkDraw3c$Chcs_Drawn=="NA")] <- COElkDraw3c$Chcs_Drawn[which(COElkDraw3c$Chcs_Drawn=="NA")-1]
  
  # might have to do this again... in case there were more than 2
  
  # now its a matter of decoding the hunt codes into multiple fields, which i have already done.
  filter(COElkDraw3c, HuntCode == "EF006P5R") #350,42,67
  filter(COElkDraw3c, HuntCode == "EF371P3R") #150,0,0
  filter(COElkDraw3c, HuntCode == "EM851O1M") #5,32,5
  
  
  
  
  
  #####################################  #####################################   #####################################
  
  COElkDraw2 <- strsplit(COElkDraw, "-")
  
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
  
  rifleseasons <- c(rifle1,rifle2,rifle3,rifle4)
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