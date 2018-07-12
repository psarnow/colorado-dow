#' ---
#' title: "Download and Read PDF Harvest Tables from Colorado CPW"
#' author: "Pierre Sarnow"
#' ---

#' ## Description
#' Colorado Parks and Wildlife (CPW) aka Colorado Department of Wildlife (CDOW)
#' provides historical statistics on Big Game hunts. The tables are organized by year
#' and available for download in pdf format.
#' In the case of the project we are solely interested in Elk Rifle Hunting on public
#' land.
#' The tables are organized by hunting seasons (First-Fourth), as well as by hunting regions (Units)
#' The hunting units vary very slightly from year to year but for the most part have
#' been consistent for many years.
#' 
#' There are two tables that I will use.
#'* 'Limited' Seasons, where a draw was required to obtain a license
#'* and the Percent Success tables that breakout number of hunters for Antlered and Antlerless
#'
setwd("~/_code/colorado-dow/datasets")

#' Load required libraries for acquiring data from pdf
library(pdftools,quietly = T)
library(stringr,quietly = T)
library(toOrdinal,quietly = T) #add ordinals to numbers

# Identify the years that CDOW will provide tables for in this pdf format
years <- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)

#'  Loop through years
COElkRifleAll <- NULL # Initialize
for (iyear in years) {
  # First use the Percent Success tables for both Bull and Cows
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
  COElkEitherSex <- pdf_text(paste(iyear,"COElkHarvest",sep=""))
  
  # Having a full page in one element of a vector is not the most practical. Using strsplit 
  # will help separate lines from each other
  COElkEitherSexa <- strsplit(COElkEitherSex, "\n")
  
  # years starting in 2016 have a cover page or table of contents
  if (iyear >= 2016) { #was 2014??
    COElkEitherSexa <- COElkEitherSexa[-1:-4] # remove cover page (map, or table of contents)
  }
  
  # The document holds more information than we are after.
  # In our case we are looking for 
  tableheadings <- c(paste(iyear,"Elk Harvest, Hunters and Percent Success for First Rifle Seasons"),
                     paste(iyear,"Elk Harvest, Hunters and Percent Success for Second Rifle Seasons"),
                     paste(iyear,"Elk Harvest, Hunters and Percent Success for Third Rifle Seasons"),
                     paste(iyear,"Elk Harvest, Hunters and Percent Success for Fourth Rifle Seasons"))
  
  # Notice that the tables run across pages, there are some pages that will have
  # info we want and info we want to ignore
  
  # Identify pages with the table headings we identified
  rifle1 <- grep(tableheadings[1], COElkEitherSexa)
  rifle2 <- grep(tableheadings[2], COElkEitherSexa)
  rifle3 <- grep(tableheadings[3], COElkEitherSexa)
  rifle4 <- grep(tableheadings[4], COElkEitherSexa)
  
  rifleseasons <- unique(c(rifle1,rifle2,rifle3,rifle4))
  COElkEitherSexb <- COElkEitherSexa[rifleseasons]
  
  # the first page has the end of a previous table, remove it so we can have consistent columns
  firsttable <- COElkEitherSexb[[1]]
  # which row has the heading of our table?
  firsttablestart <- grep(tableheadings[1], firsttable)
  # drop all rows before that table
  firstpage <- firsttable[-(1:firsttablestart-1)]
  
  # the last page might have the beginning of a new table, remove it so we can have consistent columns
  lasttablepage <- length(COElkEitherSexb) # what it the last page?
  lasttable <- COElkEitherSexb[[lasttablepage]]
  # which rows have table headings? they all start with 'iyear Elk Harvest'
  lasttableend <- grep(paste(iyear,"Elk Harvest"), lasttable)
  lasttableend <- max(lasttableend) # the second entry is the start of the table to ignore
  if (lasttableend >1) {
    # drop all rows after our table
    lastpage <- lasttable[-(lasttableend:length(lasttable))]
  } else {lastpage <- lasttable}
  
  # replace updated first and last pages
  COElkEitherSexc <- COElkEitherSexb
  COElkEitherSexc[[1]] <- firstpage
  COElkEitherSexc[[lasttablepage]] <- lastpage
  
  # unlist page elements
  COElkEitherSexd <- unlist(COElkEitherSexc)
  
  ######## SEASON ONE ######## (could make a season loop as well)
  # identify season 1 data
  seasonONEstart <- grep(tableheadings[1], COElkEitherSexd)[1]
  seasonONEend <- grep(tableheadings[2], COElkEitherSexd)[1]
  # seasonONE <- COElkEitherSexd[((seasonONEstart+1):(seasonONEend-1))]
  seasonONE <- COElkEitherSexd[((seasonONEstart):(seasonONEend-1))]
  
  # remove rows with the table headers
  removeheaderrows <- grep(paste(iyear,"Elk Harvest"), seasonONE)
  seasonONE <- seasonONE[-removeheaderrows]
  
  # determine column names 
  columnnames <- grep("([:alpha:])", seasonONE)
  seasonONE1 <- seasonONE[-columnnames]
  columnnames3 <- c("Unit","Antlered.Harvest","Antlered.Hunters","Antlered.Success",
                    "Antlerless.Harvest","Antlerless.Hunters","Antlerless.Success")
  
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
  seasonTWOstart <- grep(tableheadings[2], COElkEitherSexd)[1]
  seasonTWOend <- grep(tableheadings[3], COElkEitherSexd)[1]
  # seasonTWO <- COElkEitherSexd[((seasonTWOstart+1):(seasonTWOend-1))]
  seasonTWO <- COElkEitherSexd[((seasonTWOstart):(seasonTWOend-1))]
  
  # remove rows with the table headers
  removeheaderrows <- grep(" Elk Harvest", seasonTWO)
  seasonTWO <- seasonTWO[-removeheaderrows]
  
  # determine column names 
  columnnames <- grep("([:alpha:])", seasonTWO)
  seasonTWO1 <- seasonTWO[-columnnames]
  columnnames3 <- c("Unit","Antlered.Harvest","Antlered.Hunters","Antlered.Success",
                    "Antlerless.Harvest","Antlerless.Hunters","Antlerless.Success")
  
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
  seasonTHREEstart <- grep(tableheadings[3], COElkEitherSexd)[1]
  seasonTHREEend <- grep(tableheadings[4], COElkEitherSexd)[1]
  # seasonTHREE <- COElkEitherSexd[((seasonTHREEstart+1):(seasonTHREEend-1))]
  seasonTHREE <- COElkEitherSexd[((seasonTHREEstart):(seasonTHREEend-1))]
  
  # remove rows with the table headers
  removeheaderrows <- grep(" Elk Harvest", seasonTHREE)
  seasonTHREE <- seasonTHREE[-removeheaderrows]
  
  # determine column names
  columnnames <- grep("([:alpha:])", seasonTHREE)
  seasonTHREE1 <- seasonTHREE[-columnnames]
  columnnames3 <- c("Unit","Antlered.Harvest","Antlered.Hunters","Antlered.Success",
                    "Antlerless.Harvest","Antlerless.Hunters","Antlerless.Success")
  
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
  seasonFOURstart <- grep(tableheadings[4], COElkEitherSexd)[1]
  # seasonFOUR <- COElkEitherSexd[((seasonFOURstart+1):(length(COElkEitherSexd)))] # to the end
  seasonFOUR <- COElkEitherSexd[((seasonFOURstart):(length(COElkEitherSexd)))] # to the end
  
  # remove rows with the table headers
  removeheaderrows <- grep(" Elk Harvest", seasonFOUR)
  seasonFOUR <- seasonFOUR[-removeheaderrows]
  
  # determine column names 
  columnnames <- grep("([:alpha:])", seasonFOUR)
  seasonFOUR1 <- seasonFOUR[-columnnames]
  columnnames3 <- c("Unit","Antlered.Harvest","Antlered.Hunters","Antlered.Success",
                    "Antlerless.Harvest","Antlerless.Hunters","Antlerless.Success")
  
  seasonFOUR1 <- str_trim(seasonFOUR1) # remove extra whitespace
  pagenumbers <- grep("\\s+", seasonFOUR1) # remove page numbers
  seasonFOUR2 <- seasonFOUR1[pagenumbers]
  
  # now that it is cleaned up, use the white space to separate into columns
  seasonFOUR3 <- str_split_fixed(seasonFOUR2,pattern = "\\s+", n=length(columnnames3))
  seasonFOUR3 <- as.data.frame(seasonFOUR3) # and convert into a dataframe
  colnames(seasonFOUR3) <- columnnames3 # apply our column names
  
  # add the season column
  seasonFOUR3$Season <- 4
  
  # combine season data
  COElkRifle <- rbind(seasonONE3,seasonTWO3,seasonTHREE3,seasonFOUR3)
  Antlered <- select(COElkRifle, Unit, Season, Antlered.Harvest, Antlered.Hunters, Antlered.Success)
  Antlerless <- select(COElkRifle, Unit, Season, Antlerless.Harvest, Antlerless.Hunters, Antlerless.Success)
  
  COElkRifle1 <- rbind.fill(Antlered,Antlerless)
  ##################################
  # Now, use the tables from the Limited Draws
  ## Antlered, Antlerless, Either-sex
  ## 1st, 2nd, 3rd, 4th seasons
  # 2017 include page 46, they messed the table header
  if (iyear == 2017) {
    COElkEitherSexa[[42]][1] <- "2017 Elk Harvest, Hunters and Recreation Days for 1st Season Limited Antlered Seasons"
  }

  seasons <- c(1,2,3,4)
  elks <- c("Antlered", "Antlerless", "Either")
  LimitedTables.All <- NULL
  #Season
  for (iseason in seasons) {
    LimitedTables <- NULL
    print(iseason)
    for (ielk in elks) {
      print(ielk)
      tablelabel.1 <- paste(iyear,"Elk Harvest, Hunters and Recreation Days for",toOrdinal(iseason),"Season Limited",ielk)
      tablelabel.2 <- paste(iyear,"Elk Harvest, Hunters and Recreation Days for",iseason,"Season Limited",ielk)
      
      # look for more than one space in a row, and replace it with one space
      # need to do this because the 2013 pages have a header and is causing
      # spacing problems with table headers
      COElkEitherSexa <- unlist(COElkEitherSexa)
      COElkEitherSexb <- gsub("\\s{2,}"," ", COElkEitherSexa) # make all spacings 1 space
      
      LimitedTable.1 <- c(grep(tablelabel.1, COElkEitherSexb),grep(tablelabel.2, COElkEitherSexb))
      
      # LimitedTable.2 <- COElkEitherSexb[LimitedTable.1]
      # # unlist page elements
      # LimitedTable.2 <- unlist(LimitedTable.2)
      # tablerange <- c(grep(tablelabel.1, LimitedTable.2),grep(tablelabel.2, LimitedTable.2))
      
      tablestart <- LimitedTable.1[1] #start of table
      maxtable <- max(LimitedTable.1)
      # want to keep the actual Total row, but remove the labels that have 'Total' in them
      rowswithdigits <- grep("[[:digit:]]",COElkEitherSexb) #rows with digits
      rowswithtotal <- grep("Total",COElkEitherSexb) #rows with Total
      Totalrows <- rowswithdigits[rowswithdigits %in% rowswithtotal]
      
      # The first row with our Total is the end of the table of interest
      tableend <- Totalrows[Totalrows > maxtable][1]
      LimitedTable.3 <- COElkEitherSexb[tablestart:tableend]
      
      # remove rows with letters
      rowswithletters <- grep("[[:alpha:]]",LimitedTable.3) #rows with letters
      LimitedTable.4 <- LimitedTable.3[-rowswithletters]
      # remove page numbers
      LimitedTable.4 <- str_trim(LimitedTable.4) # remove extra whitespace
      nonpagenumbers <- grep("\\s+", LimitedTable.4) 
      LimitedTable.4 <- LimitedTable.4[nonpagenumbers]
      
      # now that it is cleaned up, use the white space to separate into columns
      columnnames <- c("Unit","Harvest.Antlered","Harvest.Antlerless.Cows","Harvest.Antlerless.Calves",
                       "Harvest.Total","Hunters.Total","Success","Rec Days")
      LimitedTable.4 <- str_split_fixed(LimitedTable.4,pattern = "\\s+", n=length(columnnames))
      LimitedTable.4 <- as.data.frame(LimitedTable.4) # and convert into a dataframe
      colnames(LimitedTable.4) <- columnnames # apply our column names
      # if (ielk == "Antlered") {
      #   LimitedTable.5 <- select(LimitedTable.4, -Harvest.Antlerless.Cows,-Harvest.Antlerless.Calves)
      # } else if (ielk == "Anterlerless") {
      #   LimitedTable.5 <- select(LimitedTable.4, -Harvest.Antlered)
      # } else {
      #   LimitedTable.5 <- LimitedTable.4
      # }
      LimitedTable.5 <- LimitedTable.4
      # add the season column
      LimitedTable.5$Season <- iseason
      
      if (ielk == "Antlered") {
        LimitedTable.5$HuntCode <- paste("EM",formatC(as.numeric(as.character(LimitedTable.5$Unit)), width = 3, format = "d", flag = "0"),"O",iseason,"R",sep="")
      } else if (ielk == "Antlerless") {
        LimitedTable.5$HuntCode <- paste("EF",formatC(as.numeric(as.character(LimitedTable.5$Unit)), width = 3, format = "d", flag = "0"),"O",iseason,"R",sep="")
      } else {
        LimitedTable.5$HuntCode <- paste("EE",formatC(as.numeric(as.character(LimitedTable.5$Unit)), width = 3, format = "d", flag = "0"),"O",iseason,"R",sep="")
      }
      LimitedTables <- rbind(LimitedTables,LimitedTable.5)
      # if (!is.null(LimitedTables)) {
      #   LimitedTables <- full_join(LimitedTables,LimitedTable.5)
      # } else {LimitedTables <- LimitedTable.5}
    } 
    LimitedTables.All <- rbind(LimitedTables.All,LimitedTables)
  }
  
  
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
COElkRifleAll$Bulls <- as.numeric(levels(COElkRifleAll$Bulls))[as.integer(COElkRifleAll$Bulls)]
COElkRifleAll$Cows <- as.numeric(levels(COElkRifleAll$Cows))[as.integer(COElkRifleAll$Cows)]
COElkRifleAll$Calves <- as.numeric(levels(COElkRifleAll$Calves))[as.integer(COElkRifleAll$Calves)]

COElkRifleAll$Season <- as.character(COElkRifleAll$Season)
COElkRifleAll$Unit <- as.character(COElkRifleAll$Unit)

#' Create new statistics based on investigation
# How much effort it takes to have a successful result
COElkRifleAll$Harvest_Effort <- COElkRifleAll$Days / COElkRifleAll$Harvest # From Phase I investigation
COElkRifleAll$Harvest_Effort[is.infinite(COElkRifleAll$Harvest_Effort)] <- NA # we get an inf where there was no harvest
COElkRifleAll$Success_Harvest <- COElkRifleAll$Harvest * (COElkRifleAll$Success / 100)

#' Peek at the dataframe
head(COElkRifleAll)