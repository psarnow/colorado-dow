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
  COElkHarvestData <- pdf_text(paste(iyear,"COElkHarvest",sep=""))
  
  # Having a full page in one element of a vector is not the most practical. Using strsplit 
  # will help separate lines from each other
  COElkHarvestData <- strsplit(COElkHarvestData, "\n")
  
  # years starting in 2016 have a cover page or table of contents
  if (iyear >= 2016) { #was 2014??
    COElkHarvestData <- COElkHarvestData[-1:-4] # remove cover page (map, or table of contents)
  }
  
  # 2017 include page 46, they messed the table header
  if (iyear == 2017) {
    COElkHarvestData[[42]][1] <- "2017 Elk Harvest, Hunters and Recreation Days for 1st Season Limited Antlered Seasons"
  }

  # look for more than one space in a row, and replace it with one space
  # need to do this because the 2013 pages have a header and is causing
  # spacing problems with table headers
  COElkHarvestData <- unlist(COElkHarvestData)
  COElkHarvestData.1 <- gsub("\\s{2,}"," ", COElkHarvestData) # make all spacings 1 space
  
  # Success tables for each season
  seasons <- c("First","Second","Third","Fourth")
  SuccessTables <- NULL
  #Season
  for (iseason in seasons) {
    tablelabel <- paste(iyear,"Elk Harvest, Hunters and Percent Success for",iseason,"Rifle Seasons")

    # which rows have our table header?
    SuccessTableRows <- grep(tablelabel, COElkHarvestData.1)
    
    tablestart <- SuccessTableRows[1] #start of table
    maxtable <- max(SuccessTableRows)
    # use the Total row to end the table
    rowswithtotal <- grep("Total",COElkHarvestData.1) #rows with Total
    # The first row with our Total is the end of the table of interest
    tableend <- rowswithtotal[rowswithtotal > maxtable][1]
    SuccessTable.1 <- COElkHarvestData.1[tablestart:tableend]
    
    # remove rows with letters
    rowswithletters <- grep("[[:alpha:]]",SuccessTable.1) #rows with letters
    SuccessTable.2 <- SuccessTable.1[-rowswithletters]
    # remove page numbers
    SuccessTable.2 <- str_trim(SuccessTable.2) # remove extra whitespace
    nonpagenumbers <- grep("\\s+", SuccessTable.2) 
    SuccessTable.2 <- SuccessTable.2[nonpagenumbers]
    
    # now that it is cleaned up, use the white space to separate into columns
    columnnames <- c("Unit","Harvest.Antlered","Hunters.Antlered","Success.Antlered",
                     "Harvest.Antlerless","Hunters.Antlerless","Success.Antlerless")
    SuccessTable.2 <- str_split_fixed(SuccessTable.2,pattern = "\\s+", n=length(columnnames))
    SuccessTable.2 <- as.data.frame(SuccessTable.2) # and convert into a dataframe
    colnames(SuccessTable.2) <- columnnames # apply our column names
    
    SuccessTable.3 <- SuccessTable.2
    # add the season column
    SuccessTable.3$Season <- which(seasons %in% iseason)
    SuccessTable.3$Unit <- as.character(SuccessTable.3$Unit) #update field class
    
    SuccessTables <- rbind(SuccessTables,SuccessTable.3)
  }
  
  # Gather on Elk type
  SuccessTables.Antlered <- select(SuccessTables, -Harvest.Antlerless, -Hunters.Antlerless, -Success.Antlerless)
  SuccessTables.Antlered$HuntCode <- paste("EM",formatC(as.numeric(SuccessTables.Antlered$Unit), width = 3, format = "d", flag = "0"),"O",SuccessTables.Antlered$Season,"R",sep="")
  
  SuccessTables.Antlerless <- select(SuccessTables, -Harvest.Antlered, -Hunters.Antlered, -Success.Antlered)
  SuccessTables.Antlerless$HuntCode <- paste("EF",formatC(as.numeric(SuccessTables.Antlerless$Unit), width = 3, format = "d", flag = "0"),"O",SuccessTables.Antlerless$Season,"R",sep="")
  
  SuccessTables <- rbind.fill(SuccessTables.Antlered,SuccessTables.Antlerless)
  # Clean up field data types
  SuccessTables$Hunters.Antlered <- as.numeric(gsub(",", "", SuccessTables$Hunters.Antlered))
  SuccessTables$Hunters.Antlerless <- as.numeric(gsub(",", "", SuccessTables$Hunters.Antlerless))
  SuccessTables$Season <- as.character(SuccessTables$Season)
  # SuccessTables$Harvest.Antlered <- as.numeric(levels(SuccessTables$Harvest.Antlered))[as.integer(SuccessTables$Harvest.Antlered)]
  # SuccessTables$Success.Antlered <- as.numeric(levels(SuccessTables$Success.Antlered))[as.integer(SuccessTables$Success.Antlered)]
  # SuccessTables$Harvest.Antlerless <- as.numeric(levels(SuccessTables$Harvest.Antlerless))[as.integer(SuccessTables$Harvest.Antlerless)]
  # SuccessTables$Success.Antlerless <- as.numeric(levels(SuccessTables$Success.Antlerless))[as.integer(SuccessTables$Success.Antlerless)]
  
  # Now, use the tables from the Limited Draws
  ## 1st, 2nd, 3rd, 4th seasons
  ## Antlered, Antlerless, Either-sex
  seasons <- c(1,2,3,4)
  # elks <- c("Antlered", "Antlerless", "Either") #FUTURE?
  elks <- c("Either")
  LimitedTables.All <- NULL
  #Season
  for (iseason in seasons) {
    LimitedTables <- NULL
    for (ielk in elks) {
      tablelabel.1 <- paste(iyear,"Elk Harvest, Hunters and Recreation Days for",toOrdinal(iseason),"Season Limited",ielk)
      tablelabel.2 <- paste(iyear,"Elk Harvest, Hunters and Recreation Days for",iseason,"Season Limited",ielk)
      
      # # look for more than one space in a row, and replace it with one space
      # # need to do this because the 2013 pages have a header and is causing
      # # spacing problems with table headers

      # which rows have our table header?
      LimitedTableRows <- c(grep(tablelabel.1, COElkHarvestData.1),grep(tablelabel.2, COElkHarvestData.1))
      
      tablestart <- LimitedTableRows[1] #start of table
      maxtable <- max(LimitedTableRows)
      # want to keep the actual Total row, but remove the labels that have 'Total' in them
      rowswithdigits <- grep("[[:digit:]]",COElkHarvestData.1) #rows with digits
      rowswithtotal <- grep("Total",COElkHarvestData.1) #rows with Total
      Totalrows <- rowswithdigits[rowswithdigits %in% rowswithtotal]
      
      # The first row with our Total is the end of the table of interest
      tableend <- Totalrows[Totalrows > maxtable][1]
      LimitedTable.1 <- COElkHarvestData.1[tablestart:tableend]
      
      # remove rows with letters
      rowswithletters <- grep("[[:alpha:]]",LimitedTable.1) #rows with letters
      LimitedTable.2 <- LimitedTable.1[-rowswithletters]
      # remove page numbers
      LimitedTable.2 <- str_trim(LimitedTable.2) # remove extra whitespace
      nonpagenumbers <- grep("\\s+", LimitedTable.2) 
      LimitedTable.2 <- LimitedTable.2[nonpagenumbers]
      
      # now that it is cleaned up, use the white space to separate into columns
      columnnames <- c("Unit","Harvest.Antlered","Harvest.Antlerless.Cows","Harvest.Antlerless.Calves",
                       "Harvest.Total","Hunters.Total","Success","RecDays")
      LimitedTable.2 <- str_split_fixed(LimitedTable.2,pattern = "\\s+", n=length(columnnames))
      LimitedTable.2 <- as.data.frame(LimitedTable.2) # and convert into a dataframe
      colnames(LimitedTable.2) <- columnnames # apply our column names

      LimitedTable.3 <- LimitedTable.2
      # add the season column
      LimitedTable.3$Season <- as.character(iseason)
      
      LimitedTable.3$Unit <- as.character(LimitedTable.3$Unit)
      
      if (ielk == "Antlered") {
        LimitedTable.3$HuntCode <- paste("EM",formatC(as.numeric(LimitedTable.3$Unit), width = 3, format = "d", flag = "0"),"O",iseason,"R",sep="")
        # all of the hunters were hunting Antlered
        colnames(LimitedTable.3)[colnames(LimitedTable.3)=="Hunters.Total"] <- "Hunters.Antlered"
      } else if (ielk == "Antlerless") {
        LimitedTable.3$HuntCode <- paste("EF",formatC(as.numeric(LimitedTable.3$Unit), width = 3, format = "d", flag = "0"),"O",iseason,"R",sep="")
        # all of the hunters were hunting Antlerless
        colnames(LimitedTable.3)[colnames(LimitedTable.3)=="Hunters.Total"] <- "Hunters.Antlerless"
      } else {
        LimitedTable.3$HuntCode <- paste("EE",formatC(as.numeric(LimitedTable.3$Unit), width = 3, format = "d", flag = "0"),"O",iseason,"R",sep="")
        # all of the hunters were hunting Either
        colnames(LimitedTable.3)[colnames(LimitedTable.3)=="Hunters.Total"] <- "Hunters.Either"
      }
      LimitedTables <- rbind.fill(LimitedTables,LimitedTable.3)
     
    } 
    LimitedTables.All <- rbind(LimitedTables.All,LimitedTables)
   
  }
  
  LimitedTables.All <- select(LimitedTables.All, -Harvest.Total, -RecDays) # remove some fields not interested in right now
  # Clean up field data types
  LimitedTables.All$Hunters.Either <- as.numeric(gsub(",", "", LimitedTables.All$Hunters.Either))
  LimitedTables.All$Success <- as.numeric(levels(LimitedTables.All$Success))[as.integer(LimitedTables.All$Success)]
  
  LimitedTables.All$Harvest.Antlered <- as.numeric(levels(LimitedTables.All$Harvest.Antlered))[as.integer(LimitedTables.All$Harvest.Antlered)]
  LimitedTables.All$Harvest.Antlerless.Cows <- as.numeric(levels(LimitedTables.All$Harvest.Antlerless.Cows))[as.integer(LimitedTables.All$Harvest.Antlerless.Cows)]
  LimitedTables.All$Harvest.Antlerless.Calves <- as.numeric(levels(LimitedTables.All$Harvest.Antlerless.Calves))[as.integer(LimitedTables.All$Harvest.Antlerless.Calves)]
  LimitedTables.All$Harvest.Antlerless <- LimitedTables.All$Harvest.Antlerless.Cows + LimitedTables.All$Harvest.Antlerless.Calves #combine to match other tables
  LimitedTables.All <- select(LimitedTables.All, -Harvest.Antlerless.Cows, -Harvest.Antlerless.Calves)
  
  #Combine tables
  # ID by huntcode
  ## HuntCode, Unit, Season, Harvest.Antlered, Harvest.Antlerless, Hunters.Antlered, Hunters.Anterless, Hunters.Either, Draw
  ## EE077O1R, 77     1           109                 53                  0                 0                 397        Yes   
  ## EF077O2R, 77     2           0                   24                  0                 74                0          Yes
  ## EM077O2R, 77     2           194                 0                   1003              0                 0          No
  
  # Might only need the Either Limited Tables, the Success tables has the rest of the data
  # Will need to gather the success tables into hunt codes (Antler,Antless)
  # The Draw field could be determined if there is a corresponding value from the Draw Results tables
  
  # testL <- filter(LimitedTables.All, Unit == "18")
  # testS <- filter(SuccessTables, Unit == "18")
  
  COElkRifle <- rbind.fill(SuccessTables,LimitedTables.All)
  # test <- filter(COElkRifle, Unit == "18" & Season == "1")
  
  # COElkRifle <- left_join(LimitedTables.All,SuccessTables)
  COElkRifle$Year <- as.character(iyear)
  COElkRifleAll <- rbind(COElkRifleAll,COElkRifle)
}

# will probably want to remove 'success' fields from the tables and recalc ourselves

#' Create new statistics based on investigation
# How much effort it takes to have a successful result
# COElkRifleAll$Harvest_Effort <- COElkRifleAll$Days / COElkRifleAll$Harvest # From Phase I investigation
# COElkRifleAll$Harvest_Effort[is.infinite(COElkRifleAll$Harvest_Effort)] <- NA # we get an inf where there was no harvest
# COElkRifleAll$Success_Harvest <- COElkRifleAll$Harvest * (COElkRifleAll$Success / 100)

#' Peek at the dataframe
head(COElkRifleAll)

test <- filter(COElkRifleAll, Unit == "77")
test
