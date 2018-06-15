#' ---
#' title: "Download and Read PDF Draw Summaries from Colorado CPW"
#' author: "Pierre Sarnow"
#' ---

#' ## Description
#' Colorado Parks and Wildlife (CPW) aka Colorado Department of Wildlife (CDOW)
#' provides historical statistics on Big Game Draw Results. The 'Draw' is the application process
#' that CPW utilizes. Altogether it is rather complex when you consider all of the possible options,
#' but here we will initially limit the analysis to the general hunt seasons, and combine
#' the hunter types (Resident, nonResident, Youth). Hunters apply using hunt codes in the Spring,
#' and CPW posts the results of the Draw in early summer for fall hunts.
#'
setwd("~/_code/colorado-dow/datasets")
library(dplyr,quietly = T)
library(tidyr,quietly = T)
library(pdftools,quietly = T)
library(stringr,quietly = T)

# Identify the years that CPW will provide tables for in this pdf format
years <- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)

#'  Loop through years
COElkDrawAll <- NULL # Initialize
for (iyear in years) {
  
  # RUN ONCE to download
  # if (iyear >= 2015) {
  #   download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
  #                       iyear,"ElkDrawRecap.pdf",sep=""),
  #                 paste(iyear,"COElkDraw",sep=""))
  # } else {
  #   download.file(paste("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/",
  #                       iyear,"ElkDrawSummary.pdf",sep=""),
  #                 paste(iyear,"COElkDraw",sep=""))
  # }
  
  # This function will directly export the raw text in a character vector with spaces to show 
  # the white space and \n to show the line breaks.
  COElkDraw <- pdf_text(paste(iyear,"COElkDraw",sep=""))
  
  # Having a full page in one element of a vector is not the most practical. Using strsplit 
  # will help separate lines from each other
  COElkDraw <- strsplit(COElkDraw, "\n")

  if (iyear >= 2015) { #different table format for recent years
    #remove the first two summary pages
    COElkDrawa <- COElkDraw[-1:-2]

    # unlist page elements
    COElkDraw1 <- unlist(COElkDrawa)
    COElkDraw1 <- str_trim(COElkDraw1)
    # remove rows with
    removerows <- c(grep("Colorado Parks", COElkDraw1), 
                      grep("Primary Elk Draw", COElkDraw1), 
                      grep("Youth Preference", COElkDraw1), 
                      grep("Landowner Leftover", COElkDraw1),
                      grep("Draw Recap", COElkDraw1),
                    grep("Determined", COElkDraw1),
                    grep("by Draw", COElkDraw1),
                    grep("Successful", COElkDraw1),
                    grep("Drawn Out At", COElkDraw1)
                    )
    COElkDraw2 <- COElkDraw1[-removerows]
    
    # index of rows that we are interested in, based on unique text of nearby fields
    HuntCode<- grep("Total Quota", COElkDraw2)-1 #hunt code is one row above this on each page
    TotalQuota <- grep("Total Quota", COElkDraw2)+3 #quota is 3 rows below
    TotalChoice1 <- grep("General Apps", COElkDraw2)+1 #choice 1 total is one row below
    NumDrawn <- grep("#DrawnHuntCode", gsub(" ", "", COElkDraw2, fixed = TRUE))+2 #drawn is 2 rows below

    # put data we are intested in into a dataframe
    COElkDraw4 <- NULL
    COElkDraw4$HuntCode <- COElkDraw2[HuntCode]
    COElkDraw4 <- as.data.frame(COElkDraw4)
    COElkDraw4$Orig_Quota <- COElkDraw2[TotalQuota]
    COElkDraw4$Orig_Quota <- as.numeric(sub(" .*$", "", COElkDraw4$Orig_Quota))
    COElkDraw4$Ttl_Chce_1 <- COElkDraw2[TotalChoice1]
    COElkDraw4$Ttl_Chce_1 <- str_trim(gsub(pattern = "(.*Total Choice 1)(.*)",replacement = "\\2",x = COElkDraw4$Ttl_Chce_1))
    COElkDraw4$Ttl_Chce_1 <- as.numeric(sub(" .*$", "", COElkDraw4$Ttl_Chce_1))
    COElkDraw4$Chcs_Drawn <- COElkDraw2[NumDrawn]
    COElkDraw4$Chcs_Drawn <- as.numeric(sub(" .*$", "", COElkDraw4$Chcs_Drawn))
    
    # some of the sheets combine hunt codes, here we will duplicate stats on each of them
    multihuntcodes <- filter(COElkDraw4, str_count(HuntCode) > 8)
    multihuntcodes <- separate(multihuntcodes, HuntCode, sep = " ",LETTERS)
    multihuntcodes <- gather(multihuntcodes,"ignore",HuntCode,A:Z)
    multihuntcodes <- select(multihuntcodes, -ignore)
    multihuntcodes <- filter(multihuntcodes, !is.na(HuntCode))
    COElkDraw4 <- filter(COElkDraw4, str_count(HuntCode) <= 8)
    COElkDraw4 <- rbind(COElkDraw4,multihuntcodes)
    
  } else {
    # unlist page elements
    COElkDraw1 <- unlist(COElkDraw)
    # remove page headings
    pageheadings <- c(grep("Date", COElkDraw1), grep("Time", COElkDraw1), grep("Elk", COElkDraw1), grep("HntCde", COElkDraw1))
    # drop all rows with the page heading
    COElkDraw2 <- COElkDraw1[-pageheadings]
    # identify the rows we are interested in
    rowsofinterest <- grep("Orig Quota|Chcs Drawn|Choice 1 % Success", COElkDraw2)
    COElkDraw3 <- COElkDraw2[rowsofinterest]
    COElkDraw3 <- str_trim(COElkDraw3)
    
    # put data we are intested in into a dataframe
    COElkDraw3a <- as.data.frame(COElkDraw3)
    colnames(COElkDraw3a) <- "FullString"
    
    #Hunt Code
    COElkDraw3a$HuntCode <- FALSE #initialize
    COElkDraw3a$HuntCode[grep("E", COElkDraw3a$FullString)] <- TRUE #which rows have the code?
    
    COElkDraw3a$HuntCode2 <- substring(COElkDraw3a$FullString,1,8) #grab the code from those rows
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
    # TODO might have to do this again... in case there were more than 2
    
    COElkDraw4 <- COElkDraw3c
  }
  
  #DECODE HuntCode
  COElkDraw4$Animal <- substring(COElkDraw4$HuntCode, 1, 1)
  COElkDraw4$Sex <- substring(COElkDraw4$HuntCode, 2, 2)
  COElkDraw4$Unit <- substring(COElkDraw4$HuntCode, 3, 5)
  COElkDraw4$Season_Type <- substring(COElkDraw4$HuntCode, 6, 6)
  COElkDraw4$Season <- substring(COElkDraw4$HuntCode, 7, 7)
  COElkDraw4$Type <- substring(COElkDraw4$HuntCode, 8, 8)
  
  #Only looking at Elk right now
  COElkDraw4 <- filter(COElkDraw4, Animal == "E")
  COElkDraw4 <- select(COElkDraw4, -Animal)
  
  #Only looking at General hunting seasons right now
  COElkDraw4 <- filter(COElkDraw4, Season_Type == "O")
  COElkDraw4 <- select(COElkDraw4, -Season_Type)
  
  #sex
  COElkDraw4$Sex[COElkDraw4$Sex=="E"] <- "Either"
  COElkDraw4$Sex[COElkDraw4$Sex=="M"] <- "Bull"
  COElkDraw4$Sex[COElkDraw4$Sex=="F"] <- "Cow"
  
  # remove preceeding zeros from hunt units
  COElkDraw4$Unit <- as.character(as.numeric(COElkDraw4$Unit))
  
  #Only looking at Rifle hunting seasons right now
  COElkDraw4 <- filter(COElkDraw4, Type=="R")
  COElkDraw4 <- select(COElkDraw4, -Type)
  
  #Drop HuntCode
  COElkDraw4 <- select(COElkDraw4, -HuntCode)
  
  #Clean up field classes
  COElkDraw4$Orig_Quota <- as.numeric(COElkDraw4$Orig_Quota)
  COElkDraw4$Ttl_Chce_1 <- as.numeric(COElkDraw4$Ttl_Chce_1)
  COElkDraw4$Chcs_Drawn <- as.numeric(COElkDraw4$Chcs_Drawn)
  
  #Combine
  COElkDraw4$Year <- as.character(iyear)
  COElkDrawAll <- rbind(COElkDrawAll,COElkDraw4)
  
}

#' Calculate Draw Success Rate
COElkDrawAll$Draw_Success <- COElkDrawAll$Chcs_Drawn / COElkDrawAll$Ttl_Chce_1

#' Spread rows based on draw Sex
COElkDrawAll2 <- COElkDrawAll %>%
  gather(label,value,Draw_Success,Ttl_Chce_1,Orig_Quota,Chcs_Drawn) %>%
  unite(label1, Sex, label, sep = ".") %>% 
  spread(label1, value) 
  
#' Peek at the dataframe
head(COElkDrawAll2)