require(pdftools)


download.file("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/2016StatewideElkHarvest.pdf",
              "2016COElkHarvest")
COElk2016 <- pdf_text("2016COElkHarvest")

# This function will directly export the raw text in a character vector with spaces to show 
# the white space and \n to show the line breaks.

# Having a full page in one element of a vector is not the most practical. Using strsplit 
# will help you separate lines from each other:

COElk2016a <- strsplit(COElk2016, "\n")
head(COElk2016a[[1]])

# each element is a page
## in this case, the tables start on page 5
COElk2016b <- COElk2016a[-(1:4)]
head(COElk2016b)

# there is a mix of tables in the pdf.  list what they are (they are in the first row of each)
tablenames <- as.character(lapply(COElk2016b, `[[`, 1))
tablenames1 <- as.data.frame(tablenames)
unique(tablenames1$tablenames)

# for starters, we only want the First thru Fourth Rifle Seasons
## for example
# 2016 Elk Harvest, Hunters and Recreation Days for First Rifle Seasons
rifleseasons1 <- grep("2016 Elk Harvest, Hunters and Recreation Days for First Rifle Seasons", lapply(COElk2016b, `[[`, 1))
rifleseasons2 <- grep("2016 Elk Harvest, Hunters and Recreation Days for Second Rifle Seasons", lapply(COElk2016b, `[[`, 1))
rifleseasons3 <- grep("2016 Elk Harvest, Hunters and Recreation Days for Third Rifle Seasons", lapply(COElk2016b, `[[`, 1))
rifleseasons4 <- grep("2016 Elk Harvest, Hunters and Recreation Days for Fourth Rifle Seasons", lapply(COElk2016b, `[[`, 1))
rifleseasons <- c(rifleseasons1,rifleseasons2,rifleseasons3,rifleseasons4)
COElk2016c <- COElk2016b[rifleseasons]


# remove the page headers (not column names)
# Remove headers on other pages
header_rows <- grep("2016 Elk", COElk2016b) # Remember: \f are for page breaks
doc[header_rows] <- "page" # I put a marker here that will be useful later
doc <- doc[- (header_rows - 1)]
COElk2016b[[1]]