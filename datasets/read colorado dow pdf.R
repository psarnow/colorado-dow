require(pdftools)


# The document holds more information than we are after. Page 1 has a table of contents that
# indicates the different data tables provided.
# In our case we are looking for 
tableheadings <- c("2016 Elk Harvest, Hunters and Recreation Days for First Rifle Seasons",
                   "2016 Elk Harvest, Hunters and Recreation Days for Second Rifle Seasons",
                   "2016 Elk Harvest, Hunters and Recreation Days for Third Rifle Seasons",
                   "2016 Elk Harvest, Hunters and Recreation Days for Fourth Rifle Seasons")

# Notice that the tables run across pages, there are some pages that will have
# info we want and info we want to ignore

# Lets get started by downloading the pdf from cpw
download.file("http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/2016StatewideElkHarvest.pdf",
              "2016COElkHarvest")

# This function will directly export the raw text in a character vector with spaces to show 
# the white space and \n to show the line breaks.
COElk2016 <- pdf_text("2016COElkHarvest")

# Having a full page in one element of a vector is not the most practical. Using strsplit 
# will help separate lines from each other:

COElk2016a <- strsplit(COElk2016, "\n")
head(COElk2016a[[1]])

# Identify pages with the table headings we identified... ignoring page one's table of contents
rifle1 <- grep(tableheadings[1], COElk2016a)[-1]
rifle2 <- grep(tableheadings[2], COElk2016a)[-1]
rifle3 <- grep(tableheadings[3], COElk2016a)[-1]
rifle4 <- grep(tableheadings[4], COElk2016a)[-1]

rifleseasons <- c(rifle1,rifle2,rifle3,rifle4)
COElk2016b <- COElk2016a[rifleseasons]

# the first page has the end of a previous table, remove it so we can have consistent columns
firsttable <- COElk2016b[[1]]
# which row has the heading of our table
firsttablestart <- grep(tableheadings[1], firsttable)
# drop all rows before our table
firstpage <- firsttable[-(1:firsttablestart-1)]

# the last page has the beginning of a new table, remove it so we can have consistent columns
lasttablepage <- length(COElk2016b) # what is the last page?
lasttable <- COElk2016b[[lasttablepage]]
# which rows have table headings? they all start with '2016 Elk Harvest'
lasttableend <- grep("2016 Elk Harvest", lasttable)[-1] # the second entry is the start of the table to ignore
# drop all rows after our table
lastpage <- lasttable[-(lasttableend:length(lasttable))]
  
# replace updated first and last pages
COElk2016c <- COElk2016b
COElk2016c[[1]] <- firstpage
COElk2016c[[lasttablepage]] <- lastpage

# unlist page elements
COElk2016d <- unlist(COElk2016c)

# identify season 1 data
seasonONEstart <- grep(tableheadings[1], COElk2016d)[1]
seasonONEend <- grep(tableheadings[2], COElk2016d)[1]
seasonONE <- COElk2016d[((seasonONEstart+1):(seasonONEend-1))]

# remove rows with the table headers
removeheaderrows <- grep("2016 Elk Harvest", seasonONE)
seasonONE <- seasonONE[-removeheaderrows]



# convert to numeric
seasonONE1 <- matrix(seasonONE)

COElk2016e <- matrix(COElk2016d,ncol = 7)
COElk2016f <- as.data.frame(COElk2016e)





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


library("tabulizer")

rifleseasons <- rifleseasons + 4 #which pages to read
COElk2016tables <- extract_tables(file="http://cpw.state.co.us/Documents/Hunting/BigGame/Statistics/Elk/2016StatewideElkHarvest.pdf", output = 'matrix')
COElk2016tables1 <- COElk2016tables[rifleseasons]




# remove the page headers (not column names)
# Remove headers on other pages
header_rows <- grep("2016 Elk", COElk2016c)
doc[header_rows] <- "page" # I put a marker here that will be useful later
doc <- doc[- (header_rows - 1)]
COElk2016b[[1]]



