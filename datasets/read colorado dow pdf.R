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

######## SEASON ONE ######## 
# identify season 1 data
seasonONEstart <- grep(tableheadings[1], COElk2016d)[1]
seasonONEend <- grep(tableheadings[2], COElk2016d)[1]
seasonONE <- COElk2016d[((seasonONEstart+1):(seasonONEend-1))]

# remove rows with the table headers
removeheaderrows <- grep("2016 Elk Harvest", seasonONE)
seasonONE <- seasonONE[-removeheaderrows]

# possible column names 
columnnames <- grep("([:alpha:])", seasonONE)
# numeric data
seasonONE1 <- seasonONE[-columnnames]
columnnames1 <- seasonONE[columnnames]
columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
columnnames2 <- columnnames1[length(columnnames1)]
columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
colremove <- grep("([.])",columnnames3)
columnnames3 <- columnnames3[-colremove]

# numeric data
seasonONE1 <- seasonONE1[-columnnames]
seasonONE1 <- str_trim(seasonONE1) # remove extra whitespace
pagenumbers <- grep("\\s+", seasonONE1) # remove page numbers
seasonONE2 <- seasonONE1[pagenumbers]

# now that it is cleaned up, use the white space to separate into columns
seasonONE3 <- str_split_fixed(seasonONE2,pattern = "\\s+", n=length(columnnames3))
seasonONE3 <- as.data.frame(seasonONE3) # and convert into a dataframe
colnames(seasonONE3) <- columnnames3 # apply our column names
seasonONE4 <- sapply(seasonONE3, as.numeric) # convert the fields to numeric
seasonONE4 <- as.data.frame(seasonONE4)

# add the season column
seasonONE4$season <- 1

######## SEASON TWO ######## 
# identify season 2 data
seasonTWOstart <- grep(tableheadings[2], COElk2016d)[1]
seasonTWOend <- grep(tableheadings[3], COElk2016d)[1]
seasonTWO <- COElk2016d[((seasonTWOstart+1):(seasonTWOend-1))]

# remove rows with the table headers
removeheaderrows <- grep("2016 Elk Harvest", seasonTWO)
seasonTWO <- seasonTWO[-removeheaderrows]

# possible column names 
columnnames <- grep("([:alpha:])", seasonTWO)
# numeric data
seasonTWO1 <- seasonTWO[-columnnames]
columnnames1 <- seasonTWO[columnnames]
columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
columnnames2 <- columnnames1[length(columnnames1)]
columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
colremove <- grep("([.])",columnnames3)
columnnames3 <- columnnames3[-colremove]

# numeric data
seasonTWO1 <- seasonTWO1[-columnnames]
seasonTWO1 <- str_trim(seasonTWO1) # remove extra whitespace
pagenumbers <- grep("\\s+", seasonTWO1) # remove page numbers
seasonTWO2 <- seasonTWO1[pagenumbers]

# now that it is cleaned up, use the white space to separate into columns
seasonTWO3 <- str_split_fixed(seasonTWO2,pattern = "\\s+", n=length(columnnames3))
seasonTWO3 <- as.data.frame(seasonTWO3) # and convert into a dataframe
colnames(seasonTWO3) <- columnnames3 # apply our column names
seasonTWO4 <- sapply(seasonTWO3, as.numeric) # convert the fields to numeric
seasonTWO4 <- as.data.frame(seasonTWO4)

# add the season column
seasonTWO4$season <- 2

######## SEASON THREE ######## 
# identify season 2 data
seasonTHREEstart <- grep(tableheadings[3], COElk2016d)[1]
seasonTHREEend <- grep(tableheadings[4], COElk2016d)[1]
seasonTHREE <- COElk2016d[((seasonTHREEstart+1):(seasonTHREEend-1))]

# remove rows with the table headers
removeheaderrows <- grep("2016 Elk Harvest", seasonTHREE)
seasonTHREE <- seasonTHREE[-removeheaderrows]

# possible column names 
columnnames <- grep("([:alpha:])", seasonTHREE)
# numeric data
seasonTHREE1 <- seasonTHREE[-columnnames]
columnnames1 <- seasonTHREE[columnnames]
columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
columnnames2 <- columnnames1[length(columnnames1)]
columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
colremove <- grep("([.])",columnnames3)
columnnames3 <- columnnames3[-colremove]

# numeric data
seasonTHREE1 <- seasonTHREE1[-columnnames]
seasonTHREE1 <- str_trim(seasonTHREE1) # remove extra whitespace
pagenumbers <- grep("\\s+", seasonTHREE1) # remove page numbers
seasonTHREE2 <- seasonTHREE1[pagenumbers]

# now that it is cleaned up, use the white space to separate into columns
seasonTHREE3 <- str_split_fixed(seasonTHREE2,pattern = "\\s+", n=length(columnnames3))
seasonTHREE3 <- as.data.frame(seasonTHREE3) # and convert into a dataframe
colnames(seasonTHREE3) <- columnnames3 # apply our column names
seasonTHREE4 <- sapply(seasonTHREE3, as.numeric) # convert the fields to numeric
seasonTHREE4 <- as.data.frame(seasonTHREE4)

# add the season column
seasonTHREE4$season <- 3


######## SEASON FOUR ######## 
# identify season 2 data
seasonFOURstart <- grep(tableheadings[4], COElk2016d)[1]
seasonFOUR <- COElk2016d[((seasonFOURstart+1):(length(COElk2016d)))] # to the end

# remove rows with the table headers
removeheaderrows <- grep("2016 Elk Harvest", seasonFOUR)
seasonFOUR <- seasonFOUR[-removeheaderrows]

# possible column names 
columnnames <- grep("([:alpha:])", seasonFOUR)
# numeric data
seasonFOUR1 <- seasonFOUR[-columnnames]
columnnames1 <- seasonFOUR[columnnames]
columnnames1 <- columnnames1[-length(columnnames1)] # we know the last row is a 'Total' summary, not a name
columnnames2 <- columnnames1[length(columnnames1)]
columnnames2 <- str_trim(columnnames2) # remove any extra whitespace
columnnames3 <- unlist(strsplit(columnnames2,split = "\\s+"))
colremove <- grep("([.])",columnnames3)
columnnames3 <- columnnames3[-colremove]

# numeric data
seasonFOUR1 <- seasonFOUR1[-columnnames]
seasonFOUR1 <- str_trim(seasonFOUR1) # remove extra whitespace
pagenumbers <- grep("\\s+", seasonFOUR1) # remove page numbers
seasonFOUR2 <- seasonFOUR1[pagenumbers]

# now that it is cleaned up, use the white space to separate into columns
seasonFOUR3 <- str_split_fixed(seasonFOUR2,pattern = "\\s+", n=length(columnnames3))
seasonFOUR3 <- as.data.frame(seasonFOUR3) # and convert into a dataframe
colnames(seasonFOUR3) <- columnnames3 # apply our column names
seasonFOUR4 <- sapply(seasonFOUR3, as.numeric) # convert the fields to numeric
seasonFOUR4 <- as.data.frame(seasonFOUR4)

# add the season column
seasonFOUR4$season <- 4

#Combine
COElk2016Rifle <- rbind(seasonONE4,seasonTWO4,seasonTHREE4,seasonFOUR4)


