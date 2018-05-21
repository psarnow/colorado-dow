# extract raw text from PDF files
# https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e

# Use the tm package
# tm is the go-to package when it comes to doing text mining/analysis in R.

# install.packages("tm")
require(tm)


# For our problem, it will help us import a PDF document in R while keeping its structure intact. 
# Plus, it makes it ready for any text analysis you want to do later.
# 
# The readPDF function from the tm package doesn’t actually read a PDF file. 
# Instead, it will help you create your own function, the benefit of it being that you can choose whatever PDF 
# extracting engine you want.

# By default, it will use xpdf, available at http://www.xpdfreader.com/download.html

# You have to:
  
##Download the archive from the website (under the Xpdf tools section).
##Unzip it.
##Make sure it is in the PATH of your computer.

read <- readPDF(control = list(text = "-layout"))

####################################################################################################################
### or use a different package
# Use pdftools::pdf_text
# install.packages("pdftools")
require(pdftools)

# A quick glance at the documentation will show you the few functions of the package, 
# the most important of which being pdf_text.

# For this article, I will use an official record from the UN that you can find on this link.
# https://github.com/Huitziii/crispy-pdf/raw/master/71_PV.62.pdf

download.file("https://github.com/Huitziii/crispy-pdf/raw/master/71_PV.62.pdf",
              "./71_PV.62.pdf")
text <- pdf_text("./71_PV.62.pdf")

# This function will directly export the raw text in a character vector with spaces to show 
# the white space and \n to show the line breaks.

# Having a full page in one element of a vector is not the most practical. Using strsplit 
# will help you separate lines from each other:

text2 <- strsplit(text, "\n")
head(text2[[1]])

# If you want to know more about the functions of the pdftools package, 
# I recommend you read Introducing pdftools - A fast and portable PDF extractor, written by the author himself.
# https://ropensci.org/blog/2016/03/01/pdftools-and-jeroen/

# Also look at this devtools r table reader
# https://github.com/ropensci/tabulizer


##################################################################################
# Once you have the PDF document in R, you want to extract the actual pieces of text that interest you, 
# and get rid of the rest.
# 
# That’s what this part is about.
# 
# I will use a few common tools for string manipulation in R:
#   
#   The grep and grepl functions.
# Base string manipulation functions (such as str_split).
# The stringr package.
# My goal is to extract all the speeches from the speakers of the document we’ve worked on so far (this one), 
# but I don’t care about the speeches from the president.
# 
# Here are the steps I will follow:
#   
#   Clean the headers and footers on all pages.
# Get the two columns together.
# Find the rows of the speakers.
# Extract the correct rows.
# I will use regular expressions (regex) regularly in the code. If you have absolutely no knowledge of it, 
# I recommend you go follow a tutorial, because it is essential as soon as you start managing text data.
# 
# If you have some basic knowledge, that should be enough. I’m not a big expert either.







