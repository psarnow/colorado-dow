#' ---
#' title: "Hunter license draw success by unit"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' # Initial Questions to Explore
#' How difficult is it to get a license for each unit and season?
#' 
#' **NOTICE** I will initially ignore the differences between resident, nonresident and youth statuses.
#' I will also ignore the amount of preference points used when applying for a license in the draw.
#' 
#' ### Setup
setwd("~/_code/colorado-dow/Phase I - Descriptive Analytics")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T)
library(dplyr,quietly = T)
library(ggplot2, quietly = T)
library(scales, quietly = T)
#' Set our preferred charting theme
theme_set(theme_minimal())
#' Run script to get draw data
#+ source draw data, message=F, warning=F
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)
#' Table of the data
COElkDrawAll2

#' # Statewide Elk Hunter Success
#' ### By Year
#' First lets look at the entire state as a whole
# Remove Inf
COElkDraw <- filter(COElkDrawAll, is.finite(Draw_Success) & !is.na(Draw_Success))
# Cap success to 100%
COElkDraw$Draw_Success[COElkDraw$Draw_Success>1] <- 1.00
#' ### By Year
DrawSuccessStatewide <- summarise(group_by(COElkDraw,Year),
                                  Draw_Success = mean(Draw_Success,na.rm = T))
