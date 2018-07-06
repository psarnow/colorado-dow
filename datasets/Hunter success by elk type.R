#' ---
#' title: "Hunter Success by Elk Type (Bull, Cow/Calf)"
#' author: "Pierre Sarnow"
#' output:
#'   html_document:
#'     fig_width: 10
#'     df_print: paged
#' ---

#' ### Setup
setwd("~/_code/colorado-dow/datasets")
#' Load required libraries for wrangling data, charting, and mapping
#+ setup, message=F, warning=F
library(plyr,quietly = T)
library(dplyr,quietly = T)
library(ggplot2, quietly = T)
library(scales, quietly = T)
#' Set our preferred charting theme
theme_set(theme_minimal())

#' Run script to get hunter data
#+ source hunter data, message=F, warning=F
source('~/_code/colorado-dow/datasets/read colorado dow pdf.R', echo=F)
#' Table of the harvest data
COElkRifleAll

#' source the draw results so we know what type of elk the hunters are hunting
#' Run script to get draw data
#+ source draw data, message=F, warning=F
source('~/_code/colorado-dow/datasets/Elk Drawing Summaries.R', echo=F)
#' Table of the data
COElkDrawAll2

#' join together
COHunterbyElkType <- left_join(COElkRifleAll,COElkDrawAll2)

#' some cleanup
#' Draws are typically for only one sex, so we can use the total hunters to calculate how many hunters for the other sex
# Possible draw for 
## Bull
## Cow/Calf
## Either
## Neither ??, not sure if this is possible
COHunterbyElkType$Success.Bull <- COHunterbyElkType$Bulls / COHunterbyElkType$Bull.Chcs_Drawn
COHunterbyElkType$Success.CowCalf <- (COHunterbyElkType$Cows + COHunterbyElkType$Calves) / COHunterbyElkType$Bull.Chcs_Drawn

#' determine success per elk type
