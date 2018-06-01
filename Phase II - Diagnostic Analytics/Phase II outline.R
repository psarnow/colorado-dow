#' Data to integrate into analysis to help determine why we are getting our results
#'
#' Consider
# Draw Results
# Preference Point Requirements
# Herd Size or Population Estimates
# Weather


#' Drawing Summaries are provided for years 2005-2014, and 2017 has one that is
#' formatted entirely different
#' 
#' These are somewhat complex tables that upon a first glipse will take some effort
#' to organize appropriately so we can use its info.
#' 
#' Preference Points are only provided explicitly for years 2005-2014. This data 
#' might be included in the Drawing Summaries, but its hard to tell. I should also note
#' that my unit of interest (Unit 77) never requires preference points, so maybe this
#' can be added in later in support of multi unit analysis.
#' 
#' Population Estimates are provided for all years and seem pretty interesting.
#' 
#' It will be interesting that the Units for the herds span multiple hunt Units.
#' I also believe it will be valuable to see how harvest, effort, success, and #hunters
#' are associated with the herd size. These tables appear to be very easy to organize.
#' 
#' Weather
#' I've never attempted to find historical weather results, but I imagine they are available
#' from some open source.  
#' 
#' I think the trick will be attaching weather station/town data to
#' each Unit appropriately, will probably need to start analyzing units in their geographic spaces
#' to do this accurately. 
#' 
#' Another new part to our model that weather data will require are calendars. We will
#' need to acquire hunt season calendar dates as well.
#' 
#' I think weather will be an important factor in hunter success rates.
#' If we have this data, we can see if that is true.
#' 
#' Also note that 'weather' has multiple fields that may prove to be useful. Hi and Low temps,
#' precipitation
#' 
#' Start with importing Population Estimates, and then historical weather recordings