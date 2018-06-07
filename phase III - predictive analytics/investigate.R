
# Investigate Unit 77, Season 1
Season1_77 <- filter(COElkHuntingData, Unit == "77" & Season == 1)

ggplot(Season1_77, aes(Year,Success)) +
  geom_bar(stat = "identity") +
  prettytheme +
  ggtitle("")
# 2010 is very low, 2013 is very high

ggplot(Season1_77, aes(Year,daily.temperatureHigh)) +
  geom_bar(stat = "identity") +
  prettytheme +
  ggtitle("")
# 2013 has a very low daily high temp

ggplot(Season1_77, aes(Year,daily.humidity)) +
  geom_bar(stat = "identity") +
  prettytheme +
  ggtitle("")
# 2013 has a very high daily humidity

ggplot(Season1_77, aes(Year,daily.precipProbability)) +
  geom_bar(stat = "identity") +
  prettytheme +
  ggtitle("")
# 2013 has a very high daily precipProbability

ggplot(Season1_77, aes(Year,daily.precipType)) +
  geom_bar(stat = "identity") +
  prettytheme +
  ggtitle("")
# 2013 has a very high daily precipType

ggplot(Season1_77, aes(Year,daily.precipAccumulation)) +
  geom_bar(stat = "identity") +
  prettytheme +
  ggtitle("")
# 2013 has a very high daily precipAccumulation

ggplot(Season1_77, aes(Year,Elks_Hunter)) +
  geom_bar(stat = "identity") +
  prettytheme +
  ggtitle("")
# 2010 has a very low number of Elk per Hunter

ggplot(Season1_77, aes(Year,Unit_Pop)) +
  geom_bar(stat = "identity") +
  prettytheme +
  coord_cartesian(ylim = c(3000,4000)) +
  ggtitle("")
# 2010 has the lowest herd size

#' So in 2010, there were not as many elk, and there were many hunters.
#' Additionally the weather was unfavorable
#' 
#' The most successfull year (2013), however, did not have the largest elk population to hunt.
#' 2013 did have the most 'weather' of all of the years though. The weather seems to
#' indicate likelihood of success in that respect
#' 
Season1_77B <- select(Season1_77,Days, Success, Unit_Pop:daily.FullmoonPhase, Elks_Hunter,Year)
Season1_77_Scaled <- as.data.frame(scale(select(Season1_77B,-Year)))
Season1_77_Scaled$Year <- Season1_77$Year
Season1_77B <- gather(Season1_77B, "Metric",value, -Year)
Season1_77_Scaled <- gather(Season1_77_Scaled, "Metric",value, -Year,-Success)

ggplot(Season1_77_Scaled, aes(Year,value,group=Metric,color=Metric)) +
  geom_point() +
  geom_line() +
  prettytheme +
  # coord_cartesian(ylim = c(3000,4000)) +
  ggtitle("")

ggplot(Season1_77_Scaled, aes(Metric,value,label=Year,size=Success,color=Success)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter() +
  prettytheme +
  ggtitle("")
#' Fields that don't seem associated with Success
#' cloudCover, FullmoonPhase, Pressure, Days, ElkperHunter, Unit Population
#' Fields that might have an association with Success
#' Humidity, MoonPhase, PrecipAccumulation, PrecipType, PrecipProbability,TempHigh,TempLow,windSpeed

