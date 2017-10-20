library(ggplot2)
##opening the storm data file
stormdata <- read.csv("./ReproducibleResearch/StormData.csv.bz2")
##filtering out data from January 1996 onward to use in analysis (other previous data years were incomplete)
data <- transform(stormdata, date=as.Date(BGN_DATE, format="%m/%d/%Y"))
data2 <- subset(data, data$date >= "1996-01-01")
##Selecting the columns needed for analysis 
datacols <- subset(data2, select=c(date, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
##creating data set for Health related things (injuries and fatalities)
datahealth <- subset(datacols, !datacols$FATALITIES ==0 & !datacols$INJURIES ==0, select = c(EVTYPE, FATALITIES, INJURIES))
##creating data set for monetary costs (property damage and crop damage)
datacosts <- subset(datacols, !datacols$PROPDMG ==0 & !datacols$CROPDMG ==0, select = c(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
##creating data set for fatalities only
datahealth_deaths <- aggregate(datahealth$FATALITIES, by = list(datahealth$EVTYPE), FUN = sum)
##changing the column names
colnames(datahealth_deaths) <- c("TYPE", "FATALITIES")
##creating data set for injuries only
datahealth_injuries <- aggregate(datahealth$INJURIES, by = list(datahealth$EVTYPE), FUN = sum)
##changing column names
colnames(datahealth_injuries) <- c("TYPE", "INJURIES")
##Filtering so it shows the top 5 types of weather events for both deaths and injuries
datahealth_deaths <- datahealth_deaths[order(datahealth_deaths$FATALITIES, decreasing = TRUE),][1:5, ]
datahealth_injuries <- datahealth_injuries[order(datahealth_injuries$INJURIES, decreasing = TRUE),][1:5, ]

ggplot(datahealth_deaths, aes(TYPE, FATALITIES, fill = FATALITIES)) + geom_bar(stat = "identity") + labs(x = "Weather Event", y = "No. of Deaths") + ggtitle("Top 5 weather events causing deaths") + theme_update() + expand_limits(y=c(0,2500))
ggplot(datahealth_injuries, aes(TYPE, INJURIES, fill = INJURIES)) + geom_bar(stat = "identity") + labs(x = "Weather Event", y = "No. of Injuries") + ggtitle("Top 5 weather events causing injuries") + theme_update() + expand_limits(y=c(0,13000))


##creating a plot of top 5 weather events for deaths
##plot_deaths <- ggplot() + geom_bar(data = datahealth_deaths, aes(x = TYPE, 
##y = FATALITIES, fill = fatalities), stat = "identity", 
##show.legend = F) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
##xlab("Harmful Weather Events") + ylab("No. of Fatailities") + ggtitle("Top 5 weather events causing fatalities") + 
##theme(axis.text.x = element_text(angle = 45, hjust = 1))
##Creating a plot of top 5 weather events for injuries
##plot_injuries <- ggplot() + geom_bar(data = datahealth_injuries, aes(x = TYPE, y = INJURIES, 
##fill = injuries), stat = "identity", show.legend = F) + 
##theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Harmful Weather Events") + 
##ylab("No. of Injuries") + ggtitle("Top 5 weather events causing Injuries") + 
##theme(axis.text.x = element_text(angle = 45, hjust = 1))
##putting both plots in same view
##grid.arrange(plot_deaths, plot_injuries, ncol = 2)

##selecting required data for economic analysis

datacosts_prop <- subset(datacosts, datacosts$PROPDMGEXP=="K"| datacosts$PROPDMGEXP=="k"| datacosts$PROPDMGEXP=="M"| datacosts$PROPDMGEXP=="m"| datacosts$PROPDMGEXP=="B"| datacosts$PROPDMGEXP== "b")
datacosts_crop <- subset(datacosts, datacosts$CROPDMGEXP=="K"| datacosts$CROPDMGEXP=="k"| datacosts$CROPDMGEXP=="M"| datacosts$CROPDMGEXP=="m"| datacosts$CROPDMGEXP=="B"| datacosts$CROPDMGEXP== "b")

##converting values into numbers
datacosts_prop$PROPDMGEXP <- gsub("m", 1e+06, datacosts_prop$PROPDMGEXP, ignore.case = TRUE)
datacosts_prop$PROPDMGEXP <- gsub("k", 1000, datacosts_prop$PROPDMGEXP, ignore.case = TRUE)
datacosts_prop$PROPDMGEXP <- gsub("b", 1e+09, datacosts_prop$PROPDMGEXP, ignore.case = TRUE)
datacosts_prop$PROPDMGEXP <- as.numeric(datacosts_prop$PROPDMGEXP)

datacosts_crop$CROPDMGEXP <- gsub("m", 1e+06, datacosts_crop$CROPDMGEXP, ignore.case = TRUE)
datacosts_crop$CROPDMGEXP <- gsub("k", 1000, datacosts_crop$CROPDMGEXP, ignore.case = TRUE)
datacosts_crop$CROPDMGEXP <- gsub("b", 1e+09, datacosts_crop$CROPDMGEXP, ignore.case = TRUE)
datacosts_crop$CROPDMGEXP <- as.numeric(datacosts_crop$CROPDMGEXP)

##creating a total of the loss costs (crop + property)
datacosts$TOTALDMG <- (datacosts$CROPDMG * datacosts_crop$CROPDMGEXP) + 
  (datacosts$PROPDMG * datacosts_prop$PROPDMGEXP)

total_final <- aggregate(datacosts$TOTAL_DMG, by = list(datacosts$EVTYPE), FUN = sum)

colnames(total_final) <- c("TYPE", "TOTAL_DMG")

##ranking the top 5 events that cost the most in damage amounts
final_rank <- total_final[order(total_final$TOTAL_DMG, decreasing = TRUE), ][1:5, ]

##plotting final top 5 most costly types of weather events (crop + property damage)
ggplot(final_rank, aes(TYPE, TOTAL_DMG, fill = TOTAL_DMG)) + geom_bar(stat = "identity") + labs(x = "Weather Event", y = "Total cost") + ggtitle("Top 5 most costly weather events (crop + property damage)") + theme_update() + expand_limits(y=c(0,250000))

