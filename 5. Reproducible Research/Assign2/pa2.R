# Preparing the data set
dir <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!file.exists("data.csv")){
        download.file(url, destfile = "data.csv", method = "curl")
}

if(!exists("raw")){
        raw <- read.csv("data.csv", stringsAsFactors = F)
}

if(!require(dplyr)){install.packages("dplyr")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(ggplot2)){install.packages("ggplo2")}

# Trimming of Data for Question 1.
q1.tbl <- raw %>% group_by(EVTYPE) %>% summarise(INJURIES = sum(INJURIES), FATALITIES = sum(FATALITIES), 
                                                      Total.harm = sum(INJURIES, FATALITIES)) %>% 
        arrange(desc(Total.harm)) %>% mutate(Rank = row_number()) %>% head(20)
names(q1.tbl)[1] <- "Event"

q1.plot.data <- q1.tbl %>% select(Event, INJURIES, FATALITIES) %>% melt(id = "Event")
names(q1.plot.data)[1:2] <- c("Event", "Type")

# Reordering of axis of x by decreasing of value of y for plotting
q1.plot.data$Event <- factor(q1.plot.data$Event, 
                             levels = q1.plot.data$Event[order(q1.tbl$Rank)])

# Making Plot for Exploratory Data Analysis
p1 <- ggplot(q1.plot.data, aes(x = Event, y = value, fill = Type))
p1 + geom_bar(stat = "identity") + theme_bw() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("What events are dangerous for people?") +
        xlab("Events(Top 20)") + ylab("Number of Damages of people")

# My Result of Q1
cat(paste("Portion of ", q1.tbl$Event[1], " in Events : ", 
          round(q1.tbl$Total.harm[1] / sum(raw$INJURIES, raw$FATALITIES) * 100, digits = 2), "%", sep = ""))

# Trimming of Data for Question 2.
q2.tbl <- raw %>% select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% 
        mutate(PROPDMGEXP = toupper(PROPDMGEXP), CROPDMGEXP = toupper(CROPDMGEXP))
names(q2.tbl)[1] <- "Event"

q2.tbl[q2.tbl$PROPDMGEXP == "H", "PROPDMGEXP"] <- 100
q2.tbl[q2.tbl$PROPDMGEXP == "K", "PROPDMGEXP"] <- 1000
q2.tbl[q2.tbl$PROPDMGEXP == "M", "PROPDMGEXP"] <- 1000000
q2.tbl[q2.tbl$PROPDMGEXP == "B", "PROPDMGEXP"] <- 1000000000
q2.tbl[q2.tbl$PROPDMGEXP == "+" | q2.tbl$PROPDMGEXP == "-", "PROPDMGEXP"] <- 1
q2.tbl[q2.tbl$PROPDMGEXP == "?" | q2.tbl$PROPDMGEXP == "", "PROPDMGEXP"] <- 0

q2.tbl[q2.tbl$CROPDMGEXP == "K", "CROPDMGEXP"] <- 1000
q2.tbl[q2.tbl$CROPDMGEXP == "M", "CROPDMGEXP"] <- 1000000
q2.tbl[q2.tbl$CROPDMGEXP == "B", "CROPDMGEXP"] <- 1000000000
q2.tbl[q2.tbl$CROPDMGEXP == "?" | q2.tbl$CROPDMGEXP == "", "CROPDMGEXP"] <- 0

q2.tbl <- q2.tbl %>% mutate(PROPDMGEXP = as.numeric(PROPDMGEXP), CROPDMGEXP = as.numeric(CROPDMGEXP)) %>% 
        mutate(PROP.damage = PROPDMG * PROPDMGEXP, CROP.damage = CROPDMG * CROPDMGEXP) %>% 
        group_by(Event) %>% summarise(PROP.damage = sum(PROP.damage), CROP.damage = sum(CROP.damage), 
                                      Total.Val = sum(PROP.damage, CROP.damage))
Total.damage <- sum(q2.tbl$Total.Val)
q2.tbl <- q2.tbl %>% arrange(desc(Total.Val)) %>% mutate(Rank = row_number()) %>% head(20)

q2.plot.data <- q2.tbl %>% select(Event, PROP.damage, CROP.damage) %>% melt(id = "Event")
names(q2.plot.data)[2] <- "Type"

# Reordering of axis of x by decreasing of value of y for plotting
q2.plot.data$Event <- factor(q2.plot.data$Event, 
                             levels = q2.plot.data$Event[order(q2.tbl$Rank)])

# Making Plot for Exploratory Data Analysis
p2 <- ggplot(q2.plot.data, aes(x = Event, y = value, fill = Type))
p2 + geom_bar(stat = "identity") + theme_bw() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("What events caused economic damages?") +
        xlab("Events(Top 20)") + ylab("Amounts of economic damages")

# My Result of Q2
cat(paste("Portion of ", q2.tbl$Event[1], " in Events : ", round(q2.tbl$Total.Val[1] / Total.damage * 100, 
                                                digits = 2), "%", sep = ""))

