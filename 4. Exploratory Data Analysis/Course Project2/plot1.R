#Q.1> Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources
#for each of the years 1999, 2002, 2005, and 2008.

dir <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

#File download
if(!file.exists(paste(dir, "/data.zip", sep = ""))){
        download.file(url, destfile = "./data.zip", method = "curl")
        unzip("data.zip")
}

#Import datafile
if(!exists("NEI")){
        NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
        SCC <- readRDS("Source_Classification_Code.rds")
}

#Load Packages for manipulating of dataset
if(!require(tidyr)){install.packages("tidyr")}
if(!require(dplyr)){install.packages("dplyr")}

tidydata <- group_by(NEI, year) %>% summarise(sum.emmit = sum(Emissions))

png("plot1.png")
barplot(tidydata$sum.emmit, names.arg = tidydata$year, 
        main = "Total PM2.5 Emissions per Year", 
        xlab = "Year", ylab = "PM2.5(tons)")

dev.off()