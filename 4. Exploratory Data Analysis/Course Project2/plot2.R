#Q.2> Have total emissions from PM2.5 decreased in the Baltimore City,
#Maryland (fips == "24510") from 1999 to 2008?
#Use the base plotting system to make a plot answering this question.

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

tidydata <- filter(NEI, fips == "24510") %>% 
        group_by(year) %>% summarise(sum.emmit = sum(Emissions))

png("plot2.png")
barplot(tidydata$sum.emmit, names.arg = tidydata$year, 
        main = "Total PM2.5 Emissions of Baltimore City, Maryland", 
        xlab = "Year", ylab = "PM2.5(tons)")
dev.off()
