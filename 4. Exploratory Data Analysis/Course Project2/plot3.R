#Q.3> Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
#variable, which of these four sources have seen decreases in emissions 
#from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

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
if(!require(ggplot2)){install.packages("ggplot2")}

tidydata <- filter(NEI, fips == "24510") %>% 
        group_by(year, type) %>% summarise(sum.emmit = sum(Emissions))

png("plot3.png")
print(qplot(year, sum.emmit, data = tidydata, geom = c("point", "line"), colour = type, 
      xlab = "Year", ylab = "PM2.5(tons)", 
      main = "PM2.5 Emissions per types of Baltimore City, Maryland") +
      scale_x_continuous(breaks= seq(1999, 2008, by = 3)))

dev.off()
