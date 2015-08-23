#Q.5> How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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

#To extract numbers of coal related sources in dataset 'SCC'
ref.scc <- SCC[grep("Mobile - On-Road", SCC$EI.Sector), c("SCC", "EI.Sector", "SCC.Level.Four")]

tidydata <- merge(NEI, ref.scc, by = "SCC")

tidydata <- filter(tidydata, fips == "24510") %>% 
        group_by(year) %>% summarise(sum.emmit = sum(Emissions))

png("plot5.png")

barplot(tidydata$sum.emmit, names.arg = tidydata$year, 
        main = "PM2.5 Emissions by moto Vehicles in Baltimore City", 
        xlab = "Year", ylab = "PM2.5(tons)")

dev.off()
