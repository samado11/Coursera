#Q.4> Across the United States, how have emissions from coal combustion-related sources
#changed from 1999–2008?

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

#To extract numbers of coal related sources in dataset 'SCC'
ref.scc <- SCC[grep("Coal", SCC$EI.Sector), c("SCC", "EI.Sector")]


tidydata <- merge(NEI, ref.scc, by = "SCC")
tidydata <- group_by(tidydata, year) %>% summarise(sum.emmit = sum(Emissions))

png("plot4.png")

barplot(tidydata$sum.emmit, names.arg = tidydata$year, 
        main = "PM2.5 Emissions by Coal combustion", 
        xlab = "Year", ylab = "PM2.5(tons)")

dev.off()