#Q.6> Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

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

#To extract numbers of coal related sources in dataset 'SCC'
ref.scc <- SCC[grep("Mobile - On-Road", SCC$EI.Sector), c("SCC", "EI.Sector", "SCC.Level.Four")]

tidydata <- merge(NEI, ref.scc, by = "SCC")

tidydata <- filter(tidydata, fips == "24510" | fips == "06037") %>% 
        group_by(year, fips) %>% summarise(sum.emmit = sum(Emissions))

tidydata$fips <- gsub("24510", "Baltimore City", tidydata$fips)
tidydata$fips <- gsub("06037", "L.A", tidydata$fips)
colnames(tidydata) <- c("year", "County", "sum.emmit")
        
png("plot6.png")
g <- ggplot(data = tidydata, aes(x = year, y = sum.emmit, colour = County))
print(g + geom_line(lwd = 2) + geom_point(lwd = 4) + labs(x = "Year", y = "PM2.5(tons)", 
                       title = "PM2.5 Emissions by moto Vehicles in Baltimore City & L.A") +
        scale_x_continuous(breaks= seq(1999, 2008, by = 3)))

dev.off()
