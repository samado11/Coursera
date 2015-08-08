dir <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

if(file.exists(paste(dir, "/data.zip", sep = "")) == FALSE){
        download.file(url, destfile = "./data.zip", method = "curl")
        unzip("data.zip")
}

#import file to data.frame
raw <- read.table("household_power_consumption.txt", sep = ";", header = T)

#subsettting the imported file to the small dataset that course porject' instruction has directed
raw$Date <- as.Date(raw$Date, format = "%d/%m/%Y")
dataset <- subset(raw, Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02"))

#transform factor variable to numeric
dataset <- transform(dataset, Global_active_power = as.numeric(as.character(dataset$Global_active_power)))

#Combine Date and Time
dataset$Datetime <- as.POSIXct(paste(dataset[ , 1], dataset[ , 2]))

#Plotting result
with(dataset, 
     plot(Global_active_power ~ Datetime, type ="l", 
        xlab = "", ylab = "Global Active Power(killowatts)", cex.lab = 0.7,
        cex.axis = 0.8)
)

#Save the plot image.
dev.copy(png, "plot2.png", width = 480, height = 480)
dev.off()
