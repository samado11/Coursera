dir <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

#file download
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

#Plotting histogram of result
hist(dataset$Global_active_power, col = "red", main = "Global Active Power", 
     xlab = "Global Active Power(killowatts)", cex.lab = 0.8, cex.main = 1, cex.axis = 0.7)

#Save the plot image.
dev.copy(png, "plot1.png", width = 480, height = 480)
dev.off()
