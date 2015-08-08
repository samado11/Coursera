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

#Combine Date and Time
dataset$Datetime <- as.POSIXct(paste(dataset[ , 1], dataset[ , 2]))

#transform factor variable to numeric
dataset <- transform(dataset, Voltage = as.numeric(as.character(dataset$Voltage)), 
                     Global_reactive_power = as.numeric(as.character(dataset$Global_reactive_power)), 
                     Global_active_power = as.numeric(as.character(dataset$Global_active_power)), 
                     Sub_metering_1 = as.numeric(as.character(dataset$Sub_metering_1)), 
                     Sub_metering_2 = as.numeric(as.character(dataset$Sub_metering_2)))

#Plotting histogram of result
#The plot at topleft
par(mfrow = c(2, 2))
with(dataset, 
     plot(Global_active_power ~ Datetime, type ="l", 
          xlab = "", ylab = "Global Active Power", cex.lab = 0.7,
          cex.axis = 0.8)
)

#The plot at topright
with(dataset, 
     plot(Voltage ~ Datetime, type ="l", 
          xlab = "datetime", ylab = "Voltage", cex.lab = 0.7,
          cex.axis = 0.7)
)

#The plot at bottom left
cols <- c("black", "red", "blue")
with(dataset, 
     plot(Sub_metering_1 ~ Datetime, ylab = "Energy sub metering", type = "l", 
          ylim = c(0, 38), xlab = "", cex.lab = 0.7, cex.axis = 0.7))
par(new = TRUE)
with(dataset, 
     plot(Sub_metering_2 ~ Datetime, type = "l", col = cols[2], ylim = c(0, 38), 
          axes = F, xlab = "", ylab = "")
)
par(new = TRUE)
with(dataset, 
     plot(Sub_metering_3 ~ Datetime, type = "l", col = cols[3], ylim = c(0, 38), 
          axes = F, xlab = "", ylab = "")
)
legend("topright", col = cols, lwd = 1, legend = colnames(dataset[7:9]), 
       cex = 0.5, box.lwd = F)

#The plot at bottom right
with(dataset, 
     plot(Global_reactive_power ~ Datetime, type ="l", 
          xlab = "", ylab = "Global_reactive_power", cex.lab = 0.7,
          cex.axis = 0.7)
)

#Save the plot image.
dev.copy(png, "plot4.png", width = 480, height = 480)
dev.off()