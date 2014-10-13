
## Exploratory Data Analysis  - Project 1 - Plot FOUR (4)
##----------------------------------------
# Plotting Power Consumption
# Sanjay Verma
# October 12th, 2014

# INTRO
# This is the first project in the Exploratory Data Analysis course offered by Johns Hopkins through Coursera’s Data Science specialization track. 
# The objective is to create plots by reproducing a four plots, and placing them in github by using forking and cloning of an existing repository.

# SYNOPSIS
# The goal here is simply to examine how household energy usage varied over the first two (2)-day period in February of 2007, and
# reproducing a series of four plots. The se plots look marginally different to the original plots becase of the specific image size requirements.

# DATA
# This project used data from the UC Irvine Machine Learning Repository, a popular repository for machine learning datasets. The “Individual household electric power consumption Data Set” was used and made available on the course web site:
#                                           •                                          Electric power consumption Dataset (20Mb)
# •	Description: Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years. Different electrical quantities and some sub-metering values are available.
# The following descriptions of the 9 variables in the dataset are taken from the UCI web site
# 1.	Date: Date in format dd/mm/yyyy
# 2.	Time: time in format hh:mm:ss
# 3.	Global_active_power: household global minute-averaged active power (in kilowatt)
# 4.	Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
# 5.	Voltage: minute-averaged voltage (in volt)
# 6.	Global_intensity: household global minute-averaged current intensity (in ampere)
# 7.	Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
# 8.	Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# 9.	Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
# Data Loading and Preprocessing
if(!file.exists("exdata-data-household_power_consumption.zip")) {
                                          temp <- tempfile()
                                          download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
                                          file <- unzip(temp)
                                          unlink(temp)
}
power <- read.table(file, header=T, sep=";")
power$Date <- as.Date(power$Date, format="%d/%m/%Y")
df <- power[(power$Date=="2007-02-01") | (power$Date=="2007-02-02"),]
df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
df$Global_reactive_power <- as.numeric(as.character(df$Global_reactive_power))
df$Voltage <- as.numeric(as.character(df$Voltage))
df <- transform(df, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))

# PLOT 1
plot1 <- function() {
                                          hist(df$Global_active_power, main = paste("Global Active Power"), col="red", xlab="Global Active Power (kilowatts)")
                                          dev.copy(png, file="plot1.png", width=480, height=480)
                                          dev.off()
                                          cat("Plot1.png has been saved in", getwd())
}
plot1()

## Plot1.png has been saved in C:/Users/Sunjay007/Desktop/Exploratory_Data_Analysis/Projects/Plot1.png

# PLOT  2
Plot2 <- function() {
                                          plot(df$timestamp,df$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
                                          dev.copy(png, file="plot2.png", width=480, height=480)
                                          dev.off()
                                          cat("plot2.png has been saved in", getwd())
}
Plot2()

## plot2.png has been saved in C:/Users/Sunjay007/Desktop/Exploratory_Data_Analysis/Projects/Plot2.png


# PLOT  3
plot3 <- function() {
                                          plot(df$timestamp,df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
                                          lines(df$timestamp,df$Sub_metering_2,col="red")
                                          lines(df$timestamp,df$Sub_metering_3,col="blue")
                                          legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), lwd=c(1,1))
                                          dev.copy(png, file="plot3.png", width=480, height=480)
                                          dev.off()
                                          cat("plot3.png has been saved in", getwd())
}
plot3()

## plot3.png has been saved in C:/Users/Sunjay007/Desktop/Exploratory_Data_Analysis/Projects/Plot3.png

# PLOT 4
plot4 <- function() {
                                          par(mfrow=c(2,2))
                                          
                                          ##PLOT 1
                                          plot(df$timestamp,df$Global_active_power, type="l", xlab="", ylab="Global Active Power")
                                          ##PLOT 2
                                          plot(df$timestamp,df$Voltage, type="l", xlab="datetime", ylab="Voltage")
                                          
                                          ##PLOT 3
                                          plot(df$timestamp,df$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
                                          lines(df$timestamp,df$Sub_metering_2,col="red")
                                          lines(df$timestamp,df$Sub_metering_3,col="blue")
                                          legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex=.5) #bty removes the box, cex shrinks the text, spacing added after labels so it renders correctly
                                          
                                          #PLOT 4
                                          plot(df$timestamp,df$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
                                          
                                          #OUTPUT
                                          dev.copy(png, file="plot4.png", width=480, height=480)
                                          dev.off()
                                          cat("plot4.png has been saved in", getwd())
}
plot4()

## plot4.png has been saved in C:/Users/Sunjay007/Desktop/Exploratory_Data_Analysis/Projects/Plot4.png
