# Carlos Lima Santoro
# Coursera
# Data Science Specialization
# Exploratory Data Analysis
# Course Project 1
# Plot 3
# 
# File description:
# This script makes the required plot on the respective course project.
#
# Imput: Eletric power consumption dataset.
# Output: Plot 3.

library(dplyr)

# Functions Definitions
GetData <- function(file.name, file.connection){
    # This function checks if already is the data on working directory and read
    # it into R. If there isn't, the function downloads the data, save it on WD
    # and read it into R.
    #
    # Warning: The read function at the final step must be adapeted to each use
    # case.
    #
    # This function uses readr package.
    #
    # Input arguments:
    #   file.name - File name to be checked - type: string.
    #   file.connection - File connection to download the file - type: string.
    # Returns: A tibble.
    
    if(!grepl(pattern = "^\\.\\/", x = file.name)){
        file.name <- paste("./", file.name, sep = "", collapse = NULL)
    }
    if(!file.exists(file.name)){
        download.file(url = file.connection, destfile = file.name)
    }
    readr::read_delim(file.name,
                      delim = ";",
                      na = "?")
}

# Reading data
file.name <- "exdata_data_household_power_consumption.zip"
file.connection <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
epc <- GetData(file.name, file.connection)

# Manipulating data
epc$Date <- as.Date(epc$Date, "%d/%m/%Y")
epc <- filter(epc, Date >= as.Date("01/02/2007", "%d/%m/%Y") &
                  Date <= as.Date("02/02/2007", "%d/%m/%Y")) %>%
    mutate(Date.Time = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")) %>%
    select(-Date, -Time) %>%
    tidyr::gather(key = "sub.metering", value = "value", Sub_metering_1,
                  Sub_metering_2, Sub_metering_3)

Sys.setlocale("LC_TIME", "US")

# Plot Statements
png(filename = "plot3.png", type = "cairo-png")
with(epc, plot(Date.Time, value, type = "n", xlab = "",
               ylab = "Energy sub metering"))
with(filter(epc, sub.metering == "Sub_metering_1"),
     points(Date.Time, value, type = "l", col = "black"))
with(filter(epc, sub.metering == "Sub_metering_2"),
     points(Date.Time, value, type = "l", col = "red"))
with(filter(epc, sub.metering == "Sub_metering_3"),
     points(Date.Time, value, type = "l", col = "blue"))
     legend("topright", lty = 1, col = c("black", "red", "blue"),
            legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()


