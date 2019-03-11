# Carlos Lima Santoro
# Coursera
# Data Science Specialization
# Exploratory Data Analysis
# Course Project 1
# Plot 1
# 
# File description:
# This script makes the required plot on the respective course project.
#
# Imput: Eletric power consumption dataset.
# Output: Plot 1.

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
                Date <= as.Date("02/02/2007", "%d/%m/%Y"))

# Plot Statements
png(filename = "plot1.png",
    type = "cairo")
hist(epc$Global_active_power,
     col = "red",
     xlab = "Global Active Power (kilowatts)",
     main = "Global Active Power")
dev.off()
