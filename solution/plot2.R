################################################################################
## Course: Exploratory Data Analysis - JHU, via Coursera (June 2014)
## Course Project 1
#
## The overall goal is simply to examine how household energy usage varies over
## a 2-day period in February, 2007. 
## We will only be using data from the dates 2007-02-01 and 2007-02-02.
#
## The task is to reconstruct the four plots shown on the project's page, all 
## of which were constructed using the base plotting system.
#
## For each plot, one should:
## 1) Construct the plot and save it to a PNG file with a width of 
##    480 pixels and a height of 480 pixels.
## 2) Name each of the plot files as plot1.png, plot2.png, etc.
## 3) Create a separate R code file (plot1.R, plot2.R, etc.) that constructs 
##    the corresponding plot, i.e. code in plot1.R constructs the plot1.png 
##    plot. 
##    The code file SHOULD INCLUDE CODE FOR READING THE DATA so that 
##    the plot can be fully reproduced. You should also include the code that 
##    creates the PNG file.
## 4) Add the PNG file and R code file to one's git repository (on GitHub)
#
## Notes:
##  - Dataset: "Electric power consumption" (UC Irvine Machine Learning 
##    Repository - [http://archive.ics.uci.edu/ml/] as of 2014-06-04)
#
##  - The dataset has 2,075,259 observations and 9 variables
##  - Missing values are coded as the following character: '?'
#
## GitHub repository forked from: [https://github.com/rdpeng/ExData_Plotting1]
#
## URL for this file (2014-JUNE-08): 
## [https://github.com/dpoet/ExData_Plotting1/blob/master/solution/plot2.R]
#
################################################################################

## Clears environnement
rm(list=ls())

## Required libraries
library(data.table) ## In order to use the fast fread() function.

LoadTargetData <- function(){
    ## Function LoadTargetData: This function loads the dataset and returns a 
    ## a subset of it, containing only observations from February 1st and 
    ## 2nd, 2007).
    
    ## Constants
    #  Name of the file containing the data.
    kMEASUREMENT_DATA <- "household_power_consumption.txt" # HPC
    
    # REGular EXpression used to search the two target dates
    kTARGET_DATES_REGEX <- "^[1|2]/2/2007$"
    
    ## Loads dataset with the fast function fread() from package "data.table";
    ## '?'  values should be converted to NA.    
    dtHPC <- fread(
        kMEASUREMENT_DATA
        , sep = ";"
        , na.strings = "?"
        , colClasses = "character"
    )
    
    ## Selects only the observations made on the target dates.
    ## Note the use of function grep() with the corresponding regular 
    ## expression used to search on the target variable (Date).
    dtFilteredHPC <- dtHPC[grep(kTARGET_DATES_REGEX, dtHPC$Date), ]
    
    ## Trying to free memory : discards the original HPC (Household Power 
    ## Consumption) dataset    
    rm(dtHPC)
    
    ## Creates variable datetime, based on columns "Date" and "Time"
    ## Note the `:=` operator below ('data.table' syntax for column creation)
    datetimeValues <- as.character(strptime(
        paste(dtFilteredHPC$Date, dtFilteredHPC$Time)
        , "%d/%m/%Y %H:%M:%S"
    )
    )
    dtFilteredHPC[ , datetime := datetimeValues]    
    
    ## Converts the data table to a data frame.
    ## Variable 'datetime' (column 10) becomes the first column.    
    result <- as.data.frame(dtFilteredHPC[ , c(10, 1:9), with = FALSE])
    
    ## Converts data types (classes)
    result$datetime <- as.POSIXlt (result$datetime, format="%Y-%m-%d %H:%M:%S")
    result$Date <- as.Date( result$Date, format = "%d/%m/%Y" )
    result[, 4:10] <- sapply(result[, 4:10], as.numeric)
    
    ## Returns a data frame containing the desired observations
    result 
}

## In the code below, we:
## 1) Set some parameters (constant values prefixed by a lowercase 'k') ;
## 2) Load the target data
## 3) Set the PNG file device and create the plot


## 1) Set some parameters (constants)
## Parameters (constants)
kFILE_NAME <- "plot2.png"
kPARAM_MARGIN <- c(2, 4, 2, 2)
kTARGET_VARIABLES <- c("datetime", "Global_active_power")

kYLABEL <- "Global Active Power (kilowatts)" 


## 2) Load target data
data <- LoadTargetData()
data <- data[ , kTARGET_VARIABLES]


## 3) Set the PNG file device and create the plot

## By default, png() sets width and heigth to 480 and units to pixels ("px").
## Calling png(filename = kFILE_NAME) is equivalent to 
## calling png(filename = kFILE_NAME, width=480, height=480, units="px").   
png(filename = kFILE_NAME)

## Adjusts the margins.
par(mar = kPARAM_MARGIN) 

## Creates plot.
par(cex.axis = 1.2, cex.lab = 1.2)
plot(x = data$datetime, y = data$Global_active_power, type = "l", ylab = kYLABEL)

## Closes the graphics device (plot is saved to file).
dev.off()