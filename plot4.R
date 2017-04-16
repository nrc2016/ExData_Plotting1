##############################################################################
#
# Filename: plot4.R
#
# This file contains the functions for creating the fourth plot for the week 1
# project of the Exploratory Data Analysis course. After loading the
# create_plot_4 can be used as the main function.
#
# Functions:
#   - create_plot_4
#   - check_required_features
#   - load_data
#
# Date: April 16, 2017
#
##############################################################################

##############################################################################
#
# Global definitions
#
##############################################################################
data.dir <- "data"
dataset.filename <- paste(data.dir, "household_power_consumption.txt", sep="/")
start.datetime.string = "2007-02-01"
end.datetime.string = "2007-02-03"
start.datetime <- as.POSIXct(start.datetime.string, format="%Y-%m-%d")
end.datetime <- as.POSIXct(end.datetime.string, format="%Y-%m-%d")
plot.filename <- "plot4.png"
dataset.cache.name <- "household.power.comsumption"
required.features <- c("datetime",
                       "globalactivepower",
                       "submetering1",
                       "submetering2",
                       "submetering3",
                       "voltage",
                       "globalreactivepower")

##############################################################################
#
# Function: create_plot_4
#
# This function creates the fourth plot for the week 1 project in the Exploratory
# Data Analysis course. This function loads in the data set and opens a graphics
# device for saving the plot. The next step is the plot is created and the
# the graphics device is closed.
#
# Args:
#
# Returns:
#
##############################################################################

create_plot_4 <- function () {
  print("... Start creating plot 4 ...")
  
  # load data
  df <- load_data(dataset.filename)
  if (class(df) != "data.frame") {
    print(df)
    return(FALSE)
  }
  
  # open graphics device
  print(paste("...... Opening PNG file", plot.filename, "..."))
  png(plot.filename, width=480, height=480)
  
  # change the format to accomodate a 2 by 2 plot layout
  par(mfrow=c(2,2))
  print(paste("......... End opening PNG file."))
  
  # create plot
  print("...... Creating multi plot ...")
  
  # create first plot
  print("...... Creating Global Active Power plot ...")
  plot(x=df$datetime, y=df$globalactivepower,
       type="l",
       xlab="",
       ylab="Global Active Power (kilowatts)")
  print("......... End creating Global Active Power plot.")
  
  # create second plot
  print("...... Creating Global Active Power plot ...")
  plot(x=df$datetime, y=df$voltage,
       type="l",
       xlab="datetime",
       ylab="Voltage")
  print("......... End creating Global Active Power plot.")
  
  # create third plot
  plot(x=df$datetime, y=df$submetering1,
       type="l",
       xlab="",
       ylab="Energy sub metering")
  lines(x=df$datetime, y=df$submetering2, col="red")
  lines(x=df$datetime, y=df$submetering3, col="blue")
  legend("topright",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col=c("black", "red", "blue"),
         lwd=1)
  
  # create fourth plot
  print("...... Creating Global Active Power plot ...")
  plot(x=df$datetime, y=df$globalreactivepower,
       type="l",
       xlab="datetime",
       ylab="Global_reactive_power")
  print("......... End creating Global Active Power plot.")
  
  print("......... End creating multi plot.")
  
  # close graphics device
  print("...... Closing graphics device ...")
  dev.off()
  print("......... End closing graphics device.")
  
  print("...... End create plot 4.")
}

##############################################################################
#
# Function: check_required_features
#
# This function checks if all the required features are present in the data
# set.
#
# Args:
# - data set
#
# Returns:
# - TRUE: all the requirements are satisfied
# - FALSE: some of the requirements are missing
#
##############################################################################

check_required_features <- function (df) {
  result <- required.features %in% colnames(df)
  
  if (length(which(result==FALSE)) > 0) {
    return(FALSE)
  }
  
  return(TRUE)
}

##############################################################################
#
# Function: load_data
#
# This function returns the data set for plotting. This is done by first
# checking the R programming environment for the data set, If one exists with
# the required features it is returned. If not the data set is read from the
# file and processed in the following way:
# - concatenating the Date and Time features into one Date object
# - filter to include the time between 2007-02-01 and 2007-02-02
# - convert feature names to lower case
# - remove special characters from the feature names
# The formatted data set is saved to the R global environment and returned.
# Caching is implemented to save time in loading the data set.
#
# Args:
# - filename: name of the data set file
#
# Returns:
# - data set
#
##############################################################################

load_data <- function (filename) {
  print("...... Loading data ...")
  # check if data set exists
  if (!file.exists(filename)) {
    return(paste("ERROR: The file ", filename, " does not exist."))
  }
  
  #check if data set was previously cached with required features
  if (exists(dataset.cache.name)) {
    df <- get(dataset.cache.name)
    if (check_required_features(df)) {
      print("......... Found cached data set ...")
      print("......... End loading data.")
      return(df)
    }
  }
  
  # load data set and change missing values to NA
  df <- read.csv(filename, sep=";" , na.strings="?")
  
  # convert date/time
  df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S")
  
  # filter only dates between 2007-02-01 and 2007-02-02
  df <- df[df$DateTime > start.datetime & df$DateTime < end.datetime,]
  
  # format the feature names
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub("_", "", colnames(df), fixed=TRUE)
  
  # save to global environment
  assign(dataset.cache.name, df, envir=.GlobalEnv)
  
  print("......... End loading data.")
  return(df)
}

##############################################################################
#
# Mainline
#
##############################################################################

create_plot_4()