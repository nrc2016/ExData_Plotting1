# global variables
data.dir <- "data"
dataset.filename <- paste(data.dir, "household_power_consumption.txt", sep="/")
start.datetime.string = "2007-02-01"
end.datetime.string = "2007-02-03"
start.datetime <- as.POSIXct(start.datetime.string, format="%Y-%m-%d")
end.datetime <- as.POSIXct(end.datetime.string, format="%Y-%m-%d")
plot.filename <- "plot3.png"
dataset.cache.name <- "household.power.comsumption"
required.features <- c("datetime",
                       "globalactivepower",
                       "submetering1",
                       "submetering2",
                       "submetering3",
                       "voltage",
                       "globalreactivepower")

check.required.features <- function (df) {
  result <- required.features %in% colnames(df)
  
  if (length(which(result==FALSE)) > 0) {
    return(FALSE)
  }
  
  return(TRUE)
}

load_data <- function (filename) {
  print("...... Loading data ...")
  # check if data set exists
  if (!file.exists(filename)) {
    return(paste("ERROR: The file ", filename, " does not exist."))
  }
  
  #check if data set was previously cached with required features
  if (exists(dataset.cache.name)) {
    df <- get(dataset.cache.name)
    if (check.required.features(df)) {
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

create_plot_3 <- function () {
  print("... Start creating plot 3 ...")
  
  # load data
  df <- load_data(dataset.filename)
  if (class(df) != "data.frame") {
    print(df)
    return(FALSE)
  }
  
  # open graphics device
  print(paste("...... Opening PNG file", plot.filename, "..."))
  png(plot.filename, width=480, height=480)
  print(paste("......... End opening PNG file."))
  
  # create plot
  print("...... Creating Sub Metering plot ...")
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
  print("......... End creating Sub Metering plot.")
  
  # close graphics device
  print("...... Closing graphics device ...")
  dev.off()
  print("......... End closing graphics device.")
  
  print("...... End create plot 3.")
}

create_plot_3()