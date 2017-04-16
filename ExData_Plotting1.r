##############################################################################
#
# Filename: ExData_Plotting1.r
#
# This file defines the functionality for creating the four plots required for
# the week 1 project in the Exploratory Data Analysis course. The program can
# executed using the mainline() function.
#
# Functions:
#   - mainline
#
# Date: April 16, 2017
#
##############################################################################

##############################################################################
#
# Function: mainline
#
# This functions executes the four plotting scripts so they create the four
# plots required by the week 1 project.
#
# Args:
#
# Returns:
#
##############################################################################

mainline <- function() {
  print("Starting mainline ...")

  # create first plot
  source("plot1.R")
  
  # create second plot
  source("plot2.R")
  
  # create third plot
  source("plot3.R")

  # create fourth plot
  source("plot4.R")
  
  print("... End mainline")
}

mainline()
