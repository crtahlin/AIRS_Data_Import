#' @title Import AIRS data exported as CSV from an Android phone
#' @description A function to import data for a certain sensor from and AIRS 
#' generated CSV file into R.
#' 
#' @details 
#' Currently implemented specificaly for "HP" aka "heart rate" sensor 
#' (i.e. Zephyr bluetooth). It might in principle work on other sensor data,
#'  although the columns will surely not get proper names.
#'  
#' @param filePath The path to the CSV file containing AIRS exported data.
#' Use forward slashes to separate folders (e.g. "/").
#' 
#' @param sensor The abbreviation representing the sensor. I.e "HP" for heart rate.
#' 
#' @export 
AIRSImport <- function( filePath, 
                        sensor) {
  # read data from CSV file
  data <- readLines(con=filePath)
  
  # read first line of AIRS expoted data for the initial date
  # temporarily set locale to "C" for date recognition
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  initialDate <- data[1]
  initialDate <- as.POSIXct(initialDate, format="%a %B %d %H:%M:%S %z %Y")
  Sys.setlocale("LC_TIME", lct)
  
  # find lines with patterns that match desired sensor 
  validEntries <- grep(pattern=sensor,x=data)
  
  # read data into dataframe
  system.time({
    temp <- gsub(x = data[validEntries], pattern = "#", replacement = "")
    assign( x=paste0("AIRS_", sensor),
            value=read.table(text=temp,
                             sep=";", 
                             comment.char="@" 
                             )
            )
    rm(temp)
    rm(data)
  })
  
  # name the columns of the HP measurements
  if (sensor=="HP") {
    colnames(AIRS_HP) <- c("Miliseconds","Sensor","Value")
  }
  
  # add a date column to data
  Date <- vector(length=dim(get(paste0("AIRS_", sensor)))[1], mode="numeric")
  Date <- get(paste0("AIRS_", sensor))$Miliseconds/1000 + initialDate
  assign( x=paste0("AIRS_", sensor),
          value=cbind(get(paste0("AIRS_", sensor)), Date)
          )
  rm(Date)
  return(get(paste0("AIRS_", sensor)))
}


########## Below are functions for debugging #############
# 
# # set path to CSV file exported form AIRS
# filePath <- "C:/Users/Crt Ahlin/Documents/Dropbox/DataSets/AIRS/1385030889684_2.txt"
# 
# # set desired sensor
# sensor <- "HP"
# 
# # extract the timestamp of data export from CSV file name
# # assumptions: the origin is "1970-01-01 00:00.00 CET"
# string <- regexpr(pattern="/?[[:digit:]]+.txt", text=filePath)
# string <- string + 1
# attr(string, "match.length") <- attr(string,"match.length") - 5
# miliseconds <- regmatches(filePath, string)
# seconds <- as.double(miliseconds)/1000
# str(miliseconds)
# fileTimestamp <- as.POSIXlt(x=seconds, tz="CET", origin="1970-01-01 00:00.00 CET")
# fileTimestampMiliseconds <- miliseconds
# 
# # list valid entries for chosen sensor
# data[validEntries]
# 
# # name the columns of the HP measurements
# colnames(AIRS_HP) <- c("Miliseconds","Sensor","Value")
# 
# head(AIRS_HP)
# str(AIRS_HP)
# 
# plot(AIRS_HP$Value ~ AIRS_HP$Miliseconds,
#      type="l")
# 

