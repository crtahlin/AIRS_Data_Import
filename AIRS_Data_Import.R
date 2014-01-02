### Import data from a CSV file exported from the AIRS android app

AIRSImport <- function( filePath, 
                        sensor
) {
  
  # read data from CSV file
  data <- readLines(con=filePath)
  
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
  colnames(AIRS_HP) <- c("Miliseconds","Sensor","Value")
  
  # add a date column to data
  date <- vector(length=dim(get(paste0("AIRS_", sensor)))[1], mode="numeric")
  names(date) <- "Date"
  date <- get(paste0("AIRS_", sensor))$Miliseconds + as.double(miliseconds)
  date <- as.POSIXlt(x=date/1000, tz="CET", origin="1970-01-01 00:00.00 CET")
  assign( x=paste0("AIRS_", sensor),
          value=cbind(get(paste0("AIRS_", sensor)), date))
}

# set path to CSV file exported form AIRS
filePath <- "C:/Users/Crt Ahlin/Documents/Dropbox/DataSets/AIRS/1385030889684_2.txt"

# set desired sensor
sensor <- "HP"



# extract the timestamp of data export from CSV file name
# assumptions: the origin is "1970-01-01 00:00.00 CET"
string <- regexpr(pattern="/?[[:digit:]]+.txt", text=filePath)
string <- string + 1
attr(string, "match.length") <- attr(string,"match.length") - 5
miliseconds <- regmatches(filePath, string)
seconds <- as.double(miliseconds)/1000
str(miliseconds)
fileTimestamp <- as.POSIXlt(x=seconds, tz="CET", origin="1970-01-01 00:00.00 CET")
fileTimestampMiliseconds <- miliseconds



# list valid entries for chosen sensor
data[validEntries]



# name the columns of the HP measurements
colnames(AIRS_HP) <- c("Miliseconds","Sensor","Value")




head(AIRS_HP)
str(AIRS_HP)

plot(AIRS_HP$Value ~ AIRS_HP$Miliseconds,
     type="l")

# TODO: 
Naredi export podatkov s telefona
dodaj, da odstrani # znak iz začetka vrstic
popravi import začetnega timestampa (nov format)
zbriši odvečen vrstice, pokomentiraj
