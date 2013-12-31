### Import data from a CSV file exported from the AIRS android app

# set path to CSV file exported form AIRS
filePath <- "C:/Users/Crt Ahlin/Documents/Dropbox/DataSets/AIRS/1385030889684.txt"

# set desired sensor
sensor <- "HP"

# read data from CSV file
data <- readLines(con=filePath)

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

# find lines with patterns that match desired sensor 
validEntries <- grep(pattern=sensor,x=data)

# list valid entries for chosen sensor
data[validEntries]

# read data from into dataframe
system.time({
assign(x=paste0("AIRS_", sensor), 
       value=read.table(text=data[validEntries],sep=";", comment.char="@", sep="#"))

})

# name the columns of the HP measurements
colnames(AIRS_HP) <- c("Miliseconds","Sensor","Value")


# add a date column to AIRS_HP
date <- vector(length=dim(AIRS_HP)[1])
names(date) <- "Date"
date <- AIRS_HP$Miliseconds + as.double(miliseconds)
date <- as.POSIXlt(x=date/1000, tz="CET", origin="1970-01-01 00:00.00 CET")
AIRS_HP <- cbind(AIRS_HP, date)

head(AIRS_HP)
str(AIRS_HP)

plot(AIRS_HP$Value ~ AIRS_HP$Miliseconds,
     type="l")
