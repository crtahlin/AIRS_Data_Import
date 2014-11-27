AIRS_HP <- AIRSImportExtended( filePath="C:/Users/Crt Ahlin/Documents/Google Drive/AIRS/1391608580610.txt",sensor="HP")
# library(ggplot2) 
AIRS_HP <- AIRSImportExtended( filePath="C:/Users/Crt Ahlin/Documents/Dropbox/DataSets/AIRS/1385030889684.txt",
                               sensor="HP")

AIRS_HP$Day <- format(AIRS_HP$Date, format="%d.%m.%Y")

dataFitbit <- read.csv("../QS2014/data/FitbitData.csv")
str(dataFitbit)
dataFitbit$date <- (as.Date(dataFitbit$date, format="%d.%m.%Y"))

## Import data ####
# AIRS data frame
load(file="C:/Users/Crt Ahlin/Documents/Google Drive/AIRS/R_AIRS_Imported.Rdata")
head(AIRSData)
# Fitbit data
dataFitbit <- read.csv("C:/Users/Crt Ahlin/Documents/Google Drive/AIRS/FitbitData - List1.csv")

## Plot ####
library(ggplot2)
plot <- ggplot(data=AIRSData[sample(1:(dim(AIRSData)[1]), size=10000),]) + theme_bw()  #+ facet_grid(Weekday ~ .)
plot <- plot + geom_point(aes(x=CyclicTime, y=Value), alpha=0.1) + scale_x_continuous(limits=c(0,1)) 
plot + scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1),labels=c("midnight","6 o'clock","noon","18 o'clock","midnight"))
print(plot)
plot + facet_grid(Weekday ~ .) 
plot + facet_grid(Day ~ .)
plot + geom_smooth(aes(x=CyclicTime, y=Value), size=2)
plot + stat_function(data=AIRSData, fun=cyclicCubicSplineTPBCurve, args=list(curve=TRUE, add=FALSE) )
plot + geom_text(data=dataFitbit, label="TEST", x=0, y=40)
plot + annotate("text", y=0, x=0, label="some text")
# todo izračunaj vektor vrednosti iz FitbitData in ga kar napiši spodaj pod graf
# ali se da to preko facetinga?
qplot( x=CyclicTime, y=Value, data=AIRS_HP  )
(as.POSIXlt(AIRS_HP$Date)$date)
xx <- as.POSIXct(AIRS_HP$Date, format="%d.%m.%Y")
xx
format(AIRS_HP$Date, format="%d.%m.%Y")

str(plot)
