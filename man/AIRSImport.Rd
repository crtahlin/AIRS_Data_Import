\name{AIRSImport}
\alias{AIRSImport}
\title{Import AIRS data exported as CSV from an Android phone}
\usage{
AIRSImport(filePath, sensor)
}
\arguments{
  \item{filePath}{The path to the CSV file containing AIRS
  exported data. Use forward slashes to separate folders
  (e.g. "/").}

  \item{sensor}{The abbreviation representing the sensor.
  I.e "HP" for heart pulse (heart rate).}
}
\description{
A function to import data for a certain sensor from and
AIRS generated CSV file into R.
}
\details{
Currently implemented specificaly for "HP" aka "heart
pulse" sensor (i.e. Zephyr bluetooth). It might in
principle work on other sensor data, although the columns
will surely not get proper names.
}

