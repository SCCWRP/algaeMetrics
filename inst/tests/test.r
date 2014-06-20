library(plyr)
library(stringr)

lapply(list.files("r", full.names=TRUE), source)

testdata <- read.csv("C:/Users/marke/Desktop/algae.test.2.csv")
testmeta <- read.csv("C:/Users/marke/Desktop/algaeMetaData3.csv")


result <- try(algae.IBIs(testdata, testmeta))


taxonInfo(testdata, testmeta)
