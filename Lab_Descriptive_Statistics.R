##25 Jan 2017 Sarun Luitel
## Descriptive Statistics.
##

library(e1071)
source("DescriptiveStatisticsRev4.R")
setwd("/home/sarun/Documents/R/")


mydata <- read.csv("Table 3_6.csv", na.strings = ".", skip = 6, nrows = 13)

disp_desc_stats(names(mydata)[2],mydata[2][!is.na(mydata[2])])
disp_desc_stats(names(mydata)[3],mydata[3][!is.na(mydata[3])])

nbins=14
png(filename="LabVar1.png")
split.screen(c(1,2))
screen(n= 1,new=TRUE)
hist(mydata[,2],breaks=nbins)
screen(n=2,new=TRUE)
hist(mydata[,3],breaks=nbins)
close.screen(all.screens=TRUE)
dev.off()