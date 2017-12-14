## 8 May 2016 sarunluitel
## Run a linear regression on a sample regression function.
##
# functions to format descriptive statistics

library(e1071)
setwd("C:\\Users\\sarun\\onedrive\\Documents\\R files\\R")

source("DescriptiveStatisticsRev4.R")
# function to format regression results
source("RegReportLibraryRev5.R")

##read data
mydata <- read.csv("sleep75.csv")
nbins=2

desc_stats_mat("total work",mydata$sleep)
desc_stats_mat("total work",mydata$yngkid)



hist(mydata$sleep, breaks = nbins, xlab =  "Sleep", main="")

hist(mydata$totwrk, breaks = nbins, xlab="totalwork", main="")

hist(mydata$educ, breaks = nbins, xlab="Education (Years)", main="")
hist(mydata$yngkid, breaks = nbins, xlab="Age", main="")


myfit <- lm(sleep~totwrk+educ+age+agesq+yngkid, data=mydata) ##linear regressions
stataOutput.display(myfit)