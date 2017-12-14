## 19 april 2015 sarunluitel
## Run a linear regression on a sample regression function.
##
# functions to format descriptive statistics

library(e1071)
setwd("C:\\Users\\sarun\\onedrive\\Documents\\R files\\R")

source("DescriptiveStatisticsRev4.R")
# function to format regression results
source("RegReportLibraryRev5.R")

##read data
mydata <- read.csv("Table9-3.csv", skip = 2)
nbins=20

disp_desc_stats("Demand", mydata$Final.Demand )
disp_desc_stats("Real GDP", mydata$Real.GDP)
disp_desc_stats("Real Energy Price", mydata$Real.Energy.Price)


hist(mydata$Final.Demand, breaks = nbins, xlab =  "Energy Demand", main="")

hist(mydata$Real.GDP, breaks = nbins, xlab="Real GDP", main="")

hist(mydata$Real.Energy.Price, breaks = 25, xlab="Real Energy price", main="")



ldemand <- log(mydata$Final.Demand)
lincome <- log(mydata$Real.GDP)
lprice <- log(mydata$Real.Energy.Price)

# myfit <- lm(ldemand~lincome+lprice)  log log regression
myfit <- lm(ldemand~lincome+lprice) ##linear regressions
stataOutput.display(myfit)