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
mydata <- read.csv("WAGE1.csv", na.strings="-")

nbins=30

disp_desc_stats("Wage", mydata$female)
disp_desc_stats("Education", mydata$educ)
disp_desc_stats("Real Energy Price", mydata$married)

hist(mydata$wage, breaks = nbins, xlab="Wage", main="")
hist(mydata$educ, breaks = nbins, xlab="Education",main="")


##histograms
# plot the histograms
#png(filename="Lab5.png")
split.screen(c(1,2))
screen(n = 1, new = TRUE)
hist(mydata$wage, breaks = nbins)
screen(n = 2, new = TRUE)
hist(mydata$educ, breaks = nbins)


# myfit <- lm(ldemand~lincome+lprice)  log log regression
myfit <- lm(wage~educ+female, data=mydata) ##linear regressions
stataOutput.display(myfit)

#Interective variable
femedu= mydata$female*mydata$educ
myfit <- lm(wage~educ+female+femedu, data=mydata) ##linear regressions
stataOutput.display(myfit)


# plot the histograms
#png(filename="Lab5.png")
close.screen(all.screens = TRUE)
dev.off()
hist(femedu, breaks = nbins)

cor(mydata[,c(1,2,6)], use = "complete.obs")



