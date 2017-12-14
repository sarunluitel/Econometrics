library(e1071)
setwd("C:\\Users\\sarun\\OneDrive\\Documents\\R files\\R")

source("DescriptiveStatisticsRev4.R")
# function to format regression results
source("RegReportLibraryRev5.R")



# read the data file
mydata <- read.csv("BWGHT.csv", skip = 0, nrows = 1390, na.strings = "-")

#### Set parameters

nbins = 20


# display descriptive statistics
disp_desc_stats("Birth Weight", mydata$bwght)
disp_desc_stats("Cigs", mydata$cigs)
disp_desc_stats("Family income", mydata$faminc)
disp_desc_stats("Mother's Education", mydata$motheduc)
disp_desc_stats("Father's Education", mydata$fatheduc)
sum(mydata$cigs == 0)
range(mydata$bwght[mydata$cigs == 0])



# # 
#  #### Display histograms for both
#  split.screen(c(1,2))
#  screen(n = 1, new = TRUE)
#  hist(mydata$bwght, breaks = nbins, xlab =  "Birth Weight (ounces)", main="")
#  screen(n = 2, new = TRUE)
#  hist( mydata$cigs, breaks = nbins, xlab = "cigarettes smoked per day during pregnancy", ylim = c(0,100), main="")
#  text(4, 97.5, pos = 4, "-- category 0 truncated - actual value 1176")
#  close.screen(all.screens = TRUE)

# 
#### Display histograms for both
split.screen(c(1,2))
screen(n = 1, new = TRUE)
hist(mydata$fatheduc, breaks = nbins, xlab =  "Father's year of schooling", main="")
screen(n = 2, new = TRUE)
hist(mydata$motheduc, breaks = nbins, xlab =  "Mother's year of schooling", main="")
close.screen(all.screens = TRUE)


# simple linear regression
myfit <- lm(mydata$bwght~mydata$cigs+mydata$faminc +mydata$motheduc+mydata$fatheduc)
stataOutput.display(myfit)

mylm <- lm(mydata$bwght~mydata$cigs+mydata$faminc +mydata$motheduc+mydata$fatheduc)
myresid <- resid(mylm)
plot(myresid)

disp_desc_stats("residual",myresid)

##hist(myresid, breaks = nbins, xlab = "residual", main="")
plot(mydata$cigs,mydata$bwght)
cor(mydata[,c(4,10,1,6,5)], use = "complete.obs")