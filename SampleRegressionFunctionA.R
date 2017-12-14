## 18 March 2015 Terry A. Student
## Run a linear regression on a sample regression function.
##
# functions to format descriptive statistics

library(e1071)
setwd("C:\\Users\\sarun\\OneDrive\\Documents\\R files\\R")

source("DescriptiveStatisticsRev4.R")
# function to format regression results
source("RegReportLibraryRev5.R")

#### Set parameters
intercept = 5 ; slope = 2 ; noise = 0.2 # synthetic data parameters
nbins = 10 ; bannerID = 101722340>
  #set.seed(bannerID)
  
  
  synth_data_x <- c(0:20)
synth_data_y <- intercept + slope * synth_data_x + noise * runif(21)


# display descriptive statistics
disp_desc_stats("synth_data_x", synth_data_x)
disp_desc_stats("synth_data_y", synth_data_y)
# plot the histograms
png(filename="Lab5.png")
split.screen(c(1,2))
screen(n = 1, new = TRUE)
hist(synth_data_x, breaks = nbins)
screen(n = 2, new = TRUE)
hist(synth_data_y, breaks = nbins)
close.screen(all.screens = TRUE)
dev.off()

# single independent variable linear regression
myfit <- lm(synth_data_y~synth_data_x)
#display regression results
stataOutput.display(myfit)

plot(synth_data_x,synth_data_y)

