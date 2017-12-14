## 8 February 2017 Sarun Luitel
## Another demonstration of random samples from known distributions
## plus random noise. The two distributions sampled in the program
## are:
##1. an F distribution
##2. a chi-squared distribution
##

library(e1071)
setwd("/home/sarun/Documents/R/")
source("DescriptiveStatisticsRev4.R")


#### Set parameters
##nobs = 30; nbins = 14 # short version
nobs = 500; nbins = 20 # long version
f_df1 = 2 ; f_df2 = 26 ; f_scale = 10 ; chi_df = 2 ; chi_scale = 10


#### Generate F-distribution random variablewith noise distributed U(-0.5,0.5)
noisy_f = f_scale * rf(nobs, f_df1, f_df2) +runif(nobs) - 0.5

#### Generate chi-squared-distributed randomvariable with noise distributed U(-0.5,0.5)
noisy_chi = chi_scale * rchisq(nobs, chi_df) + runif(nobs) - 0.5



#### Display descriptive statistics for both
disp_desc_stats("Noisy F", noisy_f)
disp_desc_stats("Noisy chi-squared", noisy_chi)


#### Display histograms for both split.screen(c(1,2))
screen(n = 1, new = TRUE)
hist(noisy_f, breaks = nbins, xlab = "Noisy F")
screen(n = 2, new = TRUE)
hist(noisy_chi, breaks = nbins, xlab = "Noisy chi-squared")
close.screen(all.screens = TRUE)

