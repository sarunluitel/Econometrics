## 1 Feb 2017 Sarun Luitel
## A demonstration of random samples from known distributions
## plus random noise. The two distributions sampled in the program
## are:
## 1. a standard normal distribution
## 2. a t distribution
##

library(e1071)
setwd("/home/sarun/Documents/R/")
source("DescriptiveStatisticsRev4.R")


#### Set parameters
nobs = 30; nbins = 14 # short version
norm_scale = 10 ; t_df = 26 ; t_scale = 10

#### Generate normal random variable with noise distributed U(-0.5,0.5)
noisy_norm = norm_scale * rnorm(nobs) + runif(nobs) - 0.5

#### Generate t-distributed random variable with noise distributed U(-0.5,0.5)
noisy_t = t_scale * rt(nobs, t_df) + runif(nobs) - 0.5

#### Display descriptive statistics for both
disp_desc_stats("Noisy normal", noisy_norm)
disp_desc_stats("Noisy T", noisy_t)

#### Display histograms for both
split.screen(c(1,2))
screen(n = 1, new = TRUE)
hist(noisy_norm, breaks = nbins, xlab = "Noisy normal")
screen(n = 2, new = TRUE)
hist(noisy_t, breaks = nbins, xlab = "Noisy T")
close.screen(all.screens = TRUE)

