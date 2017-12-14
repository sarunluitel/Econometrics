## 14 Jan 2015 Terry A. Student
## Descriptive Statistics
##

library(e1071)

source("DescriptiveStatisticsRev4.R")

#### Set parameters
nobs = 500000; nbins = 40000 # short version
norm_scale = 100 ; t_df = 206 ; t_scale = 10

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



