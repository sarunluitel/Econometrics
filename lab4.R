## 1 march 2015 sarun luitel
## Demonstrate the central limit theorem for an ensemble
## of noisy distributions. The two distributions sampled in
## the program are:
## 1. a normal distribution
## 2. a t distribution
##
a
library(e1071)
setwd("C:\\Users\\sarun\\Documents\\R files\\R")
source("DescriptiveStatisticsRev4.R")


#### Set parameters
my_guess = 0.4956 # my guess at the mean of the normal ensemble
#nobs = 4 # short version
#nobs = 30 # medium version
nobs = 500 # long version
nsamples = 50 ; norm_scale = 10 ; t_df = 4 ; t_scale = 10


normal_ensemble_matrix <- {} # null matrix
t_ensemble_matrix <- {} # null matrix

for (sampleno in 1:nsamples)
{
  # column name for both ensembles
  col_name <- paste0("Sample ",sampleno)
  # Generate standard normal random variable with U(0,1) noise
  noisy_norm = norm_scale * rnorm(nobs) + runif(nobs)
  # concatenate a new column (cbind) on the descriptive
  #statatistics matrix for the noisy normal
  normal_ensemble_matrix <- cbind(
    normal_ensemble_matrix,
    desc_stats_mat(col_name,noisy_norm))
  # Generate t-distributed random variable with U(0,1) noise
  noisy_t = t_scale * rt(nobs, t_df) + runif(nobs)
  # concatenate a new column (cbind) on the descriptive
  #statatistics matrix for the noisy t
  t_ensemble_matrix <- cbind(
    t_ensemble_matrix,
    desc_stats_mat(col_name,noisy_t))
}

###
### function to display ensemble statistics
###
ensemble_statistics <- function(ensemble_name, ensemble_mat)
{
  stat_rows = c(5:8)
  stat_cols = c(2,3,5)
  cat(sep="", "\n", ensemble_name, "\n")
  cat(sep="","----------------------------------------------\n")
  # column headers for the ensemble statistics
  cat(sep="",
      sprintf("%8s |", " "),
      sprintf("%10s |", rownames(ensemble_mat)[stat_cols]),
      "\n")
  cat(sep="","----------------------------------------------\n")
  for (rowno in stat_rows)
  {
    # get the statistics for the required rows of the ensemble
    ensemble_row_stat <- desc_stats_mat(
      rownames(ensemble_mat)[rowno],
      ensemble_mat[rowno,])
    cat(sep="",
        sprintf("%8s |", rownames(ensemble_mat)[rowno]),
        sprintf("%10g |", ensemble_row_stat[stat_cols,1]),
        "\n")
  }
  cat(sep="","----------------------------------------------\n\n")
  sample_mean <- mean(ensemble_mat[5,])
  smin <- min(ensemble_mat[5,])
  smax <- max(ensemble_mat[5,])
  sample_stdev <- mean(ensemble_mat[6,]) / sqrt(nobs)
  z_crit <- -qnorm(0.025)
  cmin <- sample_mean - z_crit * sample_stdev
  cmax <- sample_mean + z_crit * sample_stdev
  z_value <- (my_guess - sample_mean) / sample_stdev
  p_guess <- 2*pnorm(-abs(z_value))
  cat(sep="", "Sampling std. deviation = ", sample_stdev,"\n")
  cat(sep="", "
Z-critical = ", z_crit, "\n")
  cat(sep="", " 95% sampling interval = ", cmin, " to ", cmax, "\n")
  cat(sep="", "
Sample interval = ", smin, " to ", smax, "\n\n")
  cat(sep="", "
My guess at mean = ", my_guess, "\n")
  cat(sep="", "
Z-value = ", z_value, "\n")
  cat(sep="", "Probability of my guess = ", p_guess, "\n\n")
}

ensemble_statistics("Noisy Normal", normal_ensemble_matrix)
ensemble_statistics("Noisy T", t_ensemble_matrix)

# regress skewness against mean
norm_fit <- lm(normal_ensemble_matrix[7,]~normal_ensemble_matrix[5,])
t_fit <- lm(t_ensemble_matrix[7,]~t_ensemble_matrix[5,])
# get the summary object from the regression
norm_summ <- summary(norm_fit)
t_summ <- summary(t_fit)
#png(filename="Lab4.png")
split.screen(c(1,2))
screen(n = 1, new = TRUE)
norm_title = paste0(
  "Noisy normal\nR-squared = ",
  sprintf("%7g", norm_summ$r.squared))
plot(normal_ensemble_matrix[5,], normal_ensemble_matrix[7,],
     xlab="Mean", ylab="Skewness", main=norm_title)
abline(norm_fit, col="red", lwd=5)
screen(n = 2, new = TRUE)
t_title = paste0(
  "Noisy t distribution\nR-squared = ",
  sprintf("%7g", t_summ$r.squared))
plot(t_ensemble_matrix[5,], t_ensemble_matrix[7,],
     xlab="Mean", ylab="Skewness", main=t_title)
abline(t_fit, col="red", lwd=5)
close.screen(all.screens = TRUE)
#dev.off()