# 
# Utilities to compute and display descriptive statistics
#
#    Rev. 4 10 Jan 2015 D.S.Dixon - Better handling of missing values
#    Rev. 3  1 Oct 2014 D.S.Dixon - Handle missing values
#    Rev. 2 15 Sep 2014 D.S.Dixon - Separate matrix and display functions
#    Rev. 1 24 Aug 2014 D.D.Dixon - Separated from reading data
#    Rev. 0 17 Aug 2014 D.S.Dixon
#

library(e1071)

#############################################################
#
# Build a matrix with the descriptive statastics for a 
#  variable. (Handy if you need any of these statistics
#  in subsequent calculations.)
#
desc_stats_mat <- function(var_name, thisvar)
{
	desc_stats_mat <- matrix(0, 8, 1)
	desc_stats_mat[1,1] <- sum(!is.na(thisvar)) # N
	desc_stats_mat[2,1] <- min(thisvar, na.rm = TRUE)                   # min
	desc_stats_mat[3,1] <- max(thisvar, na.rm = TRUE)                   # max
	desc_stats_mat[4,1] <- median(thisvar, na.rm = TRUE)                # median
	desc_stats_mat[5,1] <- mean(thisvar, na.rm = TRUE)                  # mean
	desc_stats_mat[6,1] <- sd(thisvar, na.rm = TRUE)                    # standard deviation
	desc_stats_mat[7,1] <- skewness(thisvar, na.rm = TRUE)              # skewness
	desc_stats_mat[8,1] <- kurtosis(thisvar, na.rm = TRUE)              # kurtosis
	colnames(desc_stats_mat) <- var_name
	rownames(desc_stats_mat) <- c(
		"N",
		"min",
		"max",
		"median",
		"mean",
		"std.dev.",
		"skewness",
		"kurtosis")
	desc_stats_mat
}

#############################################################
#
# Display descriptive statistics for a variable
#
disp_desc_stats <- function(var_name, thisvar)
{
  stat_mat <- desc_stats_mat(var_name, thisvar)
  
  cat(sep="","\n", colnames(stat_mat)[1], "\n")
  cat(sep="","==================\n")
  for (i in 1:dim(stat_mat)[1])
  {
    cat(sep="",sprintf("%8s = %g",rownames(stat_mat)[i], stat_mat[i,1]), "\n")
  }
}

