# RegReportLibrary.R
#
# Utilities to format R regression results to look like STATA regression results.
#
# Rev 5. 14 Dec 2014 D.S.Dixon - Handles results with and without interaction variables
# Rev 4. 21 Nov 2014 D.S.Dixon - Better handling of interaction variables
# Rev 3. 11 Nov 2014 D.S.Dixon - Better handling of multivariate regression
# Rev 2. 12 Oct 2014 D.S.Dixon - Error trapping for empty results object
# Rev 1. 15 Sep 2014 D.S.Dixon - Generalized output for ECON 309
# Rev 0.  5 Oct 2012 D.S.Dixon - Blog output for EC 460 
#

#############################################
#
#  Check the regression object for a) nullness and
#    b) null F-statistic from bogus model
#
checklmobject <- function(myfit){
	if ( is.null(myfit) ) { stop('lm object is null')}
	mysumm <- summary(myfit)
	if ( is.null(mysumm$fstatistic) ) { stop('lm object has null results')}
	mysumm
}	

#############################################
#
#  Return a Stata-like ANOVA matrix
#
stataOutput.anovaMat <- function(myfit){

	if ( is.null(myfit) ) { stop('lm object is null')}
	mysumm <- summary(myfit)
	if ( is.null(mysumm$fstatistic) ) { stop('lm object has null results')}
	myaov <- summary(aov(myfit))

  nvars <- dim(myaov[[1]])[1]
  dfmodel <- nvars - 1
	dfresid <- myaov[[1]]["Residuals","Df"]
	dftotal <- dfmodel + dfresid

	SST <- sum(myaov[[1]][,"Sum Sq"])
	SSR <- myaov[[1]]["Residuals","Sum Sq"]
	SSE <- SST - SSR

	anovaMat <- matrix(nrow=3,ncol=3)
	anovaMat[1,1] <- SSE
	anovaMat[1,2] <- dfmodel
	anovaMat[1,3] <- SSE / dfmodel

	anovaMat[2,1] <- SSR
	anovaMat[2,2] <- dfresid
	anovaMat[2,3] <- SSR / dfresid

	anovaMat[3,1] <- SST
	anovaMat[3,2] <- dftotal
	anovaMat[3,3] <- SST / dftotal

	rownames(anovaMat) <- c("Model", "Residual", "Total")
	colnames(anovaMat) <- c("SS", "df", "MS")

	anovaMat
}

#############################################
#
#  Return a Stata-like goodness-of-fit matrix
#
stataOutput.fitMat <- function(myfit){

	if ( is.null(myfit) ) { stop('lm object is null')}
	mysumm <- summary(myfit)
	if ( is.null(mysumm$fstatistic) ) { stop('lm object has null results')}
	myaov <- summary(aov(myfit))
  
	SSR <- myaov[[1]]["Residuals","Sum Sq"]
	dfresid <- myaov[[1]]["Residuals","Df"]
	
	N <- length(myfit$residuals)

	fitMat <- matrix(nrow=6,ncol=1)
	fitMat[1,1] <- N
	fitMat[2,1] <- mysumm$fstatistic[1]
	fitMat[3,1] <- myaov[[1]][1,"Pr(>F)"]
	fitMat[4,1] <- mysumm$r.squared
	fitMat[5,1] <- mysumm$adj.r.squared
	fitMat[6,1] <- sqrt(SSR / dfresid)
  
	colnames(fitMat) <- " "
	rownames(fitMat) <- c(" "," "," "," "," "," ")
	rownames(fitMat)[1] <- "Number of obs = "
	rownames(fitMat)[2] <- sprintf("F(%3.0f,%6.0f) = ", mysumm$fstatistic[2], mysumm$fstatistic[3])
	rownames(fitMat)[3] <- "Prob > F      = "
	rownames(fitMat)[4] <- "R-squared     = "
	rownames(fitMat)[5] <- "Adj R-squared = "
	rownames(fitMat)[6] <- "Root MSE      = "

	fitMat
}

#############################################
#
#  Return a Stata-like estimator matrix
#
stataOutput.estimatorMat <- function(myfit){

	if ( is.null(myfit) ) { stop('lm object is null')}
	mysumm <- summary(myfit)
	if ( is.null(mysumm$fstatistic) ) { stop('lm object has null results')}
	myaov <- summary(aov(myfit))
	dfresid <- myaov[[1]]["Residual","Df"]

	# the number of parameters in the model
	# k <- length(colnames(myfit$model))
	k <- dim(mysumm$coefficients)[1]
	printf <- function(...) invisible(cat(sprintf(...)))

	# the range for a 95% interval
	Trange <- c(-qt(0.975, df=dfresid),qt(0.975, df=dfresid))

	estimatorMat <- matrix(nrow = k, ncol=6)
	colnames(estimatorMat) <- c("Coef.","Std. Err.","t","P>|t|","[95% Conf.","Interval]")

	# cat("\n",colnames(myfit$model[1]),"\n")

	# shift the constant term to the end
	rownames(estimatorMat) <- c(names(myfit$coeff)[2 : k],"_cons")
	# loop over the slope coefficients
	for(i in 2 : k){
	  estimatorMat[i-1, 1:4] <- mysumm$coefficients[i,1:4]
	  estimatorMat[i-1, 5:6] <- mysumm$coefficients[i,1] + mysumm$coefficients[i,2]*Trange
	}
	# put the constant at the end
	estimatorMat[k, 1:4] <- mysumm$coefficients[1,1:4]
	estimatorMat[k, 5:6] <- mysumm$coefficients[1,1] + mysumm$coefficients[1,2]*Trange
	
	estimatorMat
}

#############################################
#
#  Display Stata-like regression results
#
stataOutput.display <- function(myfit){
	if ( is.null(myfit) ) { stop('lm object is null')}
	mysumm <- summary(myfit)
	if ( is.null(mysumm$fstatistic) ) { stop('lm object has null results')}
	anovaMat <- stataOutput.anovaMat(myfit)
  fitMat <- stataOutput.fitMat(myfit)
  estimatorMat <- stataOutput.estimatorMat(myfit);

  maxVarWidth = max(nchar(rownames(estimatorMat)))
  varBaseWidth = nchar(rownames(estimatorMat)[1]) - nchar(getVarName(rownames(estimatorMat)[1]))
  
  leftNameWidth = max(11, maxVarWidth - varBaseWidth) # what's wider, 'Residual' or a variable name?
  cat(paste0("leftNameWidth = ", leftNameWidth, "\n"))
  spacerWidth = 22 - leftNameWidth

  leftNameFmt <- paste0(" %", leftNameWidth,"s |  ")
  spacerFmt <- paste0("%", spacerWidth,"s")
  
  leftNameBar <- paste0(rep("-", leftNameWidth + 2), collapse="")
  anovaBar <- paste0(rep("-", 30), collapse="")
  estimatorBar <- paste0(rep("-", leftNameWidth + 67), collapse="")
  estimatorMidBar <- paste0(rep("-", 64), collapse="")
  
  printf <- function(...) invisible(cat(sprintf(...)))

  fitFmts <- c("%7.0f\n","%7.2f\n","%7.4f\n","%7.4f\n","%7.4f\n","%7.2f\n")
  anovaRow = 0
  for (i in 1:6){
    if (i == 1) {
      printf(leftNameFmt, "Source")
      printf("%10s  %4s  %10s", "SS", "df", "MS")
    } else if (i %in% c(2,5)) {
      printf("%s+%s", leftNameBar, anovaBar)
    } else { # i %in% c(3,4,6)
      anovaRow = anovaRow + 1
      printf(leftNameFmt, rownames(anovaMat)[anovaRow])
      printf(formatVar(anovaMat[anovaRow,1], 10))
      printf(" %4.0f  %10.3f", anovaMat[anovaRow,2], anovaMat[anovaRow,3])
    }
    printf(spacerFmt, " ")
    printf("%16s", rownames(fitMat)[i], fitMat[i,1])
    printf(fitFmts[i], fitMat[i,1])
  }
  printf("\n")
  printf("%s\n", estimatorBar)
  printf(leftNameFmt, getVarName(colnames(myfit$model[1])))
  printf("    %5s   %9s      %1s    %5s     %10s%10s\n", 
         colnames(estimatorMat)[1], colnames(estimatorMat)[2], colnames(estimatorMat)[3], 
         colnames(estimatorMat)[4], colnames(estimatorMat)[5], colnames(estimatorMat)[6])  
  printf("%s+%s\n", leftNameBar, estimatorMidBar)
	for (i in 1:nrow(estimatorMat)){
    printf(leftNameFmt, getVarName(rownames(estimatorMat)[i]))
    printf(formatVar(estimatorMat[i,1], 9))
    printf(formatVar(estimatorMat[i,2], 9))
    printf("  %7.2f %7.3f  ", estimatorMat[i,3], estimatorMat[i,4])
    printf(formatVar(estimatorMat[i,5], 9))
    printf("%3s", " ")
    printf(formatVar(estimatorMat[i,6], 9))
    printf("%1s", "\n")
  }
  printf("%s\n", estimatorBar)
}

#########################################
# 
# Return the format string for a variable
#
#
formatVar <- function(datavar, width = 10){
  logvalue = log10(abs(datavar))
  magnitude = ceiling(logvalue)
  decimals = max(0, min(width - 2, width - magnitude - 2))
  formatNumber <- paste0("%", width, ".", decimals,"f")
  formatStr <- paste0("%", width + 1, "s")
  formatVar <- sprintf(formatStr,sprintf(formatNumber,datavar))
}

#########################################
# 
# Return the variable name from the 
# dataset name (e.g. 'mydata$variable' is
# returned as 'variable')
#
getVarName <- function(fullname){
  index <- regexpr("$", fullname, fixed=TRUE)
  retstring <- fullname
  if (index > -1){ 
    retstring <- substr(fullname, index + 1, nchar(fullname))    
  }
  retstring
}

#########################################
# 
#  Print out so it can cut and paste into wordpress or markdown  
# 
wordpressFormat <- function(.fit){
	if ( is.null(.fit) ) { stop('lm object is null')}
	.summ <- summary(.fit)
	if ( is.null(.summ$fstatistic) ) { stop('lm object has null results')}
	.vnames <- rownames(attr(terms(.fit), "factors"))
  .coeffs <- .summ$coefficients
  .nparms <- length(.vnames)
  .rsq <- .summ$r.squared
  .arsq <- .summ$adj.r.squared
  .nobs <- length(.summ$residuals)
  
  # latex header
  cat("\\begin{array}{", paste(rep("l",.nparms + 1), collapse=""),"}\n",sep="")
  # the regressed coefficients
  cat("\\widehat{",.vnames[1],"} = & ", .coeffs[1,1],sep="")
  for (i in 2:.nparms){
          if(.coeffs[i,1] > 0) cat(" & +",sep="") else cat(" & ",sep="") 
          cat(.coeffs[i,1], "\\ ", .vnames[i],sep="")
  }
  cat("\\\\\n")
  # the regressed standard errors
  for (i in 1:length(.vnames)){
      cat("& (",.coeffs[i,2], ") ",sep="")
  }    
  cat("\\\\\n")
  # the r=squared line
  cat("\\multicolumn{", (.nparms + 1), "}{l}{n = ",.nobs,", R^{2} = ",.rsq,", Adjusted\ R^{2} = ",.arsq,"}",sep="")
  cat("\\\\\n\\end{array}\n")
}

dexyWordpressFormat <- function(.fit){
	if ( is.null(.fit) ) { stop('lm object is null')}
	.summ <- summary(.fit)
	if ( is.null(.summ$fstatistic) ) { stop('lm object has null results')}
	.vnames <- rownames(attr(terms(.fit), "factors"))
  .coeffs <- .summ$coefficients
  .nparms <- length(.vnames)
  .rsq <- .summ$r.squared
  .arsq <- .summ$adj.r.squared
  .nobs <- length(.summ$residuals)
  
  # latex header
  cat("\\begin{array}{", paste(rep("l",.nparms + 1), collapse=""),"}\n",sep="")
  # the regressed coefficients
  cat("\\widehat{",.vnames[1],"} = & ", .coeffs[1,1],sep="")
  for (i in 2:.nparms){
          if(.coeffs[i,1] > 0) cat(" & +",sep="") else cat(" & ",sep="") 
          cat(.coeffs[i,1], "\\ ", .vnames[i],sep="")
  }
  cat("\\\\\\\\\n")
  # the regressed standard errors
  for (i in 1:length(.vnames)){
      cat("& (",.coeffs[i,2], ") ",sep="")
  }    
  cat("\\\\\\\\\n")
  # the r=squared line
  cat("\\multicolumn{", (.nparms + 1), "}{l}{n = ",.nobs,", R^{2} = ",.rsq,", Adjusted\ R^{2} = ",.arsq,"}",sep="")
  cat("\\\\\\\\\n\\end{array}\n")
}

#############################################
#
#  Format regression results like STATA would
#
stataOutput.html <- function(myfit){
  
	if ( is.null(myfit) ) { stop('lm object is null')}
	mysumm <- summary(myfit)
	if ( is.null(mysumm$fstatistic) ) { stop('lm object has null results')}
	
  #
  # set up a bunch of html constants for ease of use and maintenance
  #
  margin <- "        "
  tdstyle <- "border: none; padding-top:0; padding-bottom:0;"
  tdtightstyle <- paste(tdstyle, "padding-left:0; padding-right:0;")
  tdcharstyle <- paste(tdtightstyle, "font-family:courier,monospace;")
  tdtextstyle <- paste(tdstyle, "font-family:courier,monospace; padding-left:2px; padding-right:2px;")
  
  tdrightstyle <- paste(tdtextstyle,"text-align:right;")
  tdleftstyle <- paste(tdtextstyle,"text-align:left;")
  
  tabletag <- "      <table style='border:none;'>\n"
  hrtag <- "    <hr style='border:dashed;border-width:1px 0 0 0;'/>\n"
  
  tdright <- paste("  <td style='", tdrightstyle,"'>\n")
  tdleft <- paste("  <td style='", tdleftstyle,"'>\n")
  tdrightspan2 <- paste("  <td colspan='2' style='", tdrightstyle,"'>\n")
  
  tdtight <- paste("  <td style='", tdtightstyle, "'")
  tdchar <- paste("  <td style='", tdcharstyle, "'>\n")
  
  firstrow <- paste(tabletag, margin, "<tr>\n", margin, tdright)
  rowdivider <- paste(margin, "  </td>\n", margin, "</tr>\n", margin, "<tr>\n", margin, tdright)
  lastrow <- paste(margin,"  </td>\n", margin, "</tr>\n      </table>\n")
  columndivider <- paste(margin, "  </td>\n", margin, tdright)
  twocolumndivider <- paste(margin, "  </td>\n", margin, tdrightspan2)
  stiledivider <- paste(margin, "  </td>", tdchar, "|</td>\n", margin, tdright)
  
  fitfirstrow <- paste(tabletag, margin, "<tr>\n", margin, tdleft)
  fitrowdivider <- paste(margin,"  </td>\n",margin,"</tr>\n",margin,"<tr>\n",margin, tdleft)
  
  dashrow5 <- paste(margin, "  </td>\n", margin, "</tr>\n", margin, "<tr>\n", margin, tdtight, " colspan='5'>\n", margin, hrtag)
  dashrow8 <- paste(margin, "  </td>\n", margin, "</tr>\n", margin, "<tr>\n", margin, tdtight, " colspan='8'>\n", margin, hrtag)
  
  dashfirstrow8 <- paste(tabletag, margin, "<tr>\n", margin, tdtight, " colspan='8'>\n", margin, hrtag)
  lastrow <- paste(margin,"  </td>\n", margin, "</tr>\n      </table>\n")
  
  # start the grand table
  cat("<table style='border:none;width:800px;'>\n  <tr>\n    <td style='border:none;'>\n")
  
  #
  # Format the ANOVA table
  #
  
  dfmodel <- mysumm$df[3] - 1
  dfresid <- mysumm$df[2]
  dftotal <- dfmodel + dfresid
  
  # recover the dependent variable
  yvals = myfit$fitted.values + myfit$residuals
  
  SSR <- deviance(myfit)
  SST <- sum((yvals - mean(yvals))^2)
  SSE <- SST - SSR
  
  # Output the ANOVA table
  cat(firstrow)
  cat(margin,"    Source\n")
  cat(stiledivider)
  cat(margin,"    SS\n")
  cat(columndivider)
  cat(margin,"    df\n")
  cat(columndivider)
  cat(margin,"    MS\n")
  
  cat(dashrow5)
  cat(rowdivider)
  
  cat(margin,"    Model\n")
  cat(stiledivider)
  cat(margin,"    ",SSE,"\n")
  cat(columndivider)
  cat(margin,"    ",dfmodel,"\n")
  cat(columndivider)
  cat(margin,"    ",(SSE / dfmodel),"\n")
  
  cat(rowdivider)
  
  cat(margin,"    Residual\n")
  cat(stiledivider)
  cat(margin,"    ",SSR,"\n")
  cat(columndivider)
  cat(margin,"    ",dfresid,"\n")
  cat(columndivider)
  cat(margin,"    ",(SSR / dfresid),"\n")
  
  cat(dashrow5)
  cat(rowdivider)
  
  cat(margin,"    Total\n")
  cat(stiledivider)
  cat(margin,"    ",SST,"\n")
  cat(columndivider)
  cat(margin,"    ",dftotal,"\n")
  cat(columndivider)
  cat(margin,"    ",(SST / dftotal),"\n")
  cat(lastrow)
  
  # more grand table
  cat("    </td>\n")
  cat("    <td style='border:none;' align='right'>\n")
  
  #
  # Format the Goodness-of-Fit table
  #
  
  N <- length(myfit$residuals)
  
  cat(fitfirstrow)
  cat(margin,"Number of obs\n")
  cat(columndivider)
  cat(margin,"=\n")
  cat(columndivider)
  cat(margin, N, "\n")
  cat(fitrowdivider)
  cat(margin,"F(", mysumm$fstatistic[2], ",", mysumm$fstatistic[3], ")\n")
  cat(columndivider)
  cat(margin,"=\n")
  cat(columndivider)
  cat(margin, mysumm$fstatistic[1], "\n")
  cat(fitrowdivider)
  cat(margin,"Prob > F\n")
  cat(columndivider)
  cat(margin,"=\n")
  cat(columndivider)
  cat(margin, df(mysumm$fstatistic[1], mysumm$fstatistic[2], mysumm$fstatistic[3]), "\n")
  cat(fitrowdivider)
  cat(margin,"R-squared\n")
  cat(columndivider)
  cat(margin,"=\n")
  cat(columndivider)
  cat(margin, mysumm$r.squared, "\n")
  cat(fitrowdivider)
  cat(margin,"Adj R-squared\n")
  cat(columndivider)
  cat(margin,"=\n")
  cat(columndivider)
  cat(margin, mysumm$adj.r.squared, "\n")
  cat(fitrowdivider)
  cat(margin,"Root MSE\n")
  cat(columndivider)
  cat(margin,"=\n")
  cat(columndivider)
  cat(margin, sqrt(SSR / dfresid), "\n")
  cat(lastrow)
  
  # more grand table
  cat("    </td>\n")
  cat("  </tr>\n")
  cat("  <tr>\n")
  cat("    <td colspan='2' style='border:none; padding-top:20px;'>\n")
  
  #
  # Format the Estimators table
  #
  # the number of parameters in the model
  k <- length(colnames(myfit$model))
  
  # the range for a 95% interval
  Trange <- qt(0.975, df=dfresid)
  
  # column headers
  cat(dashfirstrow8)
  cat(rowdivider)
  cat(margin,colnames(myfit$model[1]),"\n")
  cat(stiledivider)
  cat(margin,"Coef.\n")
  cat(columndivider)
  cat(margin,"Std. Err.\n")
  cat(columndivider)
  cat(margin,"t\n")
  cat(columndivider)
  cat(margin,"P>|t|\n")
  cat(twocolumndivider)
  cat(margin,"[95% Conf. Interval]\n")
  cat(dashrow8)
  cat(rowdivider)
  
  # loop over the slope coefficients
  for(i in 2:k){
    cat(margin, colnames(myfit$model)[i],"\n")
    cat(stiledivider)
    cat(margin, mysumm$coefficients[i,1],"\n")
    cat(columndivider)
    cat(margin, mysumm$coefficients[i,2],"\n")
    cat(columndivider)
    cat(margin, mysumm$coefficients[i,3],"\n")
    cat(columndivider)
    cat(margin, mysumm$coefficients[i,4],"\n")
    cat(columndivider)
    cat(margin, mysumm$coefficients[i,1] - mysumm$coefficients[i,2]*Trange,"\n")
    cat(columndivider)
    cat(margin, mysumm$coefficients[i,1] + mysumm$coefficients[i,2]*Trange,"\n")
    cat(rowdivider)
  }
  # put the constant at the end
  cat(margin, "_cons\n")
  cat(stiledivider)
  cat(margin, mysumm$coefficients[1,1],"\n")
  cat(columndivider)
  cat(margin, mysumm$coefficients[1,2],"\n")
  cat(columndivider)
  cat(margin, mysumm$coefficients[1,3],"\n")
  cat(columndivider)
  cat(margin, mysumm$coefficients[1,4],"\n")
  cat(columndivider)
  cat(margin, mysumm$coefficients[1,1] - mysumm$coefficients[1,2]*Trange,"\n")
  cat(columndivider)
  cat(margin, mysumm$coefficients[1,1] + mysumm$coefficients[1,2]*Trange,"\n")
  cat(dashrow8)
  cat(lastrow)
  
  # finsh off the grand table
  cat("    </td>\n")
  cat("  </tr>\n")
  cat("</table>\n")
}

#############################################
#
#  Format regression results like STATA would
#   and write the html to a file
#
stataOutput.htmlFile <- function(myfit, outputfile){
	if ( is.null(myfit) ) { stop('lm object is null')}
	mysumm <- summary(myfit)
	if ( is.null(mysumm$fstatistic) ) { stop('lm object has null results')}
	sink(outputfile)
  stataOutput.html(myfit)
  sink()
}
