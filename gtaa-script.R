library(quantmod)
# Basic R code for the Tactical Asset Allocation System by Mebane Faber
#
# For info on the system see here:
# http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461
#
# Copyright (c) Peter Werner 2012

#work out the log return between 2 periods
calc_ret <- function(s, idx, per) {
	front <- as.numeric(Cl(s[idx]))
	back <- as.numeric(Cl(s[(idx-per)]))	
	return(log(front/back))
}

#calculate the average return and if it is over its 200 SMA
sym_gtaa_data <- function(strsym, prev_month=FALSE) {	

	nm <- strsym #this symbol
	s <- get(strsym) #read from env (put there by getSymbols())
	m <- to.monthly(s) #aggregate to monthly
	idx <- nrow(m) #get the last row id

	if (prev_month) { #if we want to go back 1 month
		idx <- idx - 1
	}

	#calc 3 6 and 12 month returns
	ret3m <- calc_ret(m, idx, 3)
	ret6m <- calc_ret(m, idx, 6)
	ret12m <- calc_ret(m, idx, 12)

	#then calc their average
	avg_ret <- (ret3m + ret6m + ret12m) / 3

	#are we over the 10 month sma?
	m$SMA10 <- SMA(Cl(m), 10)
	gt_sma <- as.logical(Cl(m[idx]) > m[idx]$SMA10)
	close <- as.numeric(Cl(m[idx]))
	#return a list with appropriate names
	res <- list(Sym=nm, R3m=ret3m, R6m=ret6m, R12m=ret12m, Close=close, AvgRet=avg_ret, OverMA=gt_sma)
	return(res)
}

prev_month <- TRUE
#our list of symbols
syms <- c("VTI", "VEU", "IEF", "VNQ", "DBC")
#get the data from yahoo
getSymbols(syms)
#the data frame where we will store the info
df <- data.frame()
for (s in syms) {
	row <- sym_gtaa_data(s, prev_month)
	if (nrow(df) == 0) {
		df <- data.frame(row, stringsAsFactors=F)
	} else {
		df <- rbind(df, row)
	}
}
#display the data, ordered by the average return
df[with(df, order(AvgRet, decreasing=T)),]