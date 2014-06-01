###
## using historical volatility to improve risk adjusted returns
##
## Pete Werner, 2013
###

library(quantmod)
library(PerformanceAnalytics)

build.rets <- function(x, k=1)
{
	r <- ROC(x,k)
	return(as.vector(r))
}

#realized vol
#passed a series of log rets
rv <- function(x, dpy=252)
{
	n <- length(x)
	#assume mean ret is 0
	meanret <- 0
	r <- sum((x - meanret)^2, na.rm=TRUE)/(n - 1)
	r <- r * dpy
	return(100 * sqrt(r))
}

prop.dist <- function(x)
{
	tgt <- x[length(x)]
	minval <- min(x)
	maxval <- max(x)
	rg <- max(x) - min(x)
	d <- (tgt - min(x)) / rg
	return(d)
}

perf_details <- function(rets, verbose=TRUE)
{
	if (verbose) cat('.')
	final_ret <- exp(sum(na.omit(rets))) - 1
	return(final_ret)
	if (verbose) cat('.')	
	ann_ret <- Return.annualized(rets)
	if (verbose) cat('.')
	sdev <- StdDev.annualized(rets)
	if (verbose) cat('.')
	sharpe <- SharpeRatio.annualized(rets)
	if (verbose) cat('.')
	maxdd <- maxDrawdown(rets)
	c(final_ret, ann_ret, sdev, sharpe, maxdd)
}

####
# n is the relative vol lookback period
# hv_n is how many periods to use when calculating hv
# hv_thresh is the cutoff point for 'high' vol
# if curves=FALSE (default) will return a summary of performance metrics
# if curves=TRUE will return the equity curves
###
comp_rets <- function(sym, n=63, hv_n=21, hv_thresh=0.60, curves=FALSE, verbose=TRUE) 
{
	me <- sub('.Open', '', colnames(sym)[1])
	if (verbose) cat(me)
	
	if (verbose) cat(' .')	
	base_ret <- ROC(Cl(sym))

	if (verbose) cat('.')
	histvol <- rollapply(base_ret, hv_n, rv, align='right', fill=NA)	
	hv.prop <- rollapply(histvol, n, prop.dist, align='right', fill=NA)
	hv_sig <- as.vector(Lag(ifelse(hv.prop < hv_thresh, 1, 0)))
	hv_ret <- ROC(Cl(sym)) * hv_sig

	if (verbose) cat('.')
	ma_sig <- Lag(ifelse(Cl(sym) > SMA(Cl(sym), 200), 1, 0))
	ma_ret <- ROC(Cl(sym)) * ma_sig
	
	if (verbose) cat('.')	
	hv_ma_ret <- ROC(Cl(sym)) * hv_sig * ma_sig
	
	###trim the returns so they are all the same length
	#otherwise, with a 200 ma, we would be comparing almost an 
	#extra year of the base vs the ma series
	retlist <- list(base_ret, hv_ret, ma_ret, hv_ma_ret)
	minlen <- min(sapply(retlist, function(x) length(na.omit(x))))
	retlist <- lapply(retlist, tail, minlen)

	if (curves) {
		if (verbose) cat(' building curves .')
		base_eq <- exp(cumsum(tail(base_ret, minlen)))
		if (verbose) cat('.')
		hv_eq <- exp(cumsum(tail(hv_ret, minlen)))
		if (verbose) cat('.')
		ma_eq <- exp(cumsum(tail(ma_ret, minlen)))
		if (verbose) cat('.')
		hv_ma_eq <- exp(cumsum(tail(hv_ma_ret, minlen)))
		crvdata <- list(base=base_eq, hv=hv_eq, ma=ma_eq, hv_ma=hv_ma_eq)
		if (verbose) cat(' done\n')
		return(crvdata)		
	}
	
	if (verbose) cat(' building performance details ')
	outdata <- sapply(retlist, perf_details, verbose)
	cat(me, n, outdata, '\n')
#	colnames(outdata) <- c('Base', 'HV', 'MA', 'HV_MA')
	#rownames(outdata) <- c('Final', 'Ann.Ret', 'Ann.Sd', 'Sharpe', 'MaxDD')	
#	rownames(outdata) <- c('Final')	
	if (verbose) cat(' done\n')
	return(outdata)
}

symenv <- new.env()
getSymbols(c('^GSPC', '^RUT', '^NDX'), env=symenv, from="1980-01-01")
getSymbols(c('^NDX'), env=symenv, from="1980-01-01")
perfdata <- lapply(symenv, comp_rets)
#you can look at the equity curves instead
#curves <- lapply(symenv, comp_rets, curves=T)
#plot(curves[["GSPC"]][["hv_ma"]])

#write the data to csv
for (nm in names(perfdata)) {
	outname <- paste('vix.hv.volfilter', nm, 'csv', sep='.')
	write.csv(perfdata[[nm]], outname)
}

