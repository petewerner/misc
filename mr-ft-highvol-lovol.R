library(quantmod)

# Taking a look at low vol vs high vol strategies, Pete Werner, June 2014
#
# http://petewerner.blogspot.com/2014/06/trading-in-low-vol-world.html
#

rv <- function(x, dpy=252)
{
        n <- length(x)
        #assume mean ret is 0
        meanret <- 0
        r <- sum((x - meanret)^2, na.rm=TRUE)/(n - 1)
        r <- r * dpy
        return(100 * sqrt(r))
}

mr <- function(x) {
	ifelse(x$cpr >= 0, -1 * x$npr, x$npr)
}

ft <- function(x) {
	ifelse(x$cpr >= 0, x$npr, -1 * x$npr)
}


#ntraj is how many trajectories to generate
#sz is the proportion of the data set to use for each sampled trajectory
#rv len is the rv lookback size, 21 is roughly 1 trading month
mktraj <- function(sym, ntraj=100, sz=0.3, rvlen=21) {

	rets <- as.vector(ROC(Cl(sym)))
	realvol <- rollapplyr(rets, rvlen, rv, fill=NA)
	medvol <- median(realvol, na.rm=T)
	dm <- data.frame(cpr=rets, npr=Next(rets), realvol)
	colnames(dm) <- c('cpr', 'npr', 'realvol')
	dm <- na.omit(dm)
	
	lvi <- which(dm$realvol < medvol)
	hvi <- which(dm$realvol >= medvol)
	
	nlv <- length(lvi)
	nhv <- length(hvi)
	lvsz <- nlv * sz
	hvsz <- nhv * sz
	
	#cat(sprintf("%d low vol, %d high vol, samp lo %d samp hi %d, med vol %.2f\n", nlv, nhv, lvsz, hvsz, medvol))

	lvtraj_mr <- list()
	lvtraj_ft <- list()
	hvtraj_mr <- list()
	hvtraj_ft <- list()
	
	for (i in 1:ntraj) {
		idx <- sample(lvi, lvsz, rep=T)
		lvtraj_mr[[i]] <- mr(dm[idx,])
		idx <- sample(lvi, lvsz, rep=T)
		lvtraj_ft[[i]] <- ft(dm[idx,])
		
		idx <- sample(hvi, hvsz, rep=T)
		hvtraj_mr[[i]] <- mr(dm[idx,])
		idx <- sample(hvi, hvsz, rep=T)		
		hvtraj_ft[[i]] <- ft(dm[idx,])
	}
	
	lv_mr <- mean(sapply(lvtraj_mr, sum))
	lv_ft <- mean(sapply(lvtraj_ft, sum))
	hv_mr <- mean(sapply(hvtraj_mr, sum))
	hv_ft <- mean(sapply(hvtraj_ft, sum))
	lv_tr <- sum(dm[lvi, ]$npr)
	hv_tr <- sum(dm[hvi, ]$npr)	
	return(c(lv_mr=lv_mr, lv_ft=lv_ft, lv_tr=lv_tr, hv_mr=hv_mr, hv_ft=hv_ft, hv_tr=hv_tr))
}

symenv <- new.env()
getSymbols(c("^GSPC", "^NDX", "^RUT"), env=symenv, from="1999-01-01")

#should enable you to get the same numbers as me
set.seed(12345)

#takes about 11 secs on my machine
system.time(x <- sapply(symenv, mktraj, ntraj=1000, sz=0.5))
#un log the returns
t(exp(x) - 1)

#t(sapply(symenv, mktraj))