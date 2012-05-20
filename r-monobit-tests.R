library(stats)
library(quantmod)

/*
 * NIST Monobit test, from http://csrc.nist.gov/groups/ST/toolkit/random_number.html#RNG Test
 * 
 * Copyright (c) Peter Werner 2012.
 */

code_input <- function(sym, fn=Cl) {
	return(na.omit(as.vector(ifelse(ROC(fn(sym)) >= 0, 1, -1))))
}

#from help(stats)
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)

monobit <- function(v) { 
	sobs <- abs(sum(v))/sqrt(length(v)) ; 
	return(erfc(sobs/sqrt(2)))
}

run_test_block <- function(input, n=100, sig=0.01) {
	nrand <- 0
	nnonrand <- 0
	cnt <- 0
	for (i in seq(1, length(input), n)) {
	    if ((i + n) < length(input)) {
	        cnt <- cnt + 1
	        pval <- monobit(input[i:(i+n)])
	        if (pval < sig) {
	            nnonrand <- nnonrand + 1
	        } else {
	            nrand <- nrand + 1          
	        }
	    }
	}
	return(c(cnt, nrand, nnonrand))
}

run_test <- function(input, n=100, sig=0.01) {
	
	nrand <- 0
	nnonrand <- 0
	cnt <- 0
	for (i in 1:(length(input) - n)) {
		cnt <- cnt + 1
		pval <- monobit(input[i:(i+n)])
		if (pval < sig) {
			nnonrand <- nnonrand + 1
		} else {
			nrand <- nrand + 1			
		}
	}
	return(c(cnt, nrand, nnonrand))	
}

getSymbols("^GSPC", from="1970-01-01")
r <- run_test(code_input(GSPC))
sprintf("%d tests %d random, %d nonrandom", r[1], r[2], r[3])

wk <- to.weekly(GSPC)

d <- code_input(GSPC)
period <- "Daily"
d <- code_input(wk)
period <- "Weekly"

runlen <- 100
sig_levels <- seq(0.01, 0.30, 0.01)
prop_non_rand <- rep(0, length(sig_levels))
for (i in 1:length(sig_levels)) {
	r <- run_test(d, runlen, sig_levels[i])
	prop_non_rand[i] <- r[3]/r[1]
}


plot(sig_levels, prop_non_rand*100, xlab="Significance Level", ylab="Percent non-random", main=sprintf("Monobit Randomness Test\nGSPC %s 1970-2012 runlen = %d", period, runlen), type="s", ylim=c(0, 60)) 


