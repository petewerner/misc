library(quantmod)
library(PerformanceAnalytics)
library(kernlab)

###
# This script is about data-snooping, and why it can sometimes be useful
# http://petewerner.blogspot.com/2013/10/the-case-for-data-snooping.html
###


###
# collate some summary statistics
###
retsummary <- function(x, scale=252)
{
	up <- x[x > 0]
	dn <- x[x < 0]
	mup <- mean(up)
	mdn <- mean(dn)
	acc <- length(up) / length(x)
	stddev <- sd.annualized(x)[1]
	annret <- Return.annualized(as.vector(x), scale)
	maxdd <- maxDrawdown(x)
	c(acc=acc, sd=stddev, annret=annret, maxdd=maxdd, avgup=mup, avgdn=mdn)
}

#turn a list of retsummary's into a table of sorts
lst2tbl <- function(deets) matrix(unlist(deets), nr=length(deets), byrow=T, dimnames = list(names(deets), names(deets[[1]])))

#rolling model stuff, described here http://petewerner.blogspot.com.au/2013/09/building-models-over-rolling-time.html
data_prep <- function(data, lookback=5, uselog=FALSE, snoop=FALSE)
{
	if (uselog) 
		data <- ROC(Cl(data))
	else
		data <- Cl(data)

	if (snoop)
		tmp <- cbind(data, data, Lag(data, 1:(lookback-1)))
	else
		tmp <- cbind(data, Lag(data, 1:lookback))

	colnames(tmp) <- c("Y", paste("X", 1:(ncol(tmp) - 1), sep=''))
	return(tmp)
}

## make training set
# get prev 4 weeks of data by default
train_test_split <- function(data, train=4, test=1, period="weeks")
{
	ep <- endpoints(data, on=period)
	if (length(ep) < (train+test+1)) 
		stop(sprintf("wanted %d %s, only got %d", train + test, period, length(ep)-1))
	train_end <- ep[train + 1]
	trainset <- data[1:train_end,]
	test_start <- train_end + 1
	test_end <- ep[train + test + 1]
	testset <- data[test_start:test_end,]
	return(list(train=trainset, test=testset))
}

run_model <- function(data, trainsz=4, testsz=1, period='weeks')
{
	tt <- train_test_split(data, trainsz, testsz, period)
	trainset <- tt[["train"]]
	testset <- tt[["test"]]
	testX <- testset[,-1]
	testY <- testset[,1]
	
	mod <- ksvm(Y~., trainset)
	pr <- predict(mod, testX)
	mat <- cbind(pr, testY)
	colnames(mat) <- c("pred", "actual")
	return(mat)
}

roll_model <- function(data, trainsz=4, testsz=1, period='weeks', verbose=FALSE)
{
	totsz <- trainsz + testsz
	ep <- endpoints(data, period)
	endlen <- length(ep) - totsz
	mr <- c()
	for (i in 1:endlen) {
		startidx <- ep[i] + 1
		endidx <- ep[i + totsz]
		if (verbose && i %% 10 == 0)
			cat(sprintf("%.2f %d %d %d\n", i/endlen, i, startidx, endidx))
		datasub <- data[startidx:endidx,]
		sink("/dev/null")
		#make sure the sink gets turned off if there is an error 
		mod <- tryCatch(run_model(datasub, trainsz, testsz, period), finally=sink())
		mr <- rbind(mr, mod)
	}	
	return(mr)
}

##end rolling model stuff

###
# turn our price prediction into log returns
# predicted price is in col 1, actual price in col 2. 
# put put the previous close in col 3 and go from there
###
res2rets <- function(res, clvec) 
{
	prev <- Lag(clvec)
	tmp <- cbind(res, prev[index(res)])
	t(apply(tmp, 1, function(x) c(log(x[1]/x[3]), log(x[2]/x[3]) )))
}

###
# make the actual returns realised.
# the indicator/signal is the sign (ie direction) of our prediction in col 1
# the actual returns are in col 2
###
retvec <- function(rets)
{
	ind <- sign(rets[,1])
	rval <- rets[,2] * ind
	return(rval)
}


getSymbols("^GSPC", from='2000-01-01')
spx <- GSPC

smavals <- SMA(Cl(spx), 200)
sigsma <- ifelse(Cl(spx) > smavals, 1, -1)

cl <- ROC(Cl(spx))
cl <- na.omit(cl)

#200 day sma
maret <- cl * Lag(sigsma)
maret <- na.omit(maret)

#same but with 1 day lookahead
maretsn <- cl * sigsma
maretsn <- na.omit(maretsn)

####
# svm using prices
####

#set up our testing data
sndata <- data_prep(spx, snoop=T)
#train/test the models, gives prediction and actual value
snres <- roll_model(sndata)
#convert predicted/actual to returns for analysis
snrets <- res2rets(snres, Cl(spx))
#make the actual return series generated
snrv <- retvec(snrets)

data <- data_prep(spx, snoop=F)
res <- roll_model(data)
rets <- res2rets(res, Cl(spx))
rv <- retvec(rets)

#####
# using log returns
#####

sndatar <- data_prep(spx, uselog=T, snoop=T)
snresr <- roll_model(sndatar)
snrvr <- retvec(snresr)

datar <- data_prep(spx, uselog=T, snoop=F)
resr <- roll_model(datar)
rvr <- retvec(resr)

####
# generate summary table
####

info <- list(sma.sn=maretsn, sma=maret, spx.cl.sn=snrv, spx.cl=rv, spx.r.sn=snrvr, spx.r=rvr)
tbl <- lst2tbl(lapply(info, retsummary))
tbl


