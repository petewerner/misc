##
# rolling train/test
# makes rolling subsets of data, and further splits each subset into train/test periods
#
# Copyright Pete Werner 2013
# more info http://petewerner.blogspot.com/2013/09/building-models-over-rolling-time.html


library(quantmod)
library(kernlab)

getSymbols("^GSPC")
cl <- ROC(Cl(GSPC))
cl <- na.omit(cl)

###
#I have daily data, and want to build a model based on n weeks of previous data and see how it performs over m weeks going forward.
#
#First convert our data into what we want, in this case we are looking at log closes. 
#Y is close at time t, x1 close at time t-2, x2 t-2 and so on.
###

data_prep <- function(data, lookback=5)
{
	tmp <- cbind(data, Lag(data, 1:lookback))
	colnames(tmp) <- c("Y", paste("X", 1:(ncol(tmp) - 1), sep=''))
	return(tmp)
}

#head(cl)
data <- data_prep(cl)
data <- na.omit(data)
#head(data)

#for each subset of data, we further split it into 2 groups, a training set of "train" periods, and a test set of "test" periods

#will return a list with the train/test set
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


#l <- train_test_split(data[1:30])

#once we have our list, we further split the test set x/y
#then we build the model, and see how it goes on our test set
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

#finally we have the main function, which loops through all the data
#and calls run_model, collecting the results
roll_model <- function(data, trainsz=4, testsz=1, period='weeks', verbose=FALSE, sinkfile=NA)
{
	#how much data we need for each model run
	totsz <- trainsz + testsz
	#get the end point indexes
	ep <- endpoints(data, period)
	#we work "forward" from idx 1, so we need to stop a little early
	endlen <- length(ep) - totsz
	mr <- c()
	for (i in 1:endlen) {
		startidx <- ep[i] + 1 #the starting index (note endpoints has 0 as the first index)
		endidx <- ep[i + totsz] #the end index for this run
		if (verbose && i %% 10 == 0)
			cat(sprintf("%.2f %d %d %d\n", i/endlen, i, startidx, endidx))
		datasub <- data[startidx:endidx,] #our data subset
		if (!is.na(sinkfile))
			sink(sinkfile)
		#run the model
		mr <- rbind(mr, run_model(datasub, trainsz, testsz, period))
		if (!is.na(sinkfile))
			sink()
	}	
	return(mr)
}

res <- roll_model(data, trainsz=13, testsz=1, period="months", sinkfile='/dev/null')
#see how it went at predicting the direction
acc <- ifelse(sign(res[,1]) == sign(res[,2]), 1, 0)
cat(sprintf("accuracy: %.2f\n", sum(acc)/nrow(res)))


