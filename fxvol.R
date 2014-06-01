###
# some quick plots looking at recent fx volatility levels - Pete Werner 2014
#
# post http://petewerner.blogspot.com/2014/05/a-quick-look-at-fx-realized-vol.html
#
####

library(xts)
library(quantmod)

load_data <- function(infile, scale=10000) {

	df <- read.csv(infile, header=F, stringsAsFactors=F)
	#get date component
	dt2 <- apply(df[,1:2], 1, function(x) paste(x[1], x[2]))
	#convert to time objects
	dt3 <- as.POSIXlt(strptime(dt2, "%Y.%m.%d %H:%M"))
	#make an xts object indexed by dates
	x <- xts(df[,3:6], order.by = dt3)
	colnames(x) <- c('Op', 'Hi', 'Lo', 'Cl')
	#calc log returns
	x$ret <- ROC(x$Cl)
	#rv defined as hi - lo
	x$rv <- (x$Hi - x$Lo) * scale
	x <- na.omit(x)
	return(x)
}

eur <- load_data('EURUSD60.csv')
gbp <- load_data('GBPUSD60.csv')
jpy <- load_data('USDJPY60.csv', scale=100)

d1 <- as.vector(apply.daily(eur$rv, sum))
d2 <- as.vector(apply.daily(jpy$rv, sum))
d3 <- as.vector(apply.daily(gbp$rv, sum))

minelts <- min(length(d1), length(d2), length(d3))

EUR <- tail(d1, minelts)
JPY <- tail(d2, minelts)
GBP <- tail(d3, minelts)

df <- data.frame(EUR, JPY, GBP)
boxplot(df)

#####

d1 <- as.vector(apply.daily(eur$rv, mean))
d2 <- as.vector(apply.daily(jpy$rv, mean))
d3 <- as.vector(apply.daily(gbp$rv, mean))

minelts <- min(length(d1), length(d2), length(d3))

EUR <- tail(d1, minelts)
JPY <- tail(d2, minelts)
GBP <- tail(d3, minelts)

idx <- 1:minelts

plot(idx, EUR, type='l', ylim=c(min(JPY), max(JPY)))
lines(idx, JPY, type='l', col='red')
lines(idx, GBP, type='l', col='blue')

