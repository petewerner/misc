###
# simple simulations look at trade entries, Pete Werner 2015
#
# for more info please see here http://petewerner.blogspot.com/2015/03/simulation-and-relative-performance.html
##

library(quantmod)

getSymbols("SPY", from="2005-01-01")
sym <- SPY["2005/2014"]
sym <- to.weekly(sym)

len <- length

#current period return
cpr <- ROC(Cl(sym))
#next period return
npr <- Next(cpr)
df <- data.frame(cpr, npr)
colnames(df) <- c('cpr', 'npr')
df <- na.omit(df)
sig <- ifelse(df$cpr <= 0, 1, 0)

#the ma code, skip the to.weekly() line above as well
#cpr <- ROC(Cl(sym))
#npr <- Next(cpr)
#sma <- SMA(Cl(sym), 200)
#df <- data.frame(Cl(sym), sma, npr)
#colnames(df) <- c('cl', 'sma', 'npr')
#df <- na.omit(df)
#sig <- ifelse(df$cl > df$sma, 1, 0)

val <- df$npr * sig
#total return over the period
sys_tot_ret <- exp(sum(val))
#average return of trades
sys_avg_ret <- mean(exp(val[val != 0]) - 1)
#accuracy (of taken trades)
sys_acc <- len(val[val > 0])/len(val[val != 0])

#proportion of times a signal was generated
prop <- len(sig[sig != 0])/len(sig)

prop; sys_tot_ret; sys_avg_ret; sys_acc
#[1] 0.4442308
#[1] 1.907858
#[1] 0.003330592
#[1] 0.6017316

#our simulation function (the x arg is ignored)
randsim <- function(x, data, enter_prop) {
	#random signal, roughly the same amount of trades as the actual system
	rsig <- ifelse(runif(nrow(data)) < enter_prop, 1, 0)
	val <- data$npr * rsig
	
	#our summmary stats
	active_prop <- mean(rsig)
	total_ret <- exp(sum(val))
	avg_ret <- mean(exp(val[val != 0]) - 1)
	acc <- len(val[val > 0])/len(val[val != 0])
	return(c(act=active_prop, tr=total_ret, ar=avg_ret, acc=acc))
}

nruns <- 10000
res <- sapply(1:nruns, randsim, df, prop)
res <- t(res)

#want to see act is roughly the same as prop we passed in
apply(res, 2, mean)
#        act          tr          ar         acc 
#0.444260769 1.337813228 0.001407365 0.557261231 

pdf <- data.frame(res[,2:4])

#cheap pvalues
sum(pdf$acc > sys_acc)/nruns
#[1] 0.0362
sum(pdf$tr > sys_tot_ret)/nruns
#[1] 0.0882
sum(pdf$ar > sys_avg_ret)/nruns
#[1] 0.0639

#make some plots
library(ggplot2)
library(reshape2)

#equity curve

sys_eq <- exp(cumsum(val))
bm_eq <- exp(cumsum(df$npr))
eqdf <- melt(data.frame(t=1:len(sys_eq), sys_eq, bm_eq), id="t")
ggplot(data=eqdf, aes(x=t, y=value, group=variable, color=variable)) + geom_line()

#density plots

p1 <- ggplot(data=pdf, aes(x=tr)) + geom_density(alpha=.3, fill="#FF6666") +
	geom_vline(aes(xintercept=mean(pdf$tr)), color="red") +
	geom_vline(aes(xintercept=sys_tot_ret), color="blue") +
	ggtitle("Total Return")

p2 <- ggplot(data=pdf, aes(x=ar)) + geom_density(alpha=.3, fill="#FF6666") +
	geom_vline(aes(xintercept=mean(pdf$ar)), color="red") +
	geom_vline(aes(xintercept=sys_avg_ret), color="blue") +
	ggtitle("Average Return")

p3 <- ggplot(data=pdf, aes(x=acc)) + geom_density(alpha=.3, fill="#FF6666") +
	geom_vline(aes(xintercept=mean(pdf$acc)), color="red") +
	geom_vline(aes(xintercept=sys_acc), color="blue") +
	ggtitle("Accuracy")


#this one comes from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

png("rst_mr_comb.png", width=950, height=330)
multiplot(p1, p2, p3, cols=3)
dev.off()



