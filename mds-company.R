library(quantmod)

#plotting libraries
library(ggplot2)
library(gridExtra)
library(directlabels) #for ggplot labels
library(quadprog) #for directlabels

###
# Apply MDS to financial ratios and plot the results
# Copyright Peter Werner 2012
#
# First we get the line items we are interested in (base_data), then
# calculate the ratios in a seperate structure (ratio_data). We have
# to get the market prices seperately with getSymbols() as they dont
# come in the data returned by getFin()
# Once we have all that we do mds using cmdscale() from base, and 
# plot the results with ggplot2
###

###
# The symbols we want to plot
##

#symbols <- c("ADBE", "INTU", "MSFT", "NTWK", "RHT", "VRSN", "INTC", "AAPL")
#symbols <- c("ADBE", "MSFT", "INTC")
symbols <- c("C", "JPM", "GS", "MS","RBS", "DB", "UBS", "WFC")
finEnv <- new.env()

#load symbol fin data into finEnv
#this will generate some warnings, but they are ok to ignore
#if you get an error like NA/NaN, look at ls(finEnv)
sapply(symbols, getFin, env=finEnv)
#getFin(symbols, env=finEnv)

#get the price data, since getFin will only return 
#the last 4 years at most, we can start from 2008
getSymbols(symbols, env=finEnv, from="2008-01-01")

##
# string defs, this is done to minimize the chance of typos wreaking havoc
net_income <- "Net Income"
total_equity <- "Total Equity"
eps <- "Diluted Normalized EPS"
operating_income <- "Operating Income"
gross_profit <- "Gross Profit"
current_assets <- "Total Current Assets"
current_liabilities <- "Total Current Liabilities"
total_debt <- "Total Debt"
total_assets <- "Total Assets"
total_liabilities <- "Total Liabilities"
share_price <- "Share Price"

AQ <- "A" #A for annual, Q for quarterly

IS <- "IS" #income statement
BS <- "BS" #balance sheet

#items used in formulas and which part of the statement they appear
lineItems <- list(
	c(net_income, IS), 
	c(total_equity, BS),
	c(eps, IS),
	c(operating_income, IS),
	c(gross_profit, IS),
	c(current_assets, BS),
	c(current_liabilities, BS),
	c(total_debt, BS),
	c(total_assets, BS),
	c(total_liabilities, BS),
	c(share_price, IS)	
)

#the ratios we are interested in
#the first part (arg) of the list defines the variables to use in calculation
#the second (fn) is the actual function, with variables passed in the order declared
rats <- list(
	#return on equity
	roe=list(args=c(net_income, total_equity), fn=function(x) x[1,]/x[2,]),
	#earnings per share, this is net_income / diluted weighted avg shares
	eps=list(args=c(eps), fn=function(x) x), #no op
	#price/earnings
	pe=list(args=c(share_price, eps), fn=function(x) x[1,]/x[2,]),
	#operating margin - skip for financials
	#op_mgn=list(args=c(operating_income, gross_profit), fn=function(x) x[1,]/x[2,]),
	#current ratio - skip for financials
	#cur_rat=list(args=c(current_assets, current_liabilities), fn=function(x) x[1,]/x[2,]),
	#gearing, should be net debt
	gearing=list(args=c(total_debt, total_equity), fn=function(x) x[1,]/x[2,]),
	#asset turnover - skip for financials
	#ass_turn=list(args=c(gross_profit, total_assets), fn=function(x) x[1,]/x[2,]),
	#debt ratio
	debt_rat=list(args=c(total_liabilities, total_assets), fn=function(x) x[1,]/x[2,])
)

###
# work out the close price at the time the results were released
##
get_prices <- function(sym, env=.GlobalEnv) {
	
	#look up the getFin object for sym
	symf_name <- paste(sym, ".f", sep="")
	symf <- get(symf_name, envir=env)
	#get the price data
	symd <- get(sym, env)
	#pick a line to get the dates from (1 = BS)
	dates <- as.Date(colnames(symf[[1]][[AQ]]))
	#get the date indexes for the given symbol
	didx <- index(symd)
	#find the date before the posted results, a bit slow but meh
	end_dates <- sapply(1:length(dates), function(x) last(didx[didx < dates[x]]))
	#pull out the actual close prices for the given dates
	closes <- as.vector(Cl(symd[as.Date(end_dates)]))
	#the most recent prices come first
	closes <- rev(closes)
	#add it to the IS, row name "Share Price"
	symf[[IS]][[AQ]] <- rbind(symf[[IS]][[AQ]], "Share Price"=closes)
	assign(symf_name, symf, envir=env)
	return(TRUE)
}

#load up the market price
sapply(symbols, get_prices, env=finEnv)

###
# with those definitions out of the way, get just the data we are interested in
###

#lineItem is a tuple of Item and sheet, eg c("Net Income", "IS") gives Net Income from I&E
#sym is a financial symbol from getFin() (e.g AAPL.f)
#aq is "A" for annual data, "Q" for quarterly data
get_line_item <- function(lineItem, sym, aq="A")
{
	item_name <- lineItem[1]
	sheet <- lineItem[2]
	#look up the line item by name and get it's index
	idx <- which(dimnames(sym[[sheet]][[aq]])[[1]] == item_name)
	#the actual data for the given item
	line <- sym[[sheet]][[aq]][idx,]
	return(line)	
}

get_symbol_lines <- function(sym, items, env=.GlobalEnv, aq="A")
{
	#look up the financial data from our financial env
	s <- get(paste(sym, ".f", sep=""), envir=env)
	#create a matrix that has the periods for columns 
	# and the line items in the rows
	m <- do.call(rbind, lapply(items, get_line_item, sym=s, aq=aq))
	#set the names of the rows to the line item name
	rownames(m) <- sapply(items, function(x) x[1])
	return(m)
}

#this will load up the line items we are interested in
#gives a list of matrices
#each list item is the data for the company 
#eg: base_data[["AAPL"]]["Net Income",] gives the net income for AAPL
base_data <- lapply(symbols, get_symbol_lines, items=lineItems, env=finEnv, aq=AQ)
names(base_data) <- symbols

###
# now we have the data we are interested in, we can calculate the ratios
###

#apply the given function from the ratios list
apply_fn <- function(listent, sym)
{
	#get the function string and make it a callable function
	f <- eval(listent$fn)
	#call it, passing the arguments defined
	#f(sym[listent$args,])
	v <- f(sym[listent$args,])
	#strip out Inf values
	v <- replace(v, v==Inf, 0)
	return(v)
}

#calculate the values for each ratio defined
build_rats <- function(sym, ratios, base_data)
{
	#get the data for the given symbol
	a <- base_data[[sym]]
	#calc all the ratios for it
	do.call(rbind, lapply(ratios, apply_fn, sym=a))
}

#ratio_data is a list of matrices
#index to list is symbol, eg AAPL
#cols of matrix are reporting period, rows are the ratios
ratio_data <- lapply(symbols, build_rats, ratios=rats, base_data=base_data)
names(ratio_data) <- symbols

#build the matrix with the ratio data
build_mds_mtx <- function(data, period=1)
{ 
	ratio_matrix <- sapply(data, function(x) x[names(rats),period])
	rm_mult <- t(ratio_matrix) %*% ratio_matrix
	rm_dist <- dist(rm_mult)
	rm_mds <- cmdscale(rm_dist)
	return(rm_mds)
}

#change the period to a 2, 3 or 4 for previous periods
rm_mds <- build_mds_mtx(ratio_data)
df <- as.data.frame(rm_mds)
symnames <- rownames(df)

title <- "Company similarity\n"
title <- paste(title, ifelse(AQ == "A", "Annual", "Quarterly"), "Period 1")

p <- ggplot(df, aes(V1, V2, label=symnames)) + geom_point(aes(colour=symnames)) 
p <- p + opts(title=title)
p <- p + xlab("") + ylab("")
p <- p + opts(axis.ticks = theme_blank(), axis.text.x = theme_blank())
p <- p + opts(axis.ticks = theme_blank(), axis.text.y = theme_blank())
#p <- direct.label(p, first.qp) #tends to cut off labels
p <- direct.label(p)
p

#basic plot
#plot(rm_mds, type="n")
#text(rm_mds, names(ratio_data))

#multiplot comes from 
#http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
multiplot <- function(..., plotlist=NULL, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}

build_mds_plot <- function(ratio_data, period)
{
	rm_mds <- build_mds_mtx(ratio_data, period)
	df <- as.data.frame(rm_mds)
	symnames <- rownames(df)

	title <- "Company similarity\n"
	title <- paste(title, ifelse(AQ == "A", "Annual", "Quarterly"), "Period", period)

	p <- ggplot(df, aes(V1, V2, label=symnames)) + geom_point(aes(colour=symnames)) 
	p <- p + opts(title=title)
	p <- p + xlab("") + ylab("")
	p <- p + opts(axis.ticks = theme_blank(), axis.text.x = theme_blank())
	p <- p + opts(axis.ticks = theme_blank(), axis.text.y = theme_blank())
	#p <- direct.label(p, first.qp) #tends to cut off labels
	p <- direct.label(p)
	return(p)	
}

plots <- lapply(c(1:4), function(x) build_mds_plot(ratio_data, period=x))
png("mds-annual-multiplot.png", width=645, height=640)
multiplot(plotlist=plots,cols=2)
dev.off()

png("mds-annual-period1.png", width=600, height=600)
plots[[1]]
dev.off()
