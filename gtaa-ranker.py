#!/usr/bin/env python

# simple equity ranking backtest with python/pandas
# for more info http://petewerner.blogspot.com/2015/05/equity-ranking-backtest-with.html

import code
import pandas as pd
pd.set_option('line_width', pd.util.terminal.get_terminal_size()[0])
import numpy as np

import os

#the data is in this directory, 1 file per security named SYM.csv
datadir = './etfdata' 

ohlc_dict = {'Open':'first','High':'max','Low':'min','Close': 'last','Volume': 'sum', 'Adj Close':'last'}
agg_tf = 'M'

#average annual return
def avg_ar(rets):
	y = np.exp(np.sum(rets))
	n = len(rets)
	avg_ret = np.exp(np.log(y)/n)
	if agg_tf == 'M':
		k = 12
	else:
		k = 52
	return pow(avg_ret, k)

#the gain to pain ratio
def gp(rets, log=True):
	if log:
		rets = np.exp(rets) - 1
	return np.sum(rets)/np.abs(np.sum(rets[rets < 0]))

#build our data frame with the ranking information
def make_df(sym, df, drop=True):
	#resample our dataframe from daily to a higher time frame
	adf = df.resample(agg_tf, how=ohlc_dict)
	#we use the adjust close prices for this test
	col = 'Adj Close'
	#you can also use the close prices
	#col = 'Close'

	#current period return
	adf['cpr'] = np.log(adf[col].pct_change() + 1)
	#adf['cpr'] = np.log(adf['Close'] / adf['Open'])

	#next period return
	adf['npr'] = adf['cpr'].shift(-1)

	#rolling avg for 3 months
	adf['avg'] = pd.rolling_mean(adf.cpr, 3)

	#is price over its 10 month avg
	adf['over'] = adf[col] > pd.rolling_mean(adf[col], window=10)

	#get rid of rows we have no data for
	if drop:
		adf.dropna(inplace=True)
	adf['sym'] = sym
	return adf

df_list = []
syms = []

#load our data
for f in os.listdir(datadir):
	#strip .csv
	sym = f[:-4]
	path = os.path.join(datadir, f)
	df = pd.DataFrame.from_csv(path)
	df = make_df(sym, df)
	df_list.append(df)
	syms.append(sym)

#make a combined dataframe that has our data
df = pd.concat(df_list)

rets = []
symlist = []

#we pick the top n each month
n = 2 

#work out first date we have data for all syms
start_date = max([df[df.sym==s].index.min() for s in syms])

#we will run through till the end of the data
idxr = pd.date_range(start_date, df.index.max(), freq=agg_tf)

for idx in idxr:
	#select syms for this date
	t = df.ix[idx]
	#select only those over the 10 month moving avg
	t = t[t.over == True]
	#select only those with +ve avg return
	t = t[t.avg > 0]
	#sort them by avg return and select the top n
	tmp = t.sort('avg', ascending=False).head(n)	

	#calc monthly returns
	#first unlog them
	mr = np.exp(tmp['npr']) - 1
	#then mult by allocation amt and take the sum of that
	r = (mr * 1/n).sum()
	#then convert back to log returns
	r = np.log(r + 1)
	rets.append(r)
	#keep a record of which syms we selected
	symlist.append(list(tmp['sym']))

#print out gain/pain ration, avg. annual return and total return over the test
print "% 4.2f % 4.4f % 6.2f" % (gp(rets), avg_ar(rets), np.exp(np.sum(rets)))

#uncomment this if you want to poke around the data
#code.interact(local=locals())


