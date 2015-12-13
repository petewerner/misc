#!/usr/bin/env python

##
# demonstration of the halloween effect, aka sell in may and go away
# for details see http://petewerner.blogspot.com/2015/12/the-halloween-effect-with-python-and.html
##

import pandas as pd
import numpy as np
from pandas_datareader import data as web

#download the data
df = web.get_data_yahoo('SPY', start='1/1/1993')

#current period return, log of close to close changes
df['cpr'] = np.log(df['Close'].pct_change() + 1)
#conver to monthly data, we can sum log returns
df = df.resample('M', how={'cpr':'sum'})
#our halloween indicator variable
#months 1, 2, 3, 4, 5, 10, 11 and 12 get a 1, 
#months 6, 7, 8 and 9 get 0 
df['hal'] = np.where(df.index.month % 10 <= 5, 1, 0)
df = df.dropna()

#run our regression (as of 2015/12/13)
ls = pd.ols(y=df.cpr, x=df.hal)
print ls

#...
#-----------------------Summary of Estimated Coefficients------------------------
#      Variable       Coef    Std Err     t-stat    p-value    CI 2.5%   CI 97.5%
#--------------------------------------------------------------------------------
#             x     0.0141     0.0054       2.61     0.0096     0.0035     0.0246
#     intercept    -0.0038     0.0044      -0.87     0.3869    -0.0124     0.0048
#---------------------------------End of Summary---------------------------------

#look at the average return across groups
print df.groupby('hal').cpr.mean()
#hal
#0   -0.003809
#1    0.010245

#can check the un-logged ones as well
print df.groupby('hal').cpr.apply(lambda x: (np.exp(x) - 1).mean())
#hal
#0   -0.002840
#1    0.011133

