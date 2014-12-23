# 2009 http://www.automated-trading-system.com/trend-following-wizards-december-09/ -7.3% on average
# 2010 http://www.automated-trading-system.com/a-look-back-at-trend-following-in-2010/  +18.91%
# 2011 http://www.automated-trading-system.com/state-of-trend-following-in-2011/ YTD return: -15.58%
# 2012 http://www.automated-trading-system.com/state-of-trend-following-in-december/ YTD return: -4.14%
# 2013 http://www.automated-trading-system.com/state-of-trend-following-in-2013/ YTD return: 3.07%
# 2014 http://www.automated-trading-system.com/state-trend-following-november-2014/ YTD return: 45.08%


tfrets <- c(-7.3, 18.91, -15.58, -4.14, 3.07, 45.08)
tfrets <- 1 + tfrets/100
tfrets
#[1] 0.9270 1.1891 0.8442 0.9586 1.0307 1.4508

cumprod(tfrets)
#[1] 0.9270000 1.1022957 0.9305580 0.8920329 0.9194183 1.3338921

round(100*cumprod(tfrets), 2)
#[1]  92.70 110.23  93.06  89.20  91.94 133.39
