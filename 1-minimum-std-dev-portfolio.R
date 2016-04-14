# set working directory
setwd("~/R_portfolio")

# load library quant mod to get prices from yahoo
library(quantmod)

# get ticker data for Apple (AAPL) from Yahoo Finance between 01/01/2010 and today
getSymbols.yahoo("AAPL", env=globalenv(), from="2010-01-01", to = Sys.Date())

# if you want to get the full data history for Apple available on Yahoo Finance uncomment the following line
# getSymbols("AAPL", src="yahoo")

# either way you can access the whole history using the ticker name
AAPL

# get ticker data for multiple stocks as zoo: 
#	Apple (AAPL)
#	Alphabet Inc. / Google (GOOG)
#	JPMorgan Chase & Co. (JPM)
#	Tesla Motors, Inc. (TSLA)
#	General Motors Company (GM)
ticker <- c("AAPL", "GOOG", "JPM", "TSLA", "GM")
getSymbols(ticker, src="yahoo", return.class="zoo")

# load library zoo
library(zoo)

# merge the adjusted close price for all stocks into one zoo
data.raw <- merge.zoo(Ad(AAPL), Ad(GOOG), Ad(JPM), Ad(TSLA), Ad(GM))

# load libraries PortfolioAnalytics and PerformanceAnalytics
library(PortfolioAnalytics)
library(PerformanceAnalytics)

# calculate arithmetic returns
data.arith <- Return.calculate(data.raw, method="simple")
# calculate logarithmic returns
data.log <- Return.calculate(data.raw, method="compound")

# filter data.arith for the range from 2010-11-17 to 2013-12-31 which is our in-sample period
data.arith.ins <- data.arith[ index(data.arith) >= as.Date("2010-11-17") & index(data.arith) <= as.Date("2013-12-31") ]

# load library timeSeries
library(timeSeries)
# convert zoo into timeSeries
data.arith.ins.ts <- as.timeSeries(data.arith.ins)

############################
####### OPTIMIZATION #######
############################
assets <- colnames(data.arith.ins)

portfolio.init <- portfolio.spec(assets)
portfolio.init <- add.constraint(portfolio.init, type = "full_investment")

# calculate minimum Std. Dev. portfolio
portfolio.minSD <- add.objective(portfolio = portfolio.init, type="risk", name="StdDev")
portfolio.minSD.opt <- optimize.portfolio(data.arith.ins.ts, portfolio = portfolio.minSD, optimize_method = "ROI", trace = TRUE)
portfolio.minSD.weights <- portfolio.minSD.opt$weights