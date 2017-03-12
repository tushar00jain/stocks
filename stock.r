
install.packages('quantmod')

library('quantmod')

# returns monthly adjusted closing stock price data from a given list of stock symbols
monthly <- function (symbols) {
    # fetch stock prices from yahoo finance using quantmod
    fetch <- sapply(symbols, function(x) {
        getSymbols(x, src="yahoo", from="2000-01-01", to="2017-01-01", auto.assign=FALSE)
    })

    # convert the data to monthly
    data <- lapply(fetch, function(x) {
        to.monthly(x)
    })
    
    # extract the adjust closing price from the data
    adjusted <- lapply(data, function(x) {
        temp <- x[, 'x.Adjusted']
        names(temp) <- 'Adjusted'
        return(temp)
    })
    
    return(adjusted)
}

# stock symbols
symbols <- c('^hsi', '0005.hk', '0006.hk', '0322.hk', '0001.hk', '0293.hk', '0941.hk')

# create adjusted closing price zoo object
adjusted <- monthly(symbols)
df.adjusted <- do.call(cbind, adjusted)
names(df.adjusted) <- symbols

# create zoo object for storing stock returns
returns <- lapply(adjusted, function(x) {
    diff(x[, 'Adjusted']) / lag(x[, 'Adjusted'] , 1)
})

df.returns <- do.call(cbind, returns)
rsymbols <- sapply(symbols, function(x) {
    paste('r', x, sep='')
})
names(df.returns) <- rsymbols

# merge the adjusted stock prices with stock returns data
df <- merge(df.adjusted, df.returns)

# read base rate and compute the monthly risk free rate
base <- read.csv('base.csv')
df$base <- base[, 'base']
rf <- (base[, 'base']/100) / 12

names(df) <- c(symbols, rsymbols, 'base')

# compute the difference between risk free rate and stock returns

er <- apply(df.returns, 2, function(x) {
    x - rf
})

ersymbols <- sapply(symbols, function(x) {
    paste('er', x, sep='')
})

df.er <- zoo(er, order.by=index(df.returns))
names(df.er) <- ersymbols

# merge the excess / shortage in returns with the adjusted closing prices and stock returns zoo object
master <- merge(df, df.er)

# write the data frames for future use
write.zoo(master, 'data.csv', index.name='date', sep=',')
write.zoo(df.er, 'er.csv', index.name='date', sep=',')

# data frames for CAPM to run OLS
mtft <- df.er[-1, 1]
jtft <- df.er[-1, -1]
