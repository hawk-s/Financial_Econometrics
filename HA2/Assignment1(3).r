library(quantmod)
library(rugarch)
library(dplyr)


setwd("C:/Users/cedav/OneDrive/Plocha/IES/Master/2.semestr/finecox/HA2")

set.seed(38848152)

data = read.csv("symbols2.csv")

symbols = as.vector(sample(data$Symbol, 100, replace = FALSE))

DownloadYahoo <- function(symbol, from, to = Sys.Date())
{
  # Package Check
  if (!requireNamespace("quantmod", quietly = TRUE)) 
  {
    stop("Package 'quantmod' must be installed. Use install.packages('quantmod').")
  }
  
  # Download data from yahoo finance
  tryCatch(
  {
    getSymbols(symbol, from = from, to = to, auto.assign = TRUE)
    data <- get(symbol)
    names(data) = unname(sapply(names(data), sub, pattern = ".*\\.", replacement = ""))
    return(data)
  }, error = function(e) 
  {
    message("Skipping due to error: ", e$message)
    return(NULL)
  })
}

from = as.Date('2020-01-01')
to = as.Date('2022-11-01')

dfs = list()
pricesList = list()

#names(prices) = symbols
for (i in 1 : length(symbols))
{
  df = DownloadYahoo(symbols[i], from, to)
  # Skip to the next iteration if df is NULL (= unsuccessful import)
  if (is.null(df)) next
  # Assign using double square brackets for list
  dfs[[i]] = df 
}
names(dfs) = symbols

# Create new List without the invalid symbols
for (symbol in symbols)
{
  if(!is.null(dfs[[symbol]]))
  {
    pricesList[[symbol]] = dfs[[symbol]]
  }
}
length(pricesList)
prices = data.frame("Date" = index(pricesList[[1]]))
# Create Data Frame of prices
for (validSymbol in names(pricesList))
{
  prices[validSymbol] = as.vector(pricesList[[validSymbol]]$Adjusted)
}

### 1)
# Compute Log Returns
logReturns = data.frame("Date" = prices$Date[2:length(prices$Date)])
for (validSymbol in names(pricesList))
{
  logReturns[validSymbol] = diff(log(as.vector(pricesList[[validSymbol]]$Adjusted)))
}
summary(logReturns)
# Delete the column with almost 500 NAs
logReturns = select(logReturns, -COL)
sum(is.na(logReturns))
validSymbols = colnames(logReturns)[2:ncol(logReturns)]

# We computed the logarithmic returns of each symbol in our collection, then we
# omitted all NAs in the new dataframe, since otherwise it would not be 
# compatible with the following processes. Due to this data cleaning we removed no row, since all NAs was included in the column of symbol "COL". In case of just a few NAs it would be more suitable to delete all rows containing NAs, but since all of them are in one column, we should delete it.

### 2)
# For each stock, estimate the parameters of a GARCH(1, 1) model:
garchModels = list()

for (validSymbol in validSymbols)
{
  spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                    distribution.model = "norm")
  garchModels[[validSymbol]] = ugarchfit(spec, logReturns[, validSymbol])
}

# We estimated the GARCH(1,1) model for each symbol of our dataframe using the 
# "rugarch" package.

### 3)
# Plotting Garch(1,1) coefs

# alpha_i represents the impact of the squared residuals (innovations) on the 
# conditional variance. A higher value of alpha_i indicates that the volatility
# is more sensitive to recent market shocks.
alphas = sapply(garchModels, function(model) coef(model)["alpha1"])

# beta_i represents the persistence of the conditional variance (the impact of past
# volatility on current volatility). A higher value of beta_i indicates that the
# volatility is more persistent and takes longer to dissipate.
betas = sapply(garchModels, function(model) coef(model)["beta1"])

# The sum alpha_i + beta_i measures the rate at which the response to volatility decays
# over time. A value close to 1 indicates a slower decay and more persistence
# in volatility.
alphasPlusBetas = alphas + betas

par(mfrow = c(1, 3))
hist(alphas, main = "Histogram of alphas", xlab = "Alpha")
hist(betas, main = "Histogram of betas", xlab = "Beta")
hist(alphasPlusBetas, main = "Histogram of alphas + betas", xlab = "Alpha + Beta")

### 4)
# Find the minimum and maximum values of alpha_i, beta_i, and alpha_i + beta_i. Comment briefly:
min_alpha = min(alphas)
max_alpha = max(alphas)
min_beta = min(betas)
max_beta = max(betas)
min_alphaPlusBeta = min(alphasPlusBetas)
max_alphaPlusBeta = max(alphasPlusBetas)

cat("Minimum alpha:", min_alpha, "\n")
# The value is very close to zero (practically zero).
# This indicates that for the stock with the lowest alpha, the impact of recent
# market shocks on volatility is very minimal.
cat("Maximum alpha:", max_alpha, "\n")
# The maximal alpha is relatively high.
# This suggests that for the stock with the highest alpha, the volatility is highly sensitive to recent market shocks, and new information or innovations have a significant impact on the conditional variance.
cat("Minimum beta:", min_beta, "\n")
# The minimal value of beta is relativelly low but still positive.
# This implies that for the stock with the lowest beta, the persistence of volatility is relatively low, and past volatility has a smaller impact on current volatility.
cat("Maximum beta:", max_beta, "\n")
# The maximal beta is almost 1.
# This indicates that for the stock with the highest beta, the volatility is highly persistent, and past volatility has a significant impact on current volatility. The conditional variance is heavily influenced by its own lagged values.
cat("Minimum alpha + beta:", min_alphaPlusBeta, "\n")
# The minimal value of the sum is relatively low.
# This suggests that for the stock with the lowest alpha + beta, the response to volatility decays relatively quickly over time, and the impact of past shocks on future volatility diminishes faster.

cat("Maximum alpha + beta:", max_alphaPlusBeta, "\n")
# Similarly to maximal beta, the maximal value of the sum is also almost 1.
# This indicates that for the stock with the highest alpha + beta, the response to volatility decays slowly over time, and the impact of past shocks on future volatility persists for a longer period. The conditional variance exhibits strong persistence.

### 5)
marketVolatility = data.frame("Date" = logReturns$Date)

for (i in 1:nrow(logReturns))
{
  dayVolatilities = sapply(garchModels, function(model) if (!is.null(model)) sigma(model)[i] else NA)
  marketVolatility$Median[i] = median(dayVolatilities, na.rm = TRUE)
  marketVolatility$Quantile95[i] = quantile(dayVolatilities, 0.95, na.rm = TRUE)
  marketVolatility$Quantile05[i] = quantile(dayVolatilities, 0.05, na.rm = TRUE)
}

plot(marketVolatility$Date, marketVolatility$Median, type = "l", xlab = "Date", ylab = "Market Volatility",
     main = "Market Volatility and Quantiles", ylim = range(marketVolatility[, 2:4], na.rm = TRUE))
lines(marketVolatility$Date, marketVolatility$Quantile95, col = "blue")
lines(marketVolatility$Date, marketVolatility$Quantile05, col = "red")
legend("topright", legend = c("Median", "95% Quantile", "5% Quantile"),
       col = c("black", "blue", "red"), lty = 1)
# We plotted the estimated market volatility and its "low" and "high" quantiles.
# In the figure we can see that the volatility appears to be most of the time 
# quite stable, although, there are also visible at least two high volatility 
# clusters - we might assume that the first cluster (the most significant one) 
# is likely to be impacted by the covid19 crisis hit. The explanation for the 
# second one does not seem to be apparent..
