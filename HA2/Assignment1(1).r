library(quantmod)
library(rugarch)


setwd("C:/Users/cedav/OneDrive/Plocha/IES/Master/2.semestr/finecox/HA2")

set.seed(38848152)

data = read.csv("symbols2.csv")

symbols = sample(data$Symbol, 100, replace = FALSE)

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
to = as.Date('2020-11-01')

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
logReturns = na.omit(logReturns)

### 2)
# For each stock, estimate the parameters of a GARCH(1, 1) model:
garchModels = list()

for (validSymbol in names(pricesList))
{
  spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                    distribution.model = "norm")
  garchModels[[validSymbol]] = ugarchfit(spec, logReturns[, validSymbol])
}

### 3)
# Plotting Garch(1,1) coefs
alphas = sapply(garchModels, function(model) coef(model)["alpha1"])
betas = sapply(garchModels, function(model) coef(model)["beta1"])
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
cat("Maximum alpha:", max_alpha, "\n")
cat("Minimum beta:", min_beta, "\n")
cat("Maximum beta:", max_beta, "\n")
cat("Minimum alpha + beta:", min_alphaPlusBeta, "\n")
cat("Maximum alpha + beta:", max_alphaPlusBeta, "\n")

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
