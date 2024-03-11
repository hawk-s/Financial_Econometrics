
symbols_data <- read.csv("symbols.csv", header = TRUE, sep = ';')


#filter the Consumer Discretionary sector
consumer_discretionary <- subset(symbols_data, Sector == 'Consumer Discretionary')

print(consumer_discretionary)

#get symbols
symbols <- consumer_discretionary$ď.żSymbol

print(symbols)
#time period
start_date <- as.Date("2015-07-01")
end_date <- as.Date("2023-12-31")




#install.packages('quantmod')






library('quantmod')

#'data' empty list
data <- list()


for (symbol in symbols) {
  print(symbol) #CHECK: PRINT SYMBOL BEING PROCESSED
  result <- tryCatch({
    getSymbols(symbol, auto.assign = FALSE, from = start_date, to = end_date)
  }, error = function(e) {
    cat("Error fetching data for", symbol, ": ", e$message, "\n")
    NULL #Return NULL when error
  })
  if (!is.null(result)) {
    data[[symbol]] <- result
  }
}




#check if we have the desired symbols
print(names(data))



#check the data
lapply(data, head)





#'lrets' and 'rets' empty lists to store log-returns and simple returns
lrets <- list()
rets <- list()

#lapply(data, head)

#loop through each symbol in data list:
for(symbol in names(data)) {
  #extract the Adjusted closing prices:
  prices <- Cl(data[[symbol]]) 
  
  #log-returns:
  log_returns <- diff(log(prices))
  lrets[[symbol]] <- log_returns
  
  #simple returns:
  simple_returns <- diff(prices) / lag(prices, k = 1)
  rets[[symbol]] <- simple_returns
}

#'lrets' and 'rets' contain the log-returns and simple returns for each stock




#print(head(lrets))
lapply(lrets, head)






stats_list <- list()

#function to calculate skewness
calc_skewness <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  skewness <- (n / ((n - 1) * (n - 2))) * sum(((x - mean_x) / s)^3, na.rm = TRUE)
  return(skewness)
}

#excess kurtosis
calc_excess_kurtosis <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  kurtosis <- (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * sum(((x - mean_x) / s)^4, na.rm = TRUE) - (3 * (n - 1)^2 / ((n - 2) * (n - 3)))
  return(kurtosis)
}

#loop through each symbol in lrets
for(symbol in names(lrets)) {
  #calculate statistics:
  mean_val <- mean(lrets[[symbol]], na.rm = TRUE)
  variance_val <- var(lrets[[symbol]], na.rm = TRUE)
  skewness_val <- calc_skewness(lrets[[symbol]])
  kurtosis_val <- calc_excess_kurtosis(lrets[[symbol]])
  min_val <- min(lrets[[symbol]], na.rm = TRUE)
  max_val <- max(lrets[[symbol]], na.rm = TRUE)
  
  #store the statistics in the list:
  stats_list[[symbol]] <- c(Mean = mean_val, Variance = variance_val, Skewness = skewness_val, ExcessKurtosis = kurtosis_val, Minimum = min_val, Maximum = max_val)
}

#nicely readable format():
stats_df <- do.call(rbind, lapply(stats_list, function(x) as.data.frame(t(x))))
rownames(stats_df) <- names(stats_list)


print(stats_df)







rws <- do.call(cbind, lrets)
options(repr.plot.width = 10, repr.plot.height = 8)

#plot the first time series to set up the plot
plot(ts(rws[, 1]), ylim = c(min(rws, na.rm = TRUE), max(rws, na.rm = TRUE)), ylab = 'Log Returns', xlab = 'Time', type = 'l')

#add the other time series
for(j in 2:ncol(rws)) {
  lines(ts(rws[, j]), col = colors()[j])
}

legend("topright", legend = names(lrets), col = colors()[2:(ncol(rws) + 1)], lty = 1)








options(repr.plot.width = 12, repr.plot.height = 12)
par(mfrow = c(2, 3))
sapply(lrets, function(y){
    plot(as.Date(index(y)), y, type = 'l', xlab = 'Year',
     ylab = 'returns')
})







# Assuming you have a list of data frames called lrets, where each data frame represents a symbol
# For example:
# lrets <- list(ANF = data.frame(Date = as.Date(c("2015-07-01", "2015-07-02")), ANF.Close = c(NA, -0.023398352)),
#               AMZN = data.frame(Date = as.Date(c("2015-07-01", "2015-07-02")), AMZN.Close = c(NA, 0.0007313792)))

# Find the symbol with the most observations to establish the benchmark sample period
max_length <- max(sapply(lrets, nrow))

# Function to calculate the percentage of non-missing observations
percentage_valid_data <- function(df) {
  sum(!is.na(df)) / max_length * 100
}

# Apply the function to each symbol and filter out those below the 80% threshold
valid_symbols <- sapply(lrets, function(df) percentage_valid_data(df) >= 80)

# Keep only the symbols that meet the criterion
lrets_filtered <- lrets[valid_symbols]

print(lrets_filtered)
# Now lrets_filtered contains only the symbols with valid data for at least 80% of the dates







# Ensure each dataframe in the list has a date column
lrets_filtered_with_dates <- lapply(lrets_filtered, function(df) {
  if (!"date" %in% colnames(df)) { # Check if 'date' column doesn't exist
    df$date <- rownames(df) # Assuming the row names are dates
    rownames(df) <- NULL # Remove row names to avoid confusion
  }
  df
})

# Define a custom merge function for two dataframes by date
merge_by_date <- function(x, y) {
  merge(x, y, by = "date", all = FALSE) # 'all = FALSE' ensures an inner join
}

# Use Reduce to iteratively apply the merge function across all dataframes in the list
merged_data <- Reduce(merge_by_date, lrets_filtered_with_dates)

# View the top of the merged dataframe
head(merged_data)

