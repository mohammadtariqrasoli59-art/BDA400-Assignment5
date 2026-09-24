# BDA400 Assignment 5 - Test Script
# Name: Rasoli Mohammad
#
# Run this file from the same folder as the nine indicator scripts.
# It checks basic expected behavior and input validation.

source("01_sma.R")
source("02_ema.R")
source("03_macd.R")
source("04_stdev.R")
source("05_linreg.R")
source("06_rsi.R")
source("07_stoch_rsi.R")
source("08_crossover.R")
source("09_crossunder.R")

cat("BDA400 Assignment 5 - Basic Tests\n\n")

# SMA
x <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
sma_result <- sma(x, 3)
stopifnot(length(sma_result) == 7)
stopifnot(abs(sma_result[1] - 12.3333333) < 1e-6)
cat("SMA: PASS\n")

# EMA
ema_result <- ema(x, 3)
stopifnot(length(ema_result) == length(x))
stopifnot(abs(ema_result[1] - x[1]) < 1e-10)
cat("EMA: PASS\n")

# MACD
macd_result <- macd(x, 3, 5, 2)
stopifnot(length(macd_result$macd_line) == length(x))
stopifnot(length(macd_result$signal_line) == length(x))
stopifnot(length(macd_result$histogram) == length(x))
cat("MACD: PASS\n")

# Standard deviation (population)
sd_result <- stdev(x)
expected_sd <- sqrt(sum((x - mean(x))^2) / length(x))
stopifnot(abs(sd_result - expected_sd) < 1e-10)
cat("Standard Deviation: PASS\n")

# Linear regression
lr_result <- linreg(x, regressionLength = 5, regressionOffset = 0)
stopifnot(is.numeric(lr_result$slope))
stopifnot(is.numeric(lr_result$intercept))
stopifnot(length(lr_result$predicted_values) == 5)
cat("Linear Regression: PASS\n")

# RSI
rsi_result <- rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), 5)
stopifnot(length(rsi_result) == 10)
stopifnot(all(is.na(rsi_result[1:5])))
stopifnot(all(rsi_result[6:10] >= 0 & rsi_result[6:10] <= 100))
cat("RSI: PASS\n")

# StochRSI
stoch_result <- stoch_rsi(
  c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
    64, 61, 66, 70, 68, 72, 69, 73, 71, 75),
  period = 5,
  k_period = 3,
  d_period = 3
)
stopifnot(is.list(stoch_result))
stopifnot("k_line" %in% names(stoch_result))
stopifnot("d_line" %in% names(stoch_result))
cat("Stochastic RSI: PASS\n")

# Crossover
a <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
b <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
cross_result <- crossover(a, b)
stopifnot(cross_result[4] == "Up")
cat("Crossover: PASS\n")

# Crossunder
crossunder_result <- crossunder(a, b)
stopifnot(crossunder_result[8] == "False")
cat("Crossunder: PASS\n")

# Input validation
error_found <- FALSE
tryCatch({
  sma(c(1, 2), 3)
}, error = function(e) {
  error_found <<- TRUE
})
stopifnot(error_found)
cat("Input validation: PASS\n")

cat("\nAll basic tests completed successfully.\n")
