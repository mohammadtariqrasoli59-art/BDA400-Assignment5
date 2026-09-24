# BDA400 Assignment 5 - Stochastic RSI (StochRSI)
# Name: Rasoli Mohammad
#
# The assignment pseudocode normalizes RSI using the overall minimum
# and maximum available RSI values, then applies SMA to create %K and %D.

rsi <- function(data, period) {
  if (length(data) < period + 1) {
    stop("Data length should be greater than or equal to period + 1")
  }

  if (period <= 0 || period != as.integer(period)) {
    stop("period must be a positive integer")
  }

  diff_values <- diff(data)
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }

  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values <- rep(NA_real_, length(data))

  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      if (avg_gain == 0) {
        rsi_values[i] <- 50
      } else {
        rsi_values[i] <- 100
      }
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}

sma <- function(data, period) {
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  sma_values <- numeric(length(data) - period + 1)

  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}

stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)

  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)

  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(rsi_values))
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  k_line <- sma(k_values, k_period)
  d_line <- sma(k_line, d_period)

  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}

# Example:
# data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
#           64, 61, 66, 70, 68, 72, 69, 73, 71, 75)
# print(stoch_rsi(data, period = 5, k_period = 3, d_period = 3))
