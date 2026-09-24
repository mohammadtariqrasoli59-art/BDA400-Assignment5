# BDA400 Assignment 5 - Crossover
# Name: Rasoli Mohammad

crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  if (length(arr1) == 0) {
    return(character(0))
  }

  crossover_signals <- rep("None", length(arr1))

  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}

# Example:
# arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
# print(crossover(arr1, arr2))
