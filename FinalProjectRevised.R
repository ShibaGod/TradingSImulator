#Setup----
##libraries ----
rm(list = ls())
library(tidyverse)
library(car)
library(tidyquant)
library(TTR)
library(dplyr)
library(scales)




#Working Directory ----
setwd("/Users/joeypark/Library/CloudStorage/GoogleDrive-joyp@andrew.cmu.edu/My Drive/2023 Spring/Econ and Data Sci/Final Project")


# Retrieves data from Yahoo Finance
ticker <- tq_get("SPY", from = "1990-01-01")

#Creates %Change Values
ticker <- ticker %>%
  mutate(`%ChangeClose` = (close / lag(close) - 1) * 100) %>%
  mutate(`High-Open` = high-open) %>%
  mutate(`Close-Low` = close-low)

#Creates 50-EMA values
ticker$ema50 <- EMA(ticker$close, n = 50)
#Creates 200-EMA values
ticker$ema200 <- EMA(ticker$close, n = 200)

#Creates 50-SMA values
ticker$sma50 <- SMA(ticker$close, n = 50)
#Creates 200-SMA values
ticker$sma200 <- SMA(ticker$close, n = 200)

#Creates 12-SMA values
ticker$ema12 <- EMA(ticker$close, n = 12)
#Creates 26-SMA values
ticker$ema26 <- EMA(ticker$close, n = 26)

#Mutates MACD values
ticker <- ticker %>%
  mutate("macd" = ema12 - ema26)

#Creates MACD Signal Line values
ticker$macdSignalLine <- EMA(ticker$macd, n = 9)








# Functions  --------

# Function to combine EMA crossover strategies
emaCrossOverCombined <- function() {
  # Calculates long EMA crossover values
  long_vals <- emaCrossOverLong()
  # Calculates short EMA crossover values
  short_vals <- emaCrossOverShort()
  # Combines long and short values and subtract initial cash
  combined_vals <- long_vals + short_vals - 100000 
  
  return(combined_vals)
}

# Function to combine SMA crossover strategies
smaCrossOverCombined <- function() {
  # Calculates long SMA crossover values
  long_vals <- smaCrossOverLong()
  # Calculates short SMA crossover values
  short_vals <- smaCrossOverShort()
  # Combines long and short values and subtract initial cash
  combined_vals <- long_vals + short_vals - 100000 
  
  return(combined_vals)
}

# Function to combine MACD crossover strategies
macdCrossOverCombined <- function() {
  # Calculates long MACD crossover values
  long_vals <- macdCrossOverLong()
  # Calculates short MACD crossover values
  short_vals <- macdCrossOverShort()
  # Combines long and short values and subtract initial cash
  combined_vals <- long_vals + short_vals - 100000 
  
  return(combined_vals)
}

# Function to implement long EMA crossover strategy
emaCrossOverLong <- function() {
  cash <- 100000  # initial cash
  position <- FALSE
  shares <- 0
  emaVals <- numeric(nrow(ticker))
  
  # Iterates through the ticker dataset
  # Starts at 200 as we need the 200 EMA which can only be calculated with the 200th day available
  for (i in 200:nrow(ticker)) {
    # Checks if 50-day EMA > 200-day EMA and not in a position
    if (ticker[i, "ema50"] > ticker[i, "ema200"] && position == FALSE) {
      position <- TRUE  # Enter a long position
      shares <- cash / as.numeric(ticker[i, "close"])
      cash <- 0
    }
    # Checks if 50-day EMA < 200-day EMA and in a position
    if (ticker[i, "ema50"] < ticker[i, "ema200"] && position == TRUE) {
      position <- FALSE  # Close the long position
      cash <- shares * as.numeric(ticker[i, "close"])
      shares <- 0
    }
    emaVals[i] <- cash + shares * as.numeric(ticker[i, "close"])
  }
  
  # Closes any open position at the end
  if (position == TRUE) {
    cash <- shares * as.numeric(tail(ticker, 1)["close"])
    shares <- 0
  }
  
  emaVals <- c(rep(NA, 199), emaVals[-(1:199)])
  return(emaVals)
}

# Function to implement short EMA crossover strategy
emaCrossOverShort <- function() {
  cash <- 100000  # initial cash
  position <- FALSE
  shares <- 0
  emaVals <- numeric(nrow(ticker))
  
  # Iterates through the dataset
  # Starts at 200 as we need the 200 EMA which can only be calculated with the 200th day available
  for (i in 200:nrow(ticker)) {
    # Checks if 50-day EMA < 200-day EMA and not in a position
    if (ticker[i, "ema50"] < ticker[i, "ema200"] && position == FALSE) {
      position <- TRUE  # Enter a short position
      shares <- cash / as.numeric(ticker[i, "close"])
      cash <- 0
    }
    # Checks if 50-day EMA > 200-day EMA and in a position
    if (ticker[i, "ema50"] > ticker[i, "ema200"] && position == TRUE) {
      position <- FALSE  # Close the short position
      cash <- shares * as.numeric(ticker[i, "close"])
      shares <- 0
    }
    emaVals[i] <- cash + shares * as.numeric(ticker[i, "close"])
  }
  
  # Closes open position if it exist at the end
  if (position == TRUE) {
    cash <- shares * as.numeric(tail(ticker, 1)["close"])
    shares <- 0
  }
  
  emaVals <- c(rep(NA, 199), emaVals[-(1:199)])
  return(emaVals)
}

# Function to implement long SMA crossover strategy
smaCrossOverLong <- function() {
  cash <- 100000  # initial cash
  position <- FALSE
  shares <- 0
  smaVals <- numeric(nrow(ticker))
  
  # Iterates through the dataset
  # Starts at 200 as we need the 200 SMA which can only be calculated with the 200th day available
  for (i in 200:nrow(ticker)) {
    # Checks if 50-day SMA > 200-day SMA and not in a position
    if (ticker[i, "sma50"] > ticker[i, "sma200"] && position == FALSE) {
      position <- TRUE  # Enter a long position
      shares <- cash / as.numeric(ticker[i, "close"])
      cash <- 0
    }
    # Checks if 50-day SMA < 200-day SMA and in a position
    if (ticker[i, "sma50"] < ticker[i, "sma200"] && position == TRUE) {
      position <- FALSE  # Close the long position
      cash <- shares * as.numeric(ticker[i, "close"])
      shares <- 0
    }
    smaVals[i] <- cash + shares * as.numeric(ticker[i, "close"])
  }
  
  # Closes open position if it exists at the end
  if (position == TRUE) {
    cash <- shares * as.numeric(tail(ticker, 1)["close"])
    shares <- 0
  }
  
  smaVals <- c(rep(NA, 199), smaVals[-(1:199)])
  return(smaVals)
}

# Function to implement short SMA crossover strategy
smaCrossOverShort <- function() {
  cash <- 100000  # initial cash
  position <- FALSE
  shares <- 0
  smaVals <- numeric(nrow(ticker))
  
  # Iterates through the dataset
  # Starts at 200 as we need the 200 SMA which can only be calculated with the 200th day available
  for (i in 200:nrow(ticker)) {
    # Checks if 50-day SMA < 200-day SMA and not in a position
    if (ticker[i, "sma50"] < ticker[i, "sma200"] && position == FALSE) {
      position <- TRUE  # Enter a short position
      shares <- cash / as.numeric(ticker[i, "close"])
      cash <- 0
    }
    # Checks if 50-day SMA > 200-day SMA and in a position
    if (ticker[i, "sma50"] > ticker[i, "sma200"] && position == TRUE) {
      position <- FALSE  # Close the short position
      cash <- shares * as.numeric(ticker[i, "close"])
      shares <- 0
    }
    smaVals[i] <- cash + shares * as.numeric(ticker[i, "close"])
  }
  
  # Closes open position if it exists at the end
  if (position == TRUE) {
    cash <- shares * as.numeric(tail(ticker, 1)["close"])
    shares <- 0
  }
  
  smaVals <- c(rep(NA, 199), smaVals[-(1:199)])
  return(smaVals)
}

# Function to implement long MACD crossover strategy
macdCrossOverLong <- function() {
  cash <- 100000  # initial cash
  position <- FALSE
  shares <- 0
  macdVals <- numeric(nrow(ticker))
  
  # Iterates through the dataset
  # Starts at 34 as we need a minimum of the 26 ema and 9 of these which gives number on the 34th day available
  for (i in 34:nrow(ticker)) {
    # Checks if MACD > MACD signal line and not in a position
    # Checks if MACD > MACD signal line and not in a position
    if (ticker[i, "macd"] > ticker[i, "macdSignalLine"] && position == FALSE) {
      position <- TRUE  # Enter a long position
      shares <- cash / as.numeric(ticker[i, "close"])
      cash <- 0
    }
    # Checks if MACD < MACD signal line and in a position
    if (ticker[i, "macd"] < ticker[i, "macdSignalLine"] && position == TRUE) {
      position <- FALSE  # Close the long position
      cash <- shares * as.numeric(ticker[i, "close"])
      shares <- 0
    }
    macdVals[i] <- cash + shares * as.numeric(ticker[i, "close"])
  }
  
  # Closes open position if it exists at the end
  if (position == TRUE) {
    cash <- shares * as.numeric(tail(ticker, 1)["close"])
    shares <- 0
  }
  
  macdVals <- c(rep(NA, 33), macdVals[-(1:33)])
  return(macdVals)
}

# Function to implement short MACD crossover strategy
macdCrossOverShort <- function() {
  cash <- 100000  # initial cash
  position <- FALSE
  shares <- 0
  macdVals <- numeric(nrow(ticker))
  
  # Iterates through the dataset
  # Starts at 34 as we need a minimum of the 26 ema and 9 of these which gives number on the 34th day available
  for (i in 34:nrow(ticker)) {
    # Checks if MACD < MACD signal line and not in a position
    if (ticker[i, "macd"] < ticker[i, "macdSignalLine"] && position == FALSE) {
      position <- TRUE  # Enter a short position
      shares <- cash / as.numeric(ticker[i, "close"])
      cash <- 0
    }
    # Checks if MACD > MACD signal line and in a position
    if (ticker[i, "macd"] > ticker[i, "macdSignalLine"] && position == TRUE) {
      position <- FALSE  # Close the short position
      cash <- shares * as.numeric(ticker[i, "close"])
      shares <- 0
    }
    macdVals[i] <- cash + shares * as.numeric(ticker[i, "close"])
  }
  
  # Closes open position if it exists at the end
  if (position == TRUE) {
    cash <- shares * as.numeric(tail(ticker, 1)["close"])
    shares <- 0
  }
  
  macdVals <- c(rep(NA, 33), macdVals[-(1:33)])
  return(macdVals)
}






# Data  --------
# Creates base number of shares for start of function
sharesLong = 100000/as.numeric(ticker[1,"close"])

# Adds values created by functions to dataset
ticker <- ticker %>% 
  mutate(ticker,longValues = sharesLong*close) %>%
  mutate(ticker, emaCrossValuesLong = emaCrossOverLong()) %>%
  mutate(ticker, smaCrossValuesLong = smaCrossOverLong()) %>%
  mutate(ticker, macdCrossValuesLong = macdCrossOverLong()) %>%
  mutate(ticker, emaCrossValuesShort = emaCrossOverShort()) %>%
  mutate(ticker, smaCrossValuesShort = smaCrossOverShort()) %>%
  mutate(ticker, macdCrossValuesShort = macdCrossOverShort()) %>%
  mutate(ticker, emaCrossValuesComb = emaCrossOverCombined()) %>%
  mutate(ticker, smaCrossValuesComb = smaCrossOverCombined()) %>%
  mutate(ticker, macdCrossValuesComb = macdCrossOverCombined())


# Graphical Plots --------

# EMA plot
# Creates a ggplot using ticker, sets x as date
ggplot(ticker, aes(x = date)) +
  # Adds a line for long values
  geom_line(aes(y = longValues, color = "Long Values")) +
  # Adds a line for EMA Long Strategy values
  geom_line(aes(y = emaCrossValuesLong, color = "EMA Long Strategy")) +
  # Adds a line for EMA Short Strategy values
  geom_line(aes(y = emaCrossValuesShort, color = "EMA Short Strategy")) +
  # Adds a line for EMA Combined Strategy values
  geom_line(aes(y = emaCrossValuesComb, color = "EMA Combined Strategy")) +
  # Sets plot title, x-axis label, y-axis label
  labs(title = "EMA Strategy vs Long Values",
       x = "Date",
       y = "Portfolio Value",
       color = "Strategy") +
  # Applies classic theme to plot
  theme_classic() +
  # Formats the y-axis labels as currency values
  scale_y_continuous(labels = dollar_format()) +
  # Makes graph wider
  theme(aspect.ratio = 0.3)



# SMA Plot
# Creates a ggplot using ticker, sets x as date
ggplot(ticker, aes(x = date)) +
  # Adds a line for long values
  geom_line(aes(y = longValues, color = "Long Values")) +
  # Adds a line for SMA Long Strategy values 
  geom_line(aes(y = smaCrossValuesLong, color = "SMA Long Values")) +
  # Adds a line for SMA Short Strategy values
  geom_line(aes(y = smaCrossValuesShort, color = "SMA Short Values")) +
  # Adds a line for SMA Combined Strategy values
  geom_line(aes(y = smaCrossValuesComb, color = "SMA Values")) +
  # Sets plot title, x-axis label, y-axis label
  labs(title = "SMA Strategy and Long Values",
       x = "Date",
       y = "Portfolio Value",
       color = "Strategy") +
  # Applies classic theme to plot
  theme_classic() +
  # Formats the y-axis labels as currency values
  scale_y_continuous(labels = dollar_format()) +
  # Makes graph wider
  theme(aspect.ratio = 0.3)



# MACD plot
# Creates a ggplot using ticker, sets x as date
ggplot(ticker, aes(x = date)) +
  # Adds a line for long values
  geom_line(aes(y = longValues, color = "Long Values")) +
  # Adds a line for MACD Long Strategy values
  geom_line(aes(y = macdCrossValuesLong, color = "MACD Long Strategy")) +
  # Adds a line for MACD Short Strategy values
  geom_line(aes(y = macdCrossValuesShort, color = "MACD Short Strategy")) +
  # Adds a line for MACD Combined Strategy values
  geom_line(aes(y = macdCrossValuesComb, color = "MACD Combined Strategy")) +
  # Sets plot title, x-axis label, y-axis label
  labs(title = "MACD Strategy vs Long Values",
       x = "Date",
       y = "Portfolio Value",
       color = "Strategy") +
  # Applies classic theme to plot
  theme_classic() +
  # Formats the y-axis labels as currency values
  scale_y_continuous(labels = dollar_format()) +
  # Makes graph wider
  theme(aspect.ratio = 0.3)


# Regression --------

# Models % Change of close Close by % Change in Volume
# Checks to see if % change in volume leads to a % change in close close
model <- lm(`%ChangeClose` ~ log(volume), data = ticker)
summary(model)

# Models High - Open by % Change in Volume
# Checks to see if % change in volume leads to a positive High - Open
model2 <- lm(`High-Open` ~ log(volume), data = ticker)
summary(model2)

# Models Close - Low by % Change in Volume
# Checks to see if % change in volume leads to a negative Close - Low
model3 <- lm(`Close-Low` ~ log(volume), data = ticker)
summary(model3)

# Models smaCrossValuesComb by longValues
model4 <- lm(smaCrossValuesComb ~ longValues, data = ticker)
summary(model4)

# Models emaCrossValuesComb by longValues
model5 <- lm(emaCrossValuesComb ~ longValues, data = ticker)
summary(model5)

# Models macdCrossValuesComb by longValues
model6 <- lm(macdCrossValuesComb ~ longValues, data = ticker)
summary(model6)


