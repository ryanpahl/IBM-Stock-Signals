library(quantmod)

# --- Load IBM data ---
getSymbols("IBM", from = "2020-01-01", to = Sys.Date())

# --- Calculate indicators ---
IBM$EMA9 <- EMA(Cl(IBM), n = 9)
macd_vals <- MACD(Cl(IBM), nFast = 12, nSlow = 26, nSig = 9, maType = EMA)
IBM$MACD   <- macd_vals[, 1]
IBM$Signal <- macd_vals[, 2]
IBM$RSI    <- RSI(Cl(IBM), n = 14)
IBM$SignalType <- NA_character_

# --- Define robust cross functions ---
crossOver <- function(x, y) {
  res <- (Lag(x) < Lag(y)) & (x > y)
  res[is.na(res)] <- FALSE
  return(res)
}
crossUnder <- function(x, y) {
  res <- (Lag(x) > Lag(y)) & (x < y)
  res[is.na(res)] <- FALSE
  return(res)
}

# --- Compute crossover signals ---
macd_cross_over  <- crossOver(IBM$MACD, IBM$Signal)
macd_cross_under <- crossUnder(IBM$MACD, IBM$Signal)

# Replace any remaining NAs with FALSE (critical fix)
macd_cross_over[is.na(macd_cross_over)]   <- FALSE
macd_cross_under[is.na(macd_cross_under)] <- FALSE

# --- Generate Buy/Sell signals ---
for(i in 2:NROW(IBM)) {
  if(any(is.na(c(IBM$MACD[i], IBM$Signal[i], IBM$EMA9[i], IBM$RSI[i], Cl(IBM)[i])))) next
  
  if(macd_cross_over[i] && (Cl(IBM)[i] > IBM$EMA9[i]) && (IBM$RSI[i] < 70)) {
    IBM$SignalType[i] <- "Buy"
  } else if(macd_cross_under[i] && (Cl(IBM)[i] < IBM$EMA9[i]) && (IBM$RSI[i] > 30)) {
    IBM$SignalType[i] <- "Sell"
  }
}

# --- Prepare chart data ---
IBM_ohlc <- IBM[, c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Volume", "IBM.Adjusted")]
IBM_ohlc <- as.xts(apply(IBM_ohlc, 2, function(x) as.numeric(as.character(x))),
                   order.by = index(IBM_ohlc))
ema9_xts <- EMA(Cl(IBM), n = 9)
dev.off()   # closes any open or corrupted graphics device
# plot -> zoom
# --- Plot the main price chart ---
chartSeries(IBM_ohlc,
            name = "IBM Price with EMA9 and Buy/Sell Signals",
            theme = chartTheme("white"))

# --- Overlay EMA(9) ---
addTA(ema9_xts, on = 1, col = "blue", lwd = 2)

close_prices <- as.numeric(coredata(Cl(IBM)))
close_xts <- xts(close_prices, order.by = index(Cl(IBM)))

# Now create buy_signal with numeric vector inside xts
buy_signal <- ifelse(IBM$SignalType == "Buy", close_xts * 0.98, NA)

# Similarly for sell_signal
sell_signal <- ifelse(IBM$SignalType == "Sell", close_xts * 1.02, NA)

addTA(xts(buy_signal,  order.by = index(IBM)), on = 1,
      col = "green", type = "p", pch = 24, cex = 1.5)
addTA(xts(sell_signal, order.by = index(IBM)), on = 1,
      col = "red",   type = "p", pch = 25, cex = 1.5)

