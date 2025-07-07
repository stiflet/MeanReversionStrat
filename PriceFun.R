
library(data.table)
library(xts)
library(urca)
library(roll)
library(PerformanceAnalytics)



missMatch <- function(df){
  
  setDT(df)
  if (nrow(df) < 40)           # or whatever you consider “enough”
    return(invisible(NULL))
  setorder(df, Date)       # now this sorts by reference
  
  midpoint  <- df$Date[round(nrow(df)/2)]
  
  form_set  <- df[Date <= midpoint]   # works: Date is a column
  trade_set <- df[Date >  midpoint]
  
  # 3 ── Engle-Granger cointegration test on formation window ────────────────────
  eg     <- lm(CoinB_Close ~ CoinA_Close, data = form_set)
  beta   <- coef(eg)[2]                        # hedge-ratio
  resid  <- eg$residuals
  adf    <- ur.df(resid, type = "none", lags = 1)
  # ── after computing `adf` ────────────────────────────────────────────────
  stat <- adf@teststat[1]
  crit <- adf@cval[1, "5pct"]
  
  if (stat >= crit) {
    'message("❌  No cointegration at 5% (ADF = ", round(stat, 3),
            ", crit = ", round(crit, 3), ").  Skipping this window.")'
    return(invisible(NULL))          # or return NA metrics, or a flag list…
  }
  
  
  # 4 ── Spread & rolling z-score in trading window ──────────────────────────────
  spread <- trade_set$CoinB_Close - beta * trade_set$CoinA_Close
  roll_m <- roll_mean(spread, 30, complete_obs = FALSE)
  roll_s <- roll_sd(  spread, 30, complete_obs = FALSE)
  zscore <- (spread - roll_m) / roll_s
  
  # 5 ── Trading loop ────────────────────────────────────────────────────────────
  state       <- "flat"
  entry_A     <- entry_B <- NA_real_
  cashflows   <- numeric()                    # trade P&L
  cf_dates    <- as.POSIXct(character())      # closing dates
  
  for (i in seq_along(zscore)) {
    z <- zscore[i]
    if (is.na(z)) next
    
    priceA <- trade_set$CoinA_Close[i]
    priceB <- trade_set$CoinB_Close[i]
    
    ## open positions
    if (state == "flat" && z >  1.5) {
      state <- "short_spread";  entry_A <- priceA; entry_B <- priceB
    } else if (state == "flat" && z < -1.5) {
      state <- "long_spread";   entry_A <- priceA; entry_B <- priceB
    }
    
    ## close positions
    if (state == "short_spread" && z < 0) {
      pnl        <- (entry_B - priceB) + (priceA - entry_A)
      cashflows  <- c(cashflows, pnl)
      cf_dates   <- c(cf_dates, trade_set$Date[i])
      state <- "flat"
    } else if (state == "long_spread" && z > 0) {
      pnl        <- (priceB - entry_B) + (entry_A - priceA)
      cashflows  <- c(cashflows, pnl)
      cf_dates   <- c(cf_dates, trade_set$Date[i])
      state <- "flat"
    }
  }
  
  ## force-close if still in a trade at window end
  if (state != "flat") {
    priceA <- tail(trade_set$CoinA_Close, 1)
    priceB <- tail(trade_set$CoinB_Close, 1)
    lastDt <- tail(trade_set$Date, 1)
    pnl    <- if (state == "short_spread") {
      (entry_B - priceB) + (priceA - entry_A)
    } else {
      (priceB - entry_B) + (entry_A - priceA)
    }
    cashflows <- c(cashflows, pnl)
    cf_dates  <- c(cf_dates, lastDt)
  }
  
  #cat("Total $ return over trading window:", sum(cashflows), "\n")
  
  # 6 ── Risk-adjusted metrics (annualised) ──────────────────────────────────────
  'ret_xts  <- xts(cashflows, order.by = cf_dates)   # time-indexed trade P&L
  annual   <- sqrt(12)                              # monthly-to-annual factor
  
  Sharpe   <- SharpeRatio(ret_xts, FUN = "StdDev") * annual
  Sortino  <- SortinoRatio(ret_xts)               * annual
  DownRisk <- DownsideDeviation(ret_xts)'
  
  if (length(cashflows) == 0 || length(cf_dates) == 0 || any(is.na(cashflows))) {
    return(data.frame(
      CoinA = df$CoinA[1],
      CoinB = df$CoinB[1],
      z_last = NA_real_
    ))
  }
  
  ret_xts <- tryCatch({
    xts(cashflows, order.by = cf_dates)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(ret_xts) || nrow(ret_xts) == 0) {
    return(data.frame(
      CoinA = df$CoinA[1],
      CoinB = df$CoinB[1],
      z_last = NA_real_
    ))
  }
  
  
  # compute z-score of the *last* observation in the back-test window
  z_last <- tail(zscore, 1)
  'cat("Latest z-score =", round(z_last, 2),
      if (abs(z_last) > 1.5) "→ PRICE MISMATCH!" else "→ within fair-value band", "\n")'
  
  
  results <- data.frame(
    CoinA = df$CoinA[1],
    CoinB = df$CoinB[1],
    z_last = z_last
  )
  
  
  return(results)
  
  
}



corrPairs <- apply(high_corr, 1, function(row){
  
  coinA <- row[3]
  coinB <- row[4]
  
  corNames <- c(coinA, coinB)
  
  A <- dfA %>%
    filter(Pair == coinA)
  
  B <- dfB %>%
    filter(Pair == coinB)
  
  
  df.final <- inner_join(A, B, by = 'Date', suffix = c('A','B'), relationship = "many-to-many")%>%
    distinct(Date, .keep_all = TRUE)%>%
    rename(
      CoinA_Close = Closing.priceA,
      CoinB_Close = Closing.priceB,
      CoinA = PairA,
      CoinB = PairB
    )
  
  return(df.final)
  
})


zValues <- lapply(corrPairs, function(df){
  missMatch(df)
  })%>%
  list_rbind()%>%
  na.omit%>%
  arrange(desc(abs(z_last)))

corrpairs_df <- list_rbind(corrPairs)

infos <- apply(zValues, 1, function(row){
  
  coinA <- row[1]
  coinB <- row[2]
  
  CoinA_Close <- dfA %>%
    filter(Pair == coinA)%>%
    select(Closing.price)%>%
    c
  
  CoinB_Close <- dfB %>%
    filter(Pair == coinB)%>%
    select(Closing.price)%>%
    c
  
  
  CoinA_Close <- CoinA_Close[[1]]
  CoinB_Close <- CoinB_Close[[1]]
  
  
  lenB <- length(CoinB_Close)
  lenA <- length(CoinA_Close)
  
  if(lenA != lenB){
    minlen <- min(c(lenA, lenB))
    
    CoinA_Close <- CoinA_Close[1:minlen]
    CoinB_Close <- CoinB_Close[1:minlen]
  }
  
  
  beta <- coef(lm(log(CoinB_Close) ~ log(CoinA_Close)))[[2]]
  
  
  PA_now <- last(CoinA_Close)
  PB_now <- last(CoinB_Close)

  N_short = 10
  
  if(as.double(row[[3]]) < 0){
    
    qty_B <-  N_short / PB_now
    qty_A <-  beta * N_short / PA_now
    
  }
  if(as.double(row[[3]]) > 0){
    qty_A <-  N_short / PA_now
    qty_B <-  beta * N_short / PB_now 
  }
  
  
  
  
  df <- data.frame(
    #AmountA = qty_A,
    #AmountB = qty_B,
    lastCloseA = PA_now,
    lastCloseB = PB_now
    
  )
  
  return(df)  
})%>%
  list_rbind()



dfZValues <- cbind(zValues, infos)
