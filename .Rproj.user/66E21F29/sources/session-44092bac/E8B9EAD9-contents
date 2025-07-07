
library(dplyr)
library(data.table)


dt  <- fread("Output/candles.csv")



dtLiquid <- dt %>%
  group_by(Pair)%>%
  mutate(
    volume = mean(`Trading volume in USDT`)
  )%>%
  filter(volume > 1e6)%>%
  select(
    Time,
    Pair,
    `Closing price`
  )%>%
  rename(
    Close = `Closing price`
  )%>%
  ungroup()%>%
  filter(
    endsWith(Pair, "USDC") == F,
    Pair != "NODEUSDT",
    endsWith(Pair, "USDE") == F
  )%>%
  data.table()



dt_wide <- dcast(
  dtLiquid,
  Time ~ Pair,
  value.var = "Close",
  fun.aggregate = mean
)



prices <- as.matrix(dt_wide[, -1])     # numeric matrix, drop Time
cor_mat <- cor(prices, use = "pairwise.complete.obs")
diag(cor_mat) <- 0                      # ignore self-corr


high_corr <- which(abs(cor_mat) >= 0.8, arr.ind = TRUE) %>%
  as_tibble() %>%
  filter(row < col) %>%                 # keep each pair once
  mutate(
    coinA = colnames(prices)[row],
    coinB = colnames(prices)[col],
    rho   = cor_mat[cbind(row, col)]
  )%>%
  arrange(desc(rho))%>%
  filter(rho != 1)

write.csv(high_corr,"highCorr.csv")
