xgbtestml <- length(eurmlxgpred)
xgbstart <- as.Date("2019-01-30")
xgbseq <- seq(from = xgbstart, by = "month", length.out = xgbtestml)
pred_returns_mlxgb <- data.frame(
  date = xgbseq,
  EUR = eurmlxgpred,
  JPY = jpymlxgpred,
  SEK = sekmlxgpred,
  NOK = nokmlxgpred,
  AUD = audmlxgpred,
  CHF = chfmlxgpred,
  NZD = nzdmlxgpred,
  GBP = gbpmlxgpred,
  CAD = cadmlxgpred
)

pred_returns_mlxgb

xgbtestmlac <- length(actualeurmlxg)
xgbstartac <- as.Date("2019-01-30")
xgbseqac <- seq(from = xgbstartac, by = "month", length.out = xgbtestmlac)
pred_returns_mlxgbac <- data.frame(
  date = xgbseqac,
  EUR = actualeurmlxg,
  JPY = actualjpymlxg,
  SEK = actualsekmlxg,
  NOK = actualnokmlxg,
  AUD = actualaudmlxg,
  CHF = actualchfmlxg,
  NZD = actualnzdmlxg,
  GBP = actualgbpmlxg,
  CAD = actualcadmlxg
)

actual_trainrfml <- data.frame(
  EUR = actualeurmlxgin,
  JPY = actualjpymlxgin,
  SEK = actualsekmlxgin,
  NOK = actualnokmlxgin,
  AUD = actualaudmlxgin,
  CHF = actualchfmlxgin,
  NZD = actualnzdmlxgin,
  GBP = actualgbpmlxgin,
  CAD = actualcadmlxgin
)

library(xts)
library(PerformanceAnalytics)

# 1. Prepare predicted and actual returns (convert log to simple returns)
pred_simple_xgb <- exp(pred_returns_mlxgb[,-1]) - 1
pred_simple_xgb <- cbind(date = pred_returns_mlxgb$date, pred_simple_xgb)

actual_simple_xgb <- exp(pred_returns_mlxgbac[,-1]) - 1
actual_simple_xgb <- cbind(date = pred_returns_mlxgbac$date, actual_simple_xgb)

# 2. Create tertile portfolio weights
get_tertile_weights <- function(row) {
  ranks <- rank(row, ties.method = "first")
  weights <- rep(0, length(row))
  weights[ranks >= 7] <- 1/3    # Top 3: long
  weights[ranks <= 3] <- -1/3   # Bottom 3: short
  return(weights)
}

weights_matrix_xgb <- t(apply(pred_simple_xgb[,-1], 1, get_tertile_weights))
colnames(weights_matrix_xgb) <- colnames(pred_simple_xgb)[-1]

# 3. Convert to xts objects
weights_xts_xgb <- xts(weights_matrix_xgb, order.by = as.Date(pred_simple_xgb$date))
actual_returns_xts_xgb <- xts(actual_simple_xgb[,-1], order.by = as.Date(actual_simple_xgb$date))

# 4. Calculate portfolio returns
portfolio_returns_xgb <- Return.portfolio(
  R = actual_returns_xts_xgb,
  weights = weights_xts_xgb,
  rebalance_on = "months",
  verbose = TRUE
)

charts.PerformanceSummary(portfolio_returns_xgb$returns, main = "Tertile Long-Short Portfolio (XGBoost)")
sharpe_ratio_xgb <- SharpeRatio.annualized(portfolio_returns_xgb$returns, Rf = 0)
cat("Annualized Sharpe Ratio:", round(sharpe_ratio_xgb, 3), "\n")
print(table.AnnualizedReturns(portfolio_returns_xgb$returns))
print(table.Stats(portfolio_returns_xgb$returns))

max_dd_xgb <- maxDrawdown(portfolio_returns_xgb$returns, geometric = TRUE, invert = TRUE)
cat("Maximum Drawdown:", round(max_dd_xgb, 4), "\n")

log_returns_xgb <- log(1 + portfolio_returns_xgb$returns)
cumulative_log_returns_xgb <- cumsum(log_returns_xgb)
print(cumulative_log_returns_xgb)

plot(cumulative_log_returns_xgb, main = "Cumulative Log Returns (XGBoost Portfolio)")

ttxg <- seq(from = as.Date("2004-12-31"), by = "month", length.out = nrow(actual_trainrfml))
training_weights_matrix <- t(apply(actual_trainrfml, 1, get_tertile_weights))
colnames(training_weights_matrix) <- colnames(actual_trainrfml)
training_weights_xts <- xts(training_weights_matrix, order.by = ttxg)
training_returns_xts <- xts(actual_trainrfml, order.by = ttxg)
training_portfolio_returns_xts <- Return.portfolio(
  R = training_returns_xts,
  weights = training_weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
training_portfolio_returns <- as.numeric(training_portfolio_returns_xts$returns)

actual_returns_xts <- xts(actual_simple_xgb[,-1], order.by = as.Date(actual_simple_xgb$date))
portfolio_returns_xts <- Return.portfolio(
  R = actual_returns_xts,
  weights = weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
actual_portfolio_returns <- as.numeric(portfolio_returns_xts$returns)

predicted_returns_xts <- xts(pred_simple_xgb[,-1], order.by = as.Date(pred_simple_xgb$date))
predicted_portfolio_returns_xts <- Return.portfolio(
  R = predicted_returns_xts,
  weights = weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
predicted_portfolio_returns <- as.numeric(predicted_portfolio_returns_xts$returns)

train_mean <- mean(training_portfolio_returns, na.rm = TRUE)
mspe_model <- mean((actual_portfolio_returns - predicted_portfolio_returns)^2, na.rm = TRUE)
mspe_naive <- mean((actual_portfolio_returns - train_mean)^2, na.rm = TRUE)
r2_oos <- 1 - mspe_model / mspe_naive
cat("OOS R^2 for portfolio:", round(r2_oos, 4), "\n")

actual_vec <- actual_portfolio_returns
predicted_vec <- predicted_portfolio_returns

# Directional accuracy: fraction of times the signs agree
directional_accuracy <- mean(sign(actual_vec) == sign(predicted_vec), na.rm = TRUE)

# Output result
cat("Directional Accuracy:", round(directional_accuracy, 4), "\n")


# portfolio 2 xgboost mac
xgbtestmlmac <- length(eurmacxgpred)
xgbstartmac <- as.Date("2019-01-30")
xgbseqmac <- seq(from = xgbstartmac, by = "month", length.out = xgbtestmlmac)
pred_returns_mlxgbmac <- data.frame(
  date = xgbseqmac,
  EUR = eurmacxgpred,
  JPY = jpymacxgpred,
  SEK = sekmacxgpred,
  NOK = nokmacxgpred,
  AUD = audmacxgpred,
  CHF = chfmacxgpred,
  NZD = nzdmacxgpred,
  GBP = gbpmacxgpred,
  CAD = cadmacxgpred
)

xgbtestmlmacac <- length(actualeurmacxg)  
xgbstartmacac <- as.Date("2019-01-30")
xgbseqmacac <- seq(from = xgbstartmacac, by = "month", length.out = xgbtestmlmacac)
pred_returns_mlxgbmacac <- data.frame(
  date = xgbseqmacac,
  EUR = actualeurmacxg,
  JPY = actualjpymacxg,
  SEK = actualsekmacxg,
  NOK = actualnokmacxg,
  AUD = actualaudmacxg,
  CHF = actualchfmacxg,
  NZD = actualnzdmacxg,
  GBP = actualgbpmacxg,
  CAD = actualcadmacxg
)

trianactxgmac <- data.frame(
  EUR = actualeurmacxin,
  JPY = actualjpymacxin,
  SEK = actualsekmacxin,
  NOK = actualnokmacxin,
  AUD = actualaudmacxin,
  CHF = actualchfmacxin,
  NZD = actualnzdmacxin,
  GBP = actualgbpmacxin,
  CAD = actualcadmacxin
)




# 1. Prepare predicted and actual returns (convert log to simple returns)
pred_simple_xgb_mac <- exp(pred_returns_mlxgbmac[,-1]) - 1
pred_simple_xgb_mac <- cbind(date = pred_returns_mlxgbmac$date, pred_simple_xgb_mac)
actual_simple_xgb_mac <- exp(pred_returns_mlxgbmacac[,-1]) - 1
actual_simple_xgb_mac <- cbind(date = pred_returns_mlxgbmacac$date, actual_simple_xgb_mac)
# 2. Create tertile portfolio weights
get_tertile_weights_mac <- function(row) {
  ranks <- rank(row, ties.method = "first")
  weights <- rep(0, length(row))
  weights[ranks >= 7] <- 1/3    # Top 3: long
  weights[ranks <= 3] <- -1/3   # Bottom 3: short
  return(weights)
}
weights_matrix_xgb_mac <- t(apply(pred_simple_xgb_mac[,-1], 1, get_tertile_weights_mac))
colnames(weights_matrix_xgb_mac) <- colnames(pred_simple_xgb_mac)[-1]
# 3. Convert to xts objects
weights_xts_xgb_mac <- xts(weights_matrix_xgb_mac, order.by = as.Date(pred_simple_xgb_mac$date))
actual_returns_xts_xgb_mac <- xts(actual_simple_xgb_mac[,-1], order.by = as.Date(actual_simple_xgb_mac$date))
# 4. Calculate portfolio returns
portfolio_returns_xgb_mac <- Return.portfolio(
  R = actual_returns_xts_xgb_mac,
  weights = weights_xts_xgb_mac,
  rebalance_on = "months",
  verbose = TRUE
)
charts.PerformanceSummary(portfolio_returns_xgb_mac$returns, main = "Tertile Long-Short Portfolio (XGBoost Macro)")
sharpe_ratio_xgb_mac <- SharpeRatio.annualized(portfolio_returns_xgb_mac$returns, Rf = 0)
cat("Annualized Sharpe Ratio (Macro):", round(sharpe_ratio_xgb_mac, 3), "\n")
print(table.AnnualizedReturns(portfolio_returns_xgb_mac$returns))
print(table.Stats(portfolio_returns_xgb_mac$returns))
max_dd_xgb_mac <- maxDrawdown(portfolio_returns_xgb_mac$returns, geometric = TRUE, invert = TRUE)
cat("Maximum Drawdown (Macro):", round(max_dd_xgb_mac, 4), "\n")
log_returns_xgb_mac <- log(1 + portfolio_returns_xgb_mac$returns)
cumulative_log_returns_xgb_mac <- cumsum(log_returns_xgb_mac)
print(cumulative_log_returns_xgb_mac)
plot(cumulative_log_returns_xgb_mac, main = "Cumulative Log Returns (XGBoost Macro Portfolio)")


ttxgmc <- seq(from = as.Date("2004-12-31"), by = "month", length.out = nrow(trianactxgmac))
training_weights_matrix <- t(apply(trianactxgmac, 1, get_tertile_weights))
colnames(training_weights_matrix) <- colnames(trianactxgmac)
training_weights_xts <- xts(training_weights_matrix, order.by = ttxgmc)
training_returns_xts <- xts(trianactxgmac, order.by = ttxgmc)
training_portfolio_returns_xts <- Return.portfolio(
  R = training_returns_xts,
  weights = training_weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
training_portfolio_returns <- as.numeric(training_portfolio_returns_xts$returns)

actual_returns_xts <- xts(actual_simple_xgb_mac[,-1], order.by = as.Date(actual_simple_xgb_mac$date))
portfolio_returns_xts <- Return.portfolio(
  R = actual_returns_xts,
  weights = weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
actual_portfolio_returns <- as.numeric(portfolio_returns_xts$returns)

predicted_returns_xts <- xts(pred_simple_xgb_mac[,-1], order.by = as.Date(pred_simple_xgb_mac$date))
predicted_portfolio_returns_xts <- Return.portfolio(
  R = predicted_returns_xts,
  weights = weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
predicted_portfolio_returns <- as.numeric(predicted_portfolio_returns_xts$returns)

train_mean <- mean(training_portfolio_returns, na.rm = TRUE)
mspe_model <- mean((actual_portfolio_returns - predicted_portfolio_returns)^2, na.rm = TRUE)
mspe_naive <- mean((actual_portfolio_returns - train_mean)^2, na.rm = TRUE)
r2_oos <- 1 - mspe_model / mspe_naive
cat("OOS R^2 for portfolio:", round(r2_oos, 4), "\n")

actual_vec <- actual_portfolio_returns
predicted_vec <- predicted_portfolio_returns

# Directional accuracy: fraction of times the signs agree
directional_accuracy <- mean(sign(actual_vec) == sign(predicted_vec), na.rm = TRUE)

# Output result
cat("Directional Accuracy:", round(directional_accuracy, 4), "\n")



# portfolio 2 xgboost latent
xgbtestmllatent <- length(eur_latentxgpred)
xgbstartlatent <- as.Date("2019-01-30")
xgbseqlatent <- seq(from = xgbstartlatent, by = "month", length.out = xgbtestmllatent)
pred_returns_mlxglatent <- data.frame(
  date = xgbseqlatent,
  EUR = eur_latentxgpred,
  JPY = jpy_latentxgpred,
  SEK = sek_latentxgpred,
  NOK = nok_latentxgpred,
  AUD = aud_latentxgpred,
  CHF = chf_latentxgpred,
  NZD = nzd_latentxgpred,
  GBP = gbp_latentxgpred,
  CAD = cad_latentxgpred
)

xgbtestmllatentac <- length(actualeur_latentxg)
xgbstartlatentac <- as.Date("2019-01-30")
xgbseqlatentac <- seq(from = xgbstartlatentac, by = "month", length.out = xgbtestmllatentac)
pred_returns_mlxglatentac <- data.frame(
  date = xgbseqlatentac,
  EUR = actualeur_latentxg,
  JPY = actualjpy_latentxg,
  SEK = actualsek_latentxg,
  NOK = actualnok_latentxg,
  AUD = actualaud_latentxg,
  CHF = actualchf_latentxg,
  NZD = actualnzd_latentxg,
  GBP = actualgbp_latentxg,
  CAD = actualcad_latentxg
)


xgbtestmllatentin <- data.frame(
  EUR = actualeur_latentxgin,
  JPY = actualjpy_latentxgin,
  SEK = actualsek_latentxgin,
  NOK = actualnok_latentxgin,
  AUD = actualaud_latentxgin,
  CHF = actualchf_latentxgin,
  NZD = actualnzd_latentxgin,
  GBP = actualgbp_latentxgin,
  CAD = actualcad_latentxgin
)

# 1. Prepare predicted and actual returns (convert log to simple returns)
pred_simple_xgb_latent <- exp(pred_returns_mlxglatent[,-1]) - 1
pred_simple_xgb_latent <- cbind(date = pred_returns_mlxglatent$date, pred_simple_xgb_latent)
actual_simple_xgb_latent <- exp(pred_returns_mlxglatentac[,-1]) - 1
actual_simple_xgb_latent <- cbind(date = pred_returns_mlxglatentac$date, actual_simple_xgb_latent)
# 2. Create tertile portfolio weights
get_tertile_weights_latent <- function(row) {
  ranks <- rank(row, ties.method = "first")
  weights <- rep(0, length(row))
  weights[ranks >= 7] <- 1/3    # Top 3: long
  weights[ranks <= 3] <- -1/3   # Bottom 3: short
  return(weights)
}
weights_matrix_xgb_latent <- t(apply(pred_simple_xgb_latent[,-1], 1, get_tertile_weights_latent))
colnames(weights_matrix_xgb_latent) <- colnames(pred_simple_xgb_latent)[-1]
# 3. Convert to xts objects
weights_xts_xgb_latent <- xts(weights_matrix_xgb_latent, order.by = as.Date(pred_simple_xgb_latent$date))
actual_returns_xts_xgb_latent <- xts(actual_simple_xgb_latent[,-1], order.by = as.Date(actual_simple_xgb_latent$date))
# 4. Calculate portfolio returns
portfolio_returns_xgb_latent <- Return.portfolio(
  R = actual_returns_xts_xgb_latent,
  weights = weights_xts_xgb_latent,
  rebalance_on = "months",
  verbose = TRUE
)
charts.PerformanceSummary(portfolio_returns_xgb_latent$returns, main = "Tertile Long-Short Portfolio (XGBoost Latent)")
sharpe_ratio_xgb_latent <- SharpeRatio.annualized(portfolio_returns_xgb_latent$returns, Rf = 0)
cat("Annualized Sharpe Ratio (Latent):", round(sharpe_ratio_xgb_latent, 3), "\n")
print(table.AnnualizedReturns(portfolio_returns_xgb_latent$returns))
print(table.Stats(portfolio_returns_xgb_latent$returns))
max_dd_xgb_latent <- maxDrawdown(portfolio_returns_xgb_latent$returns, geometric = TRUE, invert = TRUE)
cat("Maximum Drawdown (Latent):", round(max_dd_xgb_latent, 4), "\n")
log_returns_xgb_latent <- log(1 + portfolio_returns_xgb_latent$returns)
cumulative_log_returns_xgb_latent <- cumsum(log_returns_xgb_latent)
print(cumulative_log_returns_xgb_latent)
plot(cumulative_log_returns_xgb_latent, main = "Cumulative Log Returns (XGBoost Latent Portfolio)")

ttxgrl <- seq(from = as.Date("2004-12-31"), by = "month", length.out = nrow(xgbtestmllatentin))
training_weights_matrix <- t(apply(xgbtestmllatentin, 1, get_tertile_weights))
colnames(training_weights_matrix) <- colnames(xgbtestmllatentin)
training_weights_xts <- xts(training_weights_matrix, order.by = ttxgrl)
training_returns_xts <- xts(xgbtestmllatentin, order.by = ttxgrl)
training_portfolio_returns_xts <- Return.portfolio(
  R = training_returns_xts,
  weights = training_weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
training_portfolio_returns <- as.numeric(training_portfolio_returns_xts$returns)

actual_returns_xts <- xts(actual_simple_xgb_latent[,-1], order.by = as.Date(actual_simple_xgb_latent$date))
portfolio_returns_xts <- Return.portfolio(
  R = actual_returns_xts,
  weights = weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
actual_portfolio_returns <- as.numeric(portfolio_returns_xts$returns)

predicted_returns_xts <- xts(pred_simple_xgb_latent[,-1], order.by = as.Date(pred_simple_xgb_latent$date))
predicted_portfolio_returns_xts <- Return.portfolio(
  R = predicted_returns_xts,
  weights = weights_xts,
  rebalance_on = "months",
  verbose = TRUE
)
predicted_portfolio_returns <- as.numeric(predicted_portfolio_returns_xts$returns)

train_mean <- mean(training_portfolio_returns, na.rm = TRUE)
mspe_model <- mean((actual_portfolio_returns - predicted_portfolio_returns)^2, na.rm = TRUE)
mspe_naive <- mean((actual_portfolio_returns - train_mean)^2, na.rm = TRUE)
r2_oos <- 1 - mspe_model / mspe_naive
cat("OOS R^2 for portfolio:", round(r2_oos, 4), "\n")



portfolio_returns_combinedxg <- merge(
  portfolio_returns_xgb$returns,
  portfolio_returns_xgb_mac$returns,
  portfolio_returns_xgb_latent$returns,
  all = TRUE
)
colnames(portfolio_returns_combinedxg) <- c("XGB Ml", "XGB Raw", "XGB Rl")
charts.PerformanceSummary(portfolio_returns_combinedxg, main = "Comparison of XGB Portfolios")

plot(cumulative_log_returns_combinedxg, main = "Cumulative Log Returns", col = 1:3)
# add a legend to the plot
legend("topleft", legend = colnames(portfolio_returns_combined), col = 1:3, lty = 1)


cumulative_log_returns_combinedxg <- cumsum(log(portfolio_returns_combinedxg + 1))
plot(cumulative_log_returns_combinedxg, main = "Cumulative Log Returns", col = 1:3)

plot(cumulative_log_returns_combinedxg, main = "Cum Log Returns XGB", col = 1:3)
xts::addLegend("topleft",
               legend.names = colnames(portfolio_returns_combined),
               col = 1:3,
               lty = 1)

actual_vec <- actual_portfolio_returns
predicted_vec <- predicted_portfolio_returns

# Directional accuracy: fraction of times the signs agree
directional_accuracy <- mean(sign(actual_vec) == sign(predicted_vec), na.rm = TRUE)

# Output result
cat("Directional Accuracy:", round(directional_accuracy, 4), "\n")

