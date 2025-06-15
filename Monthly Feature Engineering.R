# Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)
library(e1071)
library(moments)
library(zoo)
library(car)
# Load the data
eurusd <- read_excel("20 year eurusd.xlsx")
nzdusd <- read_excel("20 years nzd:usd.xlsx")
usdjpy <- read_excel("20 years usdjpy.xlsx")
gbpusd <- read_excel("20 years gbpusd.xlsx")
audusd <- read_excel("20 years audusd.xlsx")
usdchf <- read_excel("20 year usdchf.xlsx")
usdcad <- read_excel("20 years usdcad.xlsx")
usdnok <- read_excel("20 years usdnok.xlsx")
usdsek <- read_excel("20 years usdsek.xlsx")
cpi <- read.csv("cpim.csv")
gdp <- read.csv("gdp.csv")
unemp <- read.csv("unemp.csv")
interest <- read.csv("interest.csv")

# Rebasing the currencies against the USD
jpyusd <- usdjpy %>% 
  mutate(across(all_of(c("Bid", "Ask", "High", "Low", "Open", "Mid Price")), ~ 1/ .))

chfusd <- usdchf %>% 
  mutate(across(all_of(c("Bid", "Ask", "High", "Low", "Open", "Mid Price")), ~ 1/ .))

cadusd <- usdcad %>% 
  mutate(across(all_of(c("Bid", "Ask", "High", "Low", "Open", "Mid Price")), ~ 1/ .))

nokusd <- usdnok %>% 
  mutate(across(all_of(c("Bid", "Ask", "High", "Low", "Open", "Mid Price")), ~ 1/ .))

sekusd <- usdsek %>% 
  mutate(across(all_of(c("Bid", "Ask", "High", "Low", "Open", "Mid Price")), ~ 1/ .))

# midprice calculation
eurusd$midprice <- (eurusd$Ask + eurusd$Bid) / 2
sekusd$midprice <- (sekusd$Ask + sekusd$Bid) / 2
jpyusd$midprice <- (jpyusd$Ask + jpyusd$Bid) / 2
chfusd$midprice <- (chfusd$Ask + chfusd$Bid) / 2
cadusd$midprice <- (cadusd$Ask + cadusd$Bid) / 2
nzdusd$midprice <- (nzdusd$Ask + nzdusd$Bid) / 2
nokusd$midprice <- (nokusd$Ask + nokusd$Bid) / 2
gbpusd$midprice <- (gbpusd$Ask + gbpusd$Bid) / 2
audusd$midprice <- (audusd$Ask + audusd$Bid) / 2

# invert date in sets
eurusd <- eurusd[rev(1:nrow(eurusd)), ]
sekusd <- sekusd[rev(1:nrow(sekusd)), ]
jpyusd <- jpyusd[rev(1:nrow(jpyusd)), ]
chfusd <- chfusd[rev(1:nrow(chfusd)), ]
cadusd <- cadusd[rev(1:nrow(cadusd)), ]
nzdusd <- nzdusd[rev(1:nrow(nzdusd)), ]
nokusd <- nokusd[rev(1:nrow(nokusd)), ]
gbpusd <- gbpusd[rev(1:nrow(gbpusd)), ]
audusd <- audusd[rev(1:nrow(audusd)), ]

#log the midprices
eurusd$logmid <- log(eurusd$midprice)
sekusd$logmid <- log(sekusd$midprice)
jpyusd$logmid <- log(jpyusd$midprice)
chfusd$logmid <- log(chfusd$midprice)
cadusd$logmid <- log(cadusd$midprice)
nzdusd$logmid <- log(nzdusd$midprice)
nokusd$logmid <- log(nokusd$midprice)
gbpusd$logmid <- log(gbpusd$midprice)
audusd$logmid <- log(audusd$midprice)

# Convert data from perecentages to decimals
interest <- interest %>%
  mutate(across(where(is.numeric) & !all_of("date"), ~ . / 100))
unemp <- unemp %>%
  mutate(across(where(is.numeric) & !all_of("date"), ~ . / 100))
gdp <- gdp %>%
  mutate(across(where(is.numeric) & !all_of("date"), ~ . / 100)) 

#filter the data between 2004-04-30 and 2025-01-03
eurusd <- eurusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
sekusd <- sekusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
jpyusd <- jpyusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
chfusd <- chfusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
cadusd <- cadusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
nzdusd <- nzdusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
nokusd <- nokusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
gbpusd <- gbpusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
audusd <- audusd %>%
  filter(between(`Exchange Date`, as.Date('2004-04-30'), as.Date('2025-01-30')))
cpi$date <- as.Date(cpi$date)
cpi <- cpi%>%
  filter(between(date, as.Date('2004-04-30'), as.Date('2025-01-30')))
gdp$date <- as.Date(gdp$date)
gdp <- gdp %>%
  filter(between(date, as.Date('2004-04-30'), as.Date('2025-01-30')))
interest$date <- as.Date(interest$date)
interest <- interest %>%
  filter(between(date, as.Date('2004-04-30'), as.Date('2025-01-30')))
unemp$date <- as.Date(unemp$date)
unemp <- unemp %>%
  filter(between(date, as.Date('2004-04-30'), as.Date('2025-01-30')))

# carry risk factor computation by taking the difference between the interest rates of the two currencies.
# the carry is in a seperate dataframe
carry <- data.frame(
  date = eurusd$`Exchange Date`,
  eur = (interest$EU.rate - interest$US.rate))
carry$sek <- (interest$SWD.rate - interest$US.rate)
carry$jpy <- (interest$JP.rate - interest$US.rate)
carry$chf <- (interest$CH.rate - interest$US.rate)
carry$cad <- (interest$CAN.rate - interest$US.rate)
carry$nzd <- (interest$NZ.rate - interest$US.rate)
carry$nok <- (interest$NW.rate - interest$US.rate)
carry$gbp <- (interest$Uk.rate - interest$US.rate)
carry$aud <- (interest$AU.rate - interest$US.rate)

# currency momentum calculation per currency pair in a new data set called momentum
# 3 and 6 month calculation.
momentum <- data.frame(
  date = eurusd$`Exchange Date`,
  m3eur = eurusd$logmid - lag(eurusd$logmid, 3),
  m6eur =  eurusd$logmid - lag(eurusd$logmid, 6))

#momentum sek/usd
momentum$m3sek = sekusd$logmid - lag(sekusd$logmid, 3)
momentum$m6sek = sekusd$logmid -lag(sekusd$logmid, 6)

# momentum calculation for gbpusd
momentum$m3gbp = gbpusd$logmid - lag(gbpusd$logmid, 3)
momentum$m6gbp = gbpusd$logmid - lag(gbpusd$logmid, 6)

# momentum calculation for nokusd
momentum$m3nok = nokusd$logmid - lag(nokusd$logmid, 3)
momentum$m6nok = nokusd$logmid - lag(nokusd$logmid, 6)

#momentum calculation for cadusd
momentum$m3cad = cadusd$logmid - lag(cadusd$logmid, 3)
momentum$m6cad = cadusd$logmid - lag(cadusd$logmid, 6)

#momentum calculation for nzdusd
momentum$m3nzd = nzdusd$logmid - lag(nzdusd$logmid, 3)
momentum$m6nzd = nzdusd$logmid - lag(nzdusd$logmid, 6)

#momentum calculation for jpyusd
momentum$m3jpy = jpyusd$logmid - lag(jpyusd$logmid, 3)
momentum$m6jpy = jpyusd$logmid - lag(jpyusd$logmid, 6)

#momentum calculation for chfusd
momentum$m3chf = chfusd$logmid - lag(chfusd$logmid, 3)
momentum$m6chf = chfusd$logmid - lag(chfusd$logmid, 6)

# momentum calculation for audusd
momentum$m3aud = audusd$logmid - lag(audusd$logmid, 3)
momentum$m6aud = audusd$logmid - lag(audusd$logmid, 6)

# compute the value risk factor
value <- data.frame(
  date = eurusd$`Exchange Date`,
  valueeur = eurusd$logmid + log(cpi$EU.CPI) - log(cpi$CPI.US)
)

# AUD
value$valueaud <- audusd$logmid + log(cpi$AU.CPI.NADJ_monthly) - log(cpi$CPI.US)

# NOK
value$valuenok <- nokusd$logmid + log(cpi$CPI.NW) - log(cpi$CPI.US)

# NZD
value$valuenzd <- nzdusd$logmid + log(cpi$NZ.CPI.NADJ_monthly) - log(cpi$CPI.US)

# GBP
value$valuegbp <- gbpusd$logmid + log(cpi$UK.CPI) - log(cpi$CPI.US)

# JPY
value$valuejpy <- jpyusd$logmid + log(cpi$JP.CPI) - log(cpi$CPI.US)

# SEK
value$valuesek <- sekusd$logmid + log(cpi$SWD.CPI) - log(cpi$CPI.US)

# CAD
value$valuecad <- cadusd$logmid + log(cpi$CPI.CAN) - log(cpi$CPI.US)

# CHF
value$valuechf <- chfusd$logmid + log(cpi$SWZ.CPI) - log(cpi$CPI.US)

# Compute currency excess returns
Snzd = lead(nzdusd$logmid) - nzdusd$logmid
excess <- data.frame(
  date = eurusd$`Exchange Date`,
  returnnzd = carry$nzd + Snzd)

# EUR
Seur = lead(eurusd$logmid) - eurusd$logmid
excess$returneur <- carry$eur + Seur

# AUD
Saud = lead(audusd$logmid) - audusd$logmid
excess$returnaud <- carry$aud + Saud

# GBP
Sgbp = lead(gbpusd$logmid) - gbpusd$logmid
excess$returngbp <- carry$gbp + Sgbp

# NOK
Snk = lead(nokusd$logmid) - nokusd$logmid
excess$returnnok <- carry$nok + Snk

# JPY
Sjpy = lead(jpyusd$logmid) - jpyusd$logmid
excess$returnjpy <- carry$jpy + Sjpy

# SEK
Ssek = lead(sekusd$logmid) - sekusd$logmid
excess$returnsek <- carry$sek + Ssek

# CHF
Schf = lead(chfusd$logmid) - chfusd$logmid
excess$returnchf <- carry$chf + Schf

# CAD
Scad = lead(cadusd$logmid) - cadusd$logmid
excess$returncad <- carry$cad + Scad

# volatility computation
#EUR

volatility <- data.frame(
  date = eurusd$`Exchange Date`,
  vol3eur = rollapply(excess$returneur, width = 3, FUN = sd,
                      fill = NA, align = "right"),
  vol6eur = rollapply(excess$returneur, width = 6, FUN = sd,
                      fill = NA, align = "right"))
# AUD
volatility$vol3aud <-rollapply(excess$returnaud, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6aud <-rollapply(excess$returnaud, width = 6, FUN = sd,
                               fill = NA, align = "right")

# NWK
volatility$vol3nok <-rollapply(excess$returnnok, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6nok <-rollapply(excess$returnnok, width = 6, FUN = sd,
                               fill = NA, align = "right")
#SWK
volatility$vol3sek <-rollapply(excess$returnsek, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6sek <-rollapply(excess$returnsek, width = 6, FUN = sd,
                               fill = NA, align = "right")

#CAD
volatility$vol3cad <-rollapply(excess$returncad, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6cad <-rollapply(excess$returncad, width = 6, FUN = sd,
                               fill = NA, align = "right")

#JPY
volatility$vol3jpy <-rollapply(excess$returnjpy, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6jpy <-rollapply(excess$returnjpy, width = 6, FUN = sd,
                               fill = NA, align = "right")
#CHF
volatility$vol3chf <-rollapply(excess$returnchf, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6chf <-rollapply(excess$returnchf, width = 6, FUN = sd,
                               fill = NA, align = "right")
#NZD
volatility$vol3nzd <-rollapply(excess$returnnzd, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6nzd <-rollapply(excess$returnnzd, width = 6, FUN = sd,
                               fill = NA, align = "right")
#GBP
volatility$vol3gbp <-rollapply(excess$returngbp, width = 3, FUN = sd,
                               fill = NA, align = "right")
volatility$vol6gbp <-rollapply(excess$returngbp, width = 6, FUN = sd,
                               fill = NA, align = "right")

#Skewness
#Eur
skewness <- data.frame(
  date = eurusd$`Exchange Date`,
  skew3eur = rollapply(excess$returneur, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                       fill = NA, align = "right"),
  skew6eur = rollapply(excess$returneur, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                       fill = NA, align = "right")
)

#AUD
skewness$skew3aud <- rollapply(excess$returnaud, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6aud <- rollapply(excess$returnaud, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")

#NK
skewness$skew3nok <- rollapply(excess$returnnok, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6nok <- rollapply(excess$returnnok, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")

#GBP
skewness$skew3gbp <- rollapply(excess$returngbp, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6gbp <- rollapply(excess$returngbp, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
#NZD
skewness$skew3nzd <- rollapply(excess$returnnzd, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6nzd <- rollapply(excess$returnnzd, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
#CHF
skewness$skew3chf <- rollapply(excess$returnchf, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6chf <- rollapply(excess$returnchf, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
#SWK
skewness$skew3sek <- rollapply(excess$returnsek, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6sek <- rollapply(excess$returnsek, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
#JPY
skewness$skew3jpy <- rollapply(excess$returnjpy, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6jpy <- rollapply(excess$returnjpy, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
#CAD
skewness$skew3cad <- rollapply(excess$returncad, width = 3, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")
skewness$skew6cad <- rollapply(excess$returncad, width = 6, FUN = function(x) skewness(x, na.rm = TRUE),
                               fill = NA, align = "right")

#Kurtosis
#Kurtosis
#EUR
kurtosis <- data.frame(
  date = eurusd$`Exchange Date`,
  kurt3eur = rollapply(excess$returneur, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                       fill = NA, align = "right"),
  kurt6eur = rollapply(excess$returneur, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                       fill = NA, align = "right")
)

#AUD
kurtosis$kurt3aud <- rollapply(excess$returnaud, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6aud <- rollapply(excess$returnaud, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")

#GBP
kurtosis$kurt3gbp <- rollapply(excess$returngbp, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6gbp <- rollapply(excess$returngbp, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
#NK
kurtosis$kurt3nok <- rollapply(excess$returnnok, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6nok <- rollapply(excess$returnnok, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
#SWK
kurtosis$kurt3sek <- rollapply(excess$returnsek, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6sek <- rollapply(excess$returnsek, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
#CAD
kurtosis$kurt3cad <- rollapply(excess$returncad, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6cad <- rollapply(excess$returncad, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
#JPY
kurtosis$kurt3jpy <- rollapply(excess$returnjpy, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6jpy <- rollapply(excess$returnjpy, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
#NZD
kurtosis$kurt3nzd <- rollapply(excess$returnnzd, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6nzd <- rollapply(excess$returnnzd, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
#CHF
kurtosis$kurt3chf <- rollapply(excess$returnchf, width = 3, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")
kurtosis$kurt6chf <- rollapply(excess$returnchf, width = 6, FUN = function(x) {kurtosis(x, na.rm = TRUE)},
                               fill = NA, align = "right")

#Price indicator calculations
eurusd <- eurusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

chfusd <- chfusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

audusd <- audusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

cadusd <- cadusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

gbpusd <- gbpusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

jpyusd <- jpyusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

nokusd <- nokusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )


sekusd <- sekusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

nzdusd <- nzdusd %>%
  mutate(
    spread = Ask - Bid,
    rel_spread = (Ask - Bid) / Ask,
    range = High - Low,
    rel_range = (High - Low) / midprice,
    log_range = log(High / Low)
  )

#Currency set aggregations
G10 <- data.frame(date = eurusd$`Exchange Date`)
G10$logeur <- eurusd$logmid
G10$lognzd <- nzdusd$logmid
G10$logjpy <- jpyusd$logmid
G10$lognk <- nokusd$logmid
G10$loggbp <- gbpusd$logmid
G10$logchf <- chfusd$logmid
G10$logcad <- cadusd$logmid
G10$logswk <- sekusd$logmid
G10$logaud <- audusd$logmid

#Filter the datasets again to match the date range
eurusd1 <- eurusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
audusd1 <- audusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
jpyusd1 <- jpyusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
nzdusd1 <- nzdusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
gbpusd1 <- gbpusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
nokusd1 <- nokusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
sekusd1 <- sekusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
chfusd1 <- chfusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
cadusd1 <- cadusd %>% filter(between(`Exchange Date`, as.Date('2004-12-31'), as.Date('2024-12-27')))
winterest1 <- interest %>% filter(between(date, as.Date('2004-12-31'), as.Date('2024-12-27')))
wcpi1 <- cpi %>% filter(between(date, as.Date('2004-12-31'), as.Date('2024-12-27')))
wunemp1 <- unemp %>% filter(between(date, as.Date('2004-12-31'), as.Date('2024-12-27')))
wgdp1 <- gdp %>% filter(between(date, as.Date('2004-12-31'), as.Date('2024-12-27')))
wexcess1 <- excess %>% filter(between(date, as.Date('2004-12-31'), as.Date('2024-12-27')))

#Combine all the datasets into one final dataset
fn <- merge(G10, momentum, by = "date")
fn <- merge(fn, value, by = "date")
fn <- merge(fn, volatility, by = "date")  
fn <- merge(fn, skewness, by = "date")
fn <- merge(fn, kurtosis, by = "date")
fn <- merge(fn, carry, by = "date")

fn <- fn %>% filter(between(date, as.Date('2004-12-31'), as.Date('2024-12-27')))


# autoencoder creation
#Eurset
eurset <- data.frame(
  logp = fn$logeur,
  carry = fn$eur,
  m3 = fn$m3eur,
  m6 = fn$m6eur,
  value = fn$valueeur,
  vol3 = fn$vol3eur,
  vol6 = fn$vol6eur,
  skew3 = fn$skew3eur,
  skew6 = fn$skew6eur,
  kurt6 = fn$kurt6eur,
  spread = eurusd1$spread,
  relspread = eurusd1$rel_spread,
  range = eurusd1$range,
  relrange = eurusd1$rel_range,
  logrange = eurusd1$log_range,
  unemp = wunemp1$EU.UNMP,
  gdp = wgdp1$EUR.GDP,
  cpi = wcpi1$EU.CPI,
  rate = winterest1$EU.rate,
  currency = "eur"
)

#lag the values
eurset <- eurset %>%
  mutate(across(everything(), ~lag(.x, 1)))
eurset <- eurset[-1, ]

#gbpset
gbpset <- data.frame(
  logp = fn$loggbp,
  carry = fn$gbp,
  m3 = fn$m3gbp,
  m6 = fn$m6gbp,
  value = fn$valuegbp,
  vol3 = fn$vol3gbp,
  vol6 = fn$vol6gbp,
  skew3 = fn$skew3gbp,
  skew6 = fn$skew6gbp,
  kurt6 = fn$kurt6gbp,
  spread = gbpusd1$spread,
  relspread = gbpusd1$rel_spread,
  range = gbpusd1$range,
  relrange = gbpusd1$rel_range,
  logrange = gbpusd1$log_range,
  unemp = wunemp1$UK.UNMP,
  gdp = wgdp1$UK.GDP,
  cpi = wcpi1$UK.CPI,
  rate = winterest1$Uk.rate,
  currency = "gbp"
)

gbpset <- gbpset %>%
  mutate(across(everything(), ~lag(.x, 1)))
#remove the first row of the dataset
gbpset <- gbpset[-1, ]

#nzdset
nzdset <- data.frame(
  logp = fn$lognzd,
  carry = fn$nzd,
  m3 = fn$m3nzd,
  m6 = fn$m6nzd,
  value = fn$valuenzd,
  vol3 = fn$vol3nzd,
  vol6 = fn$vol6nzd,
  skew3 = fn$skew3nzd,
  skew6 = fn$skew6nzd,
  kurt6 = fn$kurt6nzd,
  spread = nzdusd1$spread,
  relspread = nzdusd1$rel_spread,
  range = nzdusd1$range,
  relrange = nzdusd1$rel_range,
  logrange = nzdusd1$log_range,
  unemp = wunemp1$NZ.UNEMPLOYMENT.RATE.SADJ.y,
  gdp = wgdp1$NZ.GDP,
  cpi = wcpi1$NZ.CPI.NADJ_monthly,
  rate = winterest1$NZ.rate,
  currency = "nzd"
)

nzdset <- nzdset %>%
  mutate(across(everything(), ~lag(.x, 1)))
nzdset <- nzdset[-1, ]

#audset
audset <- data.frame(
  logp = fn$logaud,
  carry = fn$aud,
  m3 = fn$m3aud,
  m6 = fn$m6aud,
  value = fn$valueaud,
  vol3 = fn$vol3aud,
  vol6 = fn$vol6aud,
  skew3 = fn$skew3aud,
  skew6 = fn$skew6aud,
  kurt6 = fn$kurt6aud,
  spread = audusd1$spread,
  relspread = audusd1$rel_spread,
  range = audusd1$range,
  relrange = audusd1$rel_range,
  logrange = audusd1$log_range,
  unemp = wunemp1$AU.UNMP,
  gdp = wgdp1$AU.GDP,
  cpi = wcpi1$AU.CPI.NADJ_monthly,
  rate = winterest1$AU.rate,
  currency = "aud"
)

audset <- audset %>%
  mutate(across(everything(), ~lag(.x, 1)))
audset <- audset[-1, ]

#jpy
jpyset <- data.frame(
  logp = fn$logjpy,
  carry = fn$jpy,
  m3 = fn$m3jpy,
  m6 = fn$m6jpy,
  value = fn$valuejpy,
  vol3 = fn$vol3jpy,
  vol6 = fn$vol6jpy,
  skew3 = fn$skew3jpy,
  skew6 = fn$skew6jpy,
  kurt6 = fn$kurt6jpy,
  spread = jpyusd1$spread,
  relspread = jpyusd1$rel_spread,
  range = jpyusd1$range,
  relrange = jpyusd1$rel_range,
  logrange = jpyusd1$log_range,
  unemp = wunemp1$JP.UNMP,
  gdp = wgdp1$JP.GDP,
  cpi = wcpi1$JP.CPI,
  rate = winterest1$JP.rate,
  currency = "jpy"
)

jpyset<- jpyset %>%
  mutate(across(everything(), ~lag(.x, 1)))
#remove the first row of the dataset
jpyset <- jpyset[-1, ]

#chf
chfset <- data.frame(
  logp = fn$logchf,
  carry = fn$chf,
  m3 = fn$m3chf,
  m6 = fn$m6chf,
  value = fn$valuechf,
  vol3 = fn$vol3chf,
  vol6 = fn$vol6chf,
  skew3 = fn$skew3chf,
  skew6 = fn$skew6chf,
  kurt6 = fn$kurt6chf,
  spread = chfusd1$spread,
  relspread = chfusd1$rel_spread,
  range = chfusd1$range,
  relrange = chfusd1$rel_range,
  logrange = chfusd1$log_range,
  unemp = wunemp1$SWZ.UNMP,
  gdp = wgdp1$CH.GDP,
  cpi = wcpi1$SWZ.CPI,
  rate = winterest1$CH.rate,
  currency = "chf"
)

chfset <- chfset %>%
  mutate(across(everything(), ~lag(.x, 1)))
chfset <- chfset[-1, ]


#nk
nokset <- data.frame(
  logp = fn$lognk,
  carry = fn$nok,
  m3 = fn$m3nok,
  m6 = fn$m6nok,
  value = fn$valuenok,
  vol3 = fn$vol3nok,
  vol6 = fn$vol6nok,
  skew3 = fn$skew3nok,
  skew6 = fn$skew6nok,
  kurt6 = fn$kurt6nok,
  spread = nokusd1$spread,
  relspread = nokusd1$rel_spread,
  range = nokusd1$range,
  relrange = nokusd1$rel_range,
  logrange = nokusd1$log_range,
  unemp = wunemp1$NRW.UNMP.y,
  gdp = wgdp1$NW.GDP,
  cpi = wcpi1$CPI.NW,
  rate = winterest1$NW.rate,
  currency = "nok"
)

nokset <- nokset %>%
  mutate(across(everything(), ~lag(.x, 1)))
nokset <- nokset[-1, ]

#swk
sekset <- data.frame(
  logp = fn$logswk,
  carry = fn$sek,
  m3 = fn$m3sek,
  m6 = fn$m6sek,
  value = fn$valuesek,
  vol3 = fn$vol3sek,
  vol6 = fn$vol6sek,
  skew3 = fn$skew3sek,
  skew6 = fn$skew6sek,
  kurt6 = fn$kurt6sek,
  spread = sekusd1$spread,
  relspread = sekusd1$rel_spread,
  range = sekusd1$range,
  relrange = sekusd1$rel_range,
  logrange = sekusd1$log_range,
  unemp = wunemp1$SWD.UNMP,
  gdp = wgdp1$SWD.GDP,
  cpi = wcpi1$SWD.CPI,
  rate = winterest1$SWD.rate,
  currency = "sek"
)

sekset <- sekset %>%
  mutate(across(everything(), ~lag(.x, 1)))

sekset <- sekset[-1, ]

#cad
cadset <- data.frame(
  logp = fn$logcad,
  carry = fn$cad,
  m3 = fn$m3cad,
  m6 = fn$m6cad,
  value = fn$valuecad,
  vol3 = fn$vol3cad,
  vol6 = fn$vol6cad,
  skew3 = fn$skew3cad,
  skew6 = fn$skew6cad,
  kurt6 = fn$kurt6cad,
  spread = cadusd1$spread,
  relspread = cadusd1$rel_spread,
  range = cadusd1$range,
  relrange = cadusd1$rel_range,
  logrange = cadusd1$log_range,
  unemp = wunemp1$CAN.UNMP,
  gdp = wgdp1$CAN.GDP,
  cpi = wcpi1$CPI.CAN,
  rate = winterest1$CAN.rate,
  currency = "cad"
)

cadset <- cadset %>%
  mutate(across(everything(), ~lag(.x, 1)))
cadset
#remove the first row of the dataset
cadset <- cadset[-1, ]

#create the full pooled set
fulldata <- bind_rows(eurset, gbpset, nzdset, chfset, cadset, audset, jpyset,
                      nokset, sekset)

#save the labels
labels <- fulldata$currency
#Save the dataset
write.csv(fulldata, "monthlyfull.csv", row.names = FALSE)

# Load the latent variables
macrolatent_features <- read.csv('latent_factors_full.csv')
macrolatent_features
macrolatent_features$currency <- labels
macrolatent_features

#create a data set with all macrolatent_variables for each currency combined
# with the excess returns.  
eurml <- macrolatent_features %>%
  filter(currency == "eur") %>%
  select(, -currency) %>%
  mutate(excesseur = wexcess1$returneur[-1]) 

#repeat the same for the other currencies
jpyml <- macrolatent_features %>%
  filter(currency == "jpy") %>%
  select( -currency) %>%
  mutate(excessjpy = wexcess1$returnjpy[-1])

gbpml <- macrolatent_features %>%
  filter(currency == "gbp") %>%
  select( -currency) %>%
  mutate(excessgbp = wexcess1$returngbp[-1])

nzdml <- macrolatent_features %>%
  filter(currency == "nzd") %>%
  select(-currency) %>%
  mutate(excessnzd = wexcess1$returnnzd[-1])

audml <- macrolatent_features %>%
  filter(currency == "aud") %>%
  select(-currency) %>%
  mutate(excessaud = wexcess1$returnaud[-1])

chfml <- macrolatent_features %>%
  filter(currency == "chf") %>%
  select(-currency) %>%
  mutate(excesschf = wexcess1$returnchf[-1])

cadml <- macrolatent_features %>%
  filter(currency == "cad") %>%
  select(-currency) %>%
  mutate(excesscad = wexcess1$returncad[-1])

nokml <- macrolatent_features %>%
  filter(currency == "nok") %>%
  select(-currency) %>%
  mutate(excessnok = wexcess1$returnnok[-1])

sekml <- macrolatent_features %>%
  filter(currency == "sek") %>%
  select(-currency) %>%
  mutate(excesssek = wexcess1$returnsek[-1])

#save all as csv
write.csv(eurml, "eurmlm.csv", row.names = FALSE)
write.csv(jpyml, "jpymlm.csv", row.names = FALSE)
write.csv(gbpml, "gbpmlm.csv", row.names = FALSE)
write.csv(nzdml, "nzdmlm.csv", row.names = FALSE)
write.csv(audml, "audmlm.csv", row.names = FALSE)
write.csv(chfml, "chfmlm.csv", row.names = FALSE)
write.csv(cadml, "cadmlm.csv", row.names = FALSE)
write.csv(nokml, "nokmlm.csv", row.names = FALSE)
write.csv(sekml, "sekmlm.csv", row.names = FALSE)



#use the mutate function to remove currenct column from the eurset and store it in a new dataframe.
# add the excess to this new set.
eurmac <- eurset %>%
  mutate(
    currency = NULL,
    excesseur = wexcess1$returneur[-1])

jpymac <- jpyset %>%
  mutate(
    currency = NULL,
    excessjpy = wexcess1$returnjpy[-1])
gbpmac <- gbpset %>%
  mutate(
    currency = NULL,
    excessgbp = wexcess1$returngbp[-1])
nzdmac <- nzdset %>%
  mutate(
    currency = NULL,
    excessnzd = wexcess1$returnnzd[-1])
audmac <- audset %>%
  mutate(
    currency = NULL,
    excessaud = wexcess1$returnaud[-1])
chfmac <- chfset %>%
  mutate(
    currency = NULL
    , excesschf = wexcess1$returnchf[-1])
cadmac <- cadset %>%
  mutate(
    currency = NULL
    , excesscad = wexcess1$returncad[-1])
nokmac <- nokset %>%
  mutate(
    currency = NULL
    , excessnok = wexcess1$returnnok[-1])
sekmac <- sekset %>%
  mutate(
    currency = NULL,
    excesssek = wexcess1$returnsek[-1])

write.csv(eurmac, "eurmacm.csv", row.names = FALSE)
write.csv(jpymac, "jpymacm.csv", row.names = FALSE)
write.csv(gbpmac, "gbpmacm.csv", row.names = FALSE)
write.csv(nzdmac, "nzdmacm.csv", row.names = FALSE)
write.csv(audmac, "audmacm.csv", row.names = FALSE)
write.csv(chfmac, "chfmacm.csv", row.names = FALSE)
write.csv(cadmac, "cadmacm.csv", row.names = FALSE)
write.csv(nokmac, "nokmacm.csv", row.names = FALSE)
write.csv(sekmac, "sekmacm.csv", row.names = FALSE)

# create a dataset per currency without interest, cpi, gdp and unemployment rate
eurset_no_macro <- eurset %>%
  select(-unemp, -gdp, -cpi, -rate)
gbpset_no_macro <- gbpset %>%
  select(-unemp, -gdp, -cpi, -rate)
nzdset_no_macro <- nzdset %>%
  select(-unemp, -gdp, -cpi, -rate)
audset_no_macro <- audset %>%
  select(-unemp, -gdp, -cpi, -rate)
jpyset_no_macro <- jpyset %>%
  select(-unemp, -gdp, -cpi, -rate)
chfset_no_macro <- chfset %>%
  select(-unemp, -gdp, -cpi, -rate)
cadset_no_macro <- cadset %>%
  select(-unemp, -gdp, -cpi, -rate)
nokset_no_macro <- nokset %>%
  select(-unemp, -gdp, -cpi, -rate)
sekset_no_macro <- sekset %>%
  select(-unemp, -gdp, -cpi, -rate)

# combine into. full no macro fulldataset
fulldataset_no_macro <- bind_rows(
  eurset_no_macro,
  gbpset_no_macro,
  nzdset_no_macro,
  audset_no_macro,
  jpyset_no_macro,
  chfset_no_macro,
  cadset_no_macro,
  nokset_no_macro,
  sekset_no_macro
)



macrolabels <- fulldataset_no_macro$currency
macfinalset <- fulldataset_no_macro
macfinalset
write.csv(macfinalset, "nomacfinalsetmonth.csv", row.names = FALSE)

#Load the latent variables without the macroeconomic variables
latnomac <- read.csv("latent_factors.csv")
latnomac$currency <- labels
latnomac
# create a new dataframe with only the latent features that have eur in the currency column and add the excesseur column.
eur_latent_no_macro <- latnomac %>%
  filter(currency == 'eur') %>%
  select(-currency) %>% 
  mutate(
    excesseur = wexcess1$returneur[-1])
# create a new dataframe with only the latent features that have jpy in the currency column and add the excessjpy column.
jpy_latent_no_macro <- latnomac %>%
  filter(currency == 'jpy') %>%
  select(-currency) %>% 
  mutate(
    excessjpy = wexcess1$returnjpy[-1])
# create a new dataframe with only the latent features that have gbp in the currency column and add the excessgbp column.
gbp_latent_no_macro <- latnomac %>%
  filter(currency == 'gbp') %>%
  select(-currency) %>% 
  mutate(
    excessgbp = wexcess1$returngbp[-1])
# create a new dataframe with only the latent features that have nzd in the currency column and add the excessnzd column.
nzd_latent_no_macro <- latnomac %>%
  filter(currency == 'nzd') %>%
  select(-currency) %>% 
  mutate(
    excessnzd = wexcess1$returnnzd[-1])
# create a new dataframe with only the latent features that have aud in the currency column and add the excessaud column.
aud_latent_no_macro <- latnomac %>%
  filter(currency == 'aud') %>%
  select(-currency) %>% 
  mutate(
    excessaud = wexcess1$returnaud[-1])
# create a new dataframe with only the latent features that have chf in the currency column and add the excesschf column.
chf_latent_no_macro <- latnomac %>%
  filter(currency == 'chf') %>%
  select(-currency) %>% 
  mutate(
    excesschf = wexcess1$returnchf[-1])
# create a new dataframe with only the latent features that have cad in the currency column and add the excesscad column.
cad_latent_no_macro <- latnomac %>%
  filter(currency == 'cad') %>%
  select(-currency) %>% 
  mutate(
    excesscad = wexcess1$returncad[-1])
# create a new dataframe with only the latent features that have nok in the currency column and add the excessnok column.
nok_latent_no_macro <- latnomac %>%
  filter(currency == 'nok') %>%
  select(-currency) %>% 
  mutate(
    excessnok = wexcess1$returnnok[-1])
# create a new dataframe with only the latent features that have sek in the currency column and add the excesssek column.
sek_latent_no_macro <- latnomac %>%
  filter(currency == 'sek') %>%
  select(-currency) %>% 
  mutate(
    excesssek = wexcess1$returnsek[-1])



#save the _latent_no_macro sets as csv files
write.csv(eur_latent_no_macro, "eur_latent_no_macrom.csv", row.names = FALSE)
write.csv(jpy_latent_no_macro, "jpy_latent_no_macrom.csv", row.names = FALSE)
write.csv(gbp_latent_no_macro, "gbp_latent_no_macrom.csv", row.names = FALSE)
write.csv(nzd_latent_no_macro, "nzd_latent_no_macrom.csv", row.names = FALSE)
write.csv(aud_latent_no_macro, "aud_latent_no_macrom.csv", row.names = FALSE)
write.csv(chf_latent_no_macro, "chf_latent_no_macrom.csv", row.names = FALSE)
write.csv(cad_latent_no_macro, "cad_latent_no_macrom.csv", row.names = FALSE)
write.csv(nok_latent_no_macro, "nok_latent_no_macrom.csv", row.names = FALSE)
write.csv(sek_latent_no_macro, "sek_latent_no_macrom.csv", row.names = FALSE)


