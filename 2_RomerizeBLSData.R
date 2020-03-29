# ------------------------------------------------------------------------
# Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
#   Historical Classifications," Journal of Applied Econometrics, forthcoming
# ------------------------------------------------------------------------
#
# Constructs replications of historical CPI series using modern data
# - Replication of the CPI by Hoover (1960) covering 1851-1880 replicated
#   since the 1950s until today
# - Weights are saved in vectors (Hoover weight, 2018 BLS weight)
# - If single observations missing, data are interpolated linearly
# - If series is missing data are replaced by next higher aggregate
#
# Constructs replication of the proxy used in the paper based on modern 
# PPI data
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------
rm(list = ls())
library(jsonlite)
library(lubridate)
library(xts)
library(tsbox)
library(forecast)
library(seasonal)
library(quantmod)
library(ggplot2)   
library(xlsx)
library(reshape2)
library(xtable)
library(mFilter)
library(stargazer)
library(sandwich)
library(extrafont)
library(car)
library(extrafont)

# ------------------------------------------------------------------------
# 0) User-defined functions
# ------------------------------------------------------------------------
ts_fred <- function(..., class = "data.frame") {
  symb <- c(...)
  dta.env <- new.env()
  suppressMessages(getSymbols(symb, env = dta.env, src = "FRED"))
  z <- data.table::rbindlist(lapply(as.list(dta.env), ts_dt), idcol = "id")
  tsbox:::as_class(class)(z)
}

getData <- function(series, startY, endY, regkey) {
  # Download BLS data from api and return xts object
 
  baseurl <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"

  mydata <- fromJSON(paste(baseurl, series, "?registrationkey=", key, "&startyear=", startY, "&endyear=", endY, sep = ""), flatten=TRUE)
  mydata <- fromJSON(paste(baseurl, series, "?registrationkey=", key, "&startyear=", startY, "&endyear=", endY, sep = ""), simplifyDataFrame = TRUE)
  mydata <- as.data.frame(mydata$Results$series$data)
  myYear <- mydata$year
  myMonth <- substr(mydata$period, 2, 3)
  
 
  series <- ts_xts(ts_ts(xts(as.numeric(mydata$value), order.by = as.Date(paste(myYear, "-", myMonth, "-01", sep ="")))))
  
  return(series)
}

getDataMBLS <- function(series, startY, endY, regkey, fillna) {
  # Wrapper function to get monthly BLS data for ranges larger than 20 years
  # Option for interpolating missing values linearly (relevant in early sample of price data)
  
  # Loop in steps of 20 years because this is the maximum number of years allowed
  for(y in seq(from = startY, to = endY, by = 20)){
    
    temp1 <- getData(series, y, min(y+20, year(today()), endY) , key) 
    if (y > startY){
      temp2 <- ts_bind(temp2, temp1)
    }else{
      temp2 <- temp1
    }
  }
  
  series <- temp2
  
  if (fillna == TRUE){
    series <- ts_xts(na.approx(ts_ts(series)) )
  }
  
  return(series)
}

calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}

# ------------------------------------------------------------------------
# 1) Get Data from BLS for replication of Hoover (1960) index
#    See Online Appendix Table B3 for details
# ------------------------------------------------------------------------
# NOTE: To get the original data from the BLS (loadFromWeb <- TRUE), the 
# user has to obtain a key from the BLS (https://www.bls.gov/developers/api_signature_v2.htm)
# Otherwise, the data downloaded before (from BLSData.RData) is used.
key <- "api key from bls"
endY <- 2018
figw <- 4
figh <- 4
loadFromWeb <- FALSE
Sys.setenv(X13_PATH = "C:/Users/daenu/Documents/R/win-library/3.6/x13binary/bin")


# Load all necessary data for later use
if(loadFromWeb == TRUE){

  # Flour
  f10 <- getDataMBLS("MUUR0000SA1111", 1930, 1999, key, fillna = TRUE)
  f11 <- getDataMBLS("MUUR0000SE0101", 1960, 1999, key, fillna = TRUE)
  f12 <- getDataMBLS("CUUR0000SEFA01", 1980, endY, key, fillna = TRUE)
  
  # Rice
  f20 <- getDataMBLS("MUUR0000SA1111", 1930, 1999, key, fillna = TRUE)
  f21 <- getDataMBLS("MUUR0000SE0103", 1960, 1999, key, fillna = TRUE)
  f22 <- getDataMBLS("CUUR0000SEFA03", 1960, endY, key, fillna = TRUE)
  
  
  # Beef
  F3 <- getDataMBLS("CUUR0000SEFC", 1920, endY, key, fillna = TRUE)
  
  # Pork
  f41 <- getDataMBLS("MUUR0000SE04", 1920, 1999, key, fillna = TRUE)
  f42 <- getDataMBLS("CUUR0000SEFD", 1920, endY, key, fillna = TRUE)
  
  # Mutton
  f50 <- getDataMBLS("MUSR0000SA111211", 1940, 1999, key, fillna = TRUE)
  f51 <- getDataMBLS("CUUR0000SEFE", 1940, endY, key, fillna = TRUE)
  
  # Fish
  F6 <- getDataMBLS("CUUR0000SEFG", 1920, endY, key, fillna = TRUE)
  
  # Whole milk
  F7 <- getDataMBLS("CUUR0000SS09011", 1920, endY, key, fillna = TRUE)
  f72 <- getDataMBLS("CUUR0000SS09021", 1995, endY, key, fillna = TRUE)
  
  # Butter
  F8 <- getDataMBLS("CUUR0000SS10011", 1920, endY, key, fillna = TRUE)
  
  # Cheese
  f90 <- getDataMBLS("MUUR0000SA1113", 1930, 1990, key, fillna = TRUE)
  f91 <- getDataMBLS("CUUR0000SEFJ02", 1970, endY, key, fillna = TRUE)
  
  # Fresh vegetables
  F10 <- getDataMBLS("CUUR0000SEFL", 1930, endY, key, fillna = TRUE)
  
  # Fresh fruit
  fws <- getDataMBLS("WPU0111", 1950, endY, key, fillna = TRUE)
  frt <- getDataMBLS("CUUR0000SEFK", 1950, 2000, key, fillna = TRUE)
  pws <- getDataMBLS("WPU011303", 1950, 2000, key, fillna = TRUE)
  prt <- getDataMBLS("CUUR0000SEFL01", 1950, 2000, key, fillna = TRUE)
  
  # Eggs
  F12 <- getDataMBLS("CUUR0000SEFH", 1920, endY, key, fillna = TRUE)
  
  # Other beverages (Tea)
  f131 <- getDataMBLS("MUUR0000SE17", 1930, 1999, key, fillna = TRUE)
  f132 <- getDataMBLS("CUUR0000SEFP02", 1980, endY, key, fillna = TRUE)
  
  # Coffee
  f141 <- getDataMBLS("MUUR0000SS17031", 1930, 1999, key, fillna = TRUE)
  f142 <- getDataMBLS("CUUR0000SEFP01", 1980, endY, key, fillna = TRUE)
  
  # Margarine
  F15 <- getDataMBLS("CUUR0000SS16011", 1930, endY, key, fillna = TRUE)
  
  # Sugar and Sweets
  F16 <- getDataMBLS("CUUR0000SEFR", 1930, endY, key, fillna = TRUE)
  
  # Women's apparel
  c11 <- getDataMBLS("MUUR0000SA3112", 1930, 1999, key, fillna = TRUE)
  c12 <- getDataMBLS("CUUR0000SEAC", 1990, endY, key, fillna = TRUE)
  
  # Men's apparel
  c21 <- getDataMBLS("MUUR0000SA3111", 1930, 1999, key, fillna = TRUE)
  c22 <- getDataMBLS("CUUR0000SEAA", 1990, endY, key, fillna = TRUE)
  
  # Footwear
  c31 <- getDataMBLS("MUUR0000SE40", 1930, 1999, key, fillna = TRUE)
  c32 <- getDataMBLS("CUUR0000SEAE", 1990, endY, key, fillna = TRUE)
  
  # Personal care products (Soap)
  h11 <- getDataMBLS("MUUR0000SE64", 1930, 1999, key, fillna = TRUE)
  h12 <- getDataMBLS("CUUR0000SEGB01", 1980, endY, key, fillna = TRUE)
  h13 <- getDataMBLS("CUUR0000SEGB02", 1980, endY, key, fillna = TRUE)
  h14 <- getDataMBLS("CUUR0000SEGB", 1980, endY, key, fillna = TRUE)
  
  # Housekeeping supplies
  h20 <- getDataMBLS("MUUR0000SS33011", 1950, 1999, key, fillna = TRUE)
  h21 <- getDataMBLS("MUUR0000SE33", 1950, 1999, key, fillna = TRUE)
  h22 <- getDataMBLS("CUUR0000SEHN", 1980, endY, key, fillna = TRUE)
  
  # Textile furnishings
  h31 <- getDataMBLS("MUUR0000SE28", 1950, 1999, key, fillna = TRUE)
  h32 <- getDataMBLS("CUUR0000SEHH", 1980, endY, key, fillna = TRUE)
  
  # Energy
  e10a <- getDataMBLS("MUURA102SA22", 1940, 1999, key, fillna = TRUE)
  e10b <- getDataMBLS("MUURA102SE25", 1940, 1999, key, fillna = TRUE)
  e11 <- getDataMBLS("MUUR0000SACE", 1950, 1999, key, fillna = TRUE)
  e12 <- getDataMBLS("CUUR0000SACE", 1980, endY, key, fillna = TRUE)
  
  # Newspapers
  o11 <- getDataMBLS("MUUR0000SE5901", 1930, 1999, key, fillna = TRUE)
  o12 <- getDataMBLS("CUUR0000SERG01", 1980, endY, key, fillna = TRUE)
  
  # Household repair services
  o20 <- getDataMBLS("MUSR0000SAS", 1950, 1999, key, fillna = TRUE)
  o21 <- getDataMBLS("MUUR0000SE23", 1950, 1999, key, fillna = TRUE)
  o22 <- getDataMBLS("CUUR0000SEHP04", 1980, endY, key, fillna = TRUE)
  
  # Medical care services 
  o31 <- getDataMBLS("MUUR0000SA512", 1950, 1999, key, fillna = TRUE)
  o32 <- getDataMBLS("CUUR0000SAM2", 1980, endY, key, fillna = TRUE)
  o33 <- getDataMBLS("CUUR0100SAM2", 1999, endY, key, fillna = TRUE)
  
  # Rent
  R1 <- getDataMBLS("CUUR0000SEHA", 1930, endY, key, fillna = TRUE)
  
  # Data for proxy (modern)
  ppiF <- ts_xts(ts_fred("WPU02"))
  ppiC <- ts_xts(ts_fred("WPU0381")) 
  ppiE <- ts_xts(ts_fred("PPIENG"))
  ppiH <- ts_xts(ts_fred("WPU0382"))
  
  # Data for proxy (historical: Warren and Pearson linked with Hanes replication)
  DataHist = read.csv("./Data/RawDataProxy.csv")

  Date <- as.Date(paste(as.character(DataHist[,1]), "-01-01", sep = ""))
  ppiAllh <- xts(DataHist[, 2], order.by = Date)
  ppiFh <- xts(DataHist[, 3], order.by = Date)
  ppiCh <- xts(DataHist[, 4], order.by = Date)
  ppiHh <- xts(as.numeric(xts(DataHist[, 5], order.by = Date)), order.by = Date)
  ppiEh <- xts(DataHist[, 6], order.by = Date)
  rm(DataHist)
  
  # Get aggregate data for comparison
  CPI <- ts_xts(ts_fred('CPIAUCNS'))
  PPI <- ts_xts(ts_fred('PPIACO'))
  
  # Data for proxy (historical: Warren and Pearson linked with Hanes replication)
  DataHist = read.csv("./Data/CompositeCPIMeasuringWorth.csv")
  
  Date <- as.Date(paste(as.character(DataHist[,1]), "-01-01", sep = ""))
  cpiAllh <- xts(DataHist[, 2], order.by = Date)
  rm(DataHist)
  
  save.image(file="./Data/BLSData.RData") 

}else{
  load(file="./Data/BLSData.RData")
}

# ------------------------------------------------------------------------
# 2) Construct the modern subindices. See Online Appendix Tab. B3 
# ------------------------------------------------------------------------
# F1: Flour
# ------------------------------------------------------------------------
F1w <- cbind(9.7, 0.04)

# Start and end dates
f10[1]
f11[1]
f12[1]

startNew <- as.Date(index(f12[1]))
startOld <- as.Date(index(f11[1]))

# Calculate linked series
F1 <- ts_chain(ts_span(f10, NULL, startOld), f11)
F1 <- ts_chain(ts_span(F1, NULL, startNew), f12)

# Plot
ts_plot(
  `Flour chained` = F1, 
  `Cereals and pastry products old`= f10,
  `Flour old`= f11,
  `Flour new` = f12,
  title = "F1: Flour"
)
ts_save(filename = "./Results/Fig_B1//F1_Flour.pdf", width = figw, height = figh, open = FALSE)


# F2: Rice, pasta, cornmeal
# ------------------------------------------------------------------------
F2w <- cbind(1.3, 0.119)

# Start and end dates
f20[1]
f21[1]
f22[1]

startNew <- as.Date(index(f22[1]))
startOld <- as.Date(index(f21[1]))

# Calculate linked series
f21b <- ts_chain(ts_span(f20, startOld), f21)
F2 <- ts_chain(ts_span(f21b, NULL, startNew), f22)

# Plot
ts_plot(
  `Rice chained` = F2, 
  `Cereals and pastry products old`= f20,
  `Rice old`= f21,
  `Rice new` = f22,
  title = "F2: Rice, pasta, cornmeal"
)
ts_save(filename = "./Results/Fig_B1//F2_Rice.pdf", width = figw, height = figh, open = FALSE)


# F3: Beef and veal
# ------------------------------------------------------------------------
F3w <- cbind(7.9, 0.436)

# Calculate linked series

# Plot
ts_plot(
  `Beef and veal chained` = F3, 
  title = "F3: Beef and veal"
)
ts_save(filename = "./Results/Fig_B1//F3_Beef.pdf", width = figw, height = figh, open = FALSE)


# F4: Pork
# ------------------------------------------------------------------------
F4w <- cbind(2.8, 0.3)

# Calculate linked series
F4 <- ts_chain(f41, f42)

# Plot
ts_plot(
  `Pork chained` = F4, 
  `Pork old`= f41,
  `Pork new` = f42,
  title = "F4: Pork"
)
ts_save(filename = "./Results/Fig_B1//F4_Pork.pdf", width = figw, height = figh, open = FALSE)


# F5: Mutton
# ------------------------------------------------------------------------
F5w <- cbind(1.5, 0.227)

# Start and end dates
f50[1]
f51[1]
startNew <- as.Date(index(f51[1]))

# Calculate linked series
F5 <- ts_chain(ts_span(f50, NULL, startNew), f51)

# Plot
ts_plot(
  `Other meat chained` = F5, 
  `Meats old` = f50, 
  `Other meat` = f51, 
  title = "F5: Other meat"
)
ts_save(filename = "./Results/Fig_B1//F5_OtherMeat.pdf", width = figw, height = figh, open = FALSE)


# F6: Fish and seafood
# ------------------------------------------------------------------------
F6w <- cbind(1.4, 0.25)

# Calculate linked series

# Plot
ts_plot(
  `Fish and seafood chained` = F6, 
  title = "F6: Fish and seafood"
)
ts_save(filename = "./Results/Fig_B1//F6_Fish.pdf", width = figw, height = figh, open = FALSE)


# F7: Fresh whole milk
# ------------------------------------------------------------------------
F7w <- cbind(3, 0.207)

# Calculate linked series

# Plot
ts_plot(
  `Fresh whole milk chained` = ts_index(F7, "2010-01-01"), 
  `Other milk (not used)` =  ts_index(f72, "2010-01-01"), 
  
  title = "F7: Fresh whole milk"
)
ts_save(filename = "./Results/Fig_B1//F7_Milk.pdf", width = figw, height = figh, open = FALSE)


# F8: Butter
# ------------------------------------------------------------------------
F8w <- cbind(5.9, 0.0305)

# Plot
ts_plot(
  `Butter chained` = ts_index(F8, "2010-01-01"), 
  title = "F8: Butter"
)
ts_save(filename = "./Results/Fig_B1//F8_Butter.pdf", width = figw, height = figh, open = FALSE)


# F9: Cheese
# ------------------------------------------------------------------------
F9w <- cbind(0.5, 0.242)

# Start and end dates
f90[1]
f91[1]
startNew <- as.Date(index(f91[1]))

# Calculate linked series
F9 <- ts_chain(ts_span(f90, NULL, startNew), f91)

# Plot
ts_plot(
  `Cheese chained` = F9, 
  `Dairy products` = f90, 
  `Cheese` = f91, 
  title = "F9: Cheese"
)
ts_save(filename = "./Results/Fig_B1//F9_Cheese.pdf", width = figw, height = figh, open = FALSE)


# F10: Fresh vegetables
# ------------------------------------------------------------------------
F10w <- cbind(7.0, 0.478)

# Plot
ts_plot(
  `Fresh vegetables chained` = F10, 
  title = "F10: Fresh vegetables"
)
ts_save(filename = "./Results/Fig_B1//F10_Vegetables.pdf", width = figw, height = figh, open = FALSE)


# F11: Fresh fruit
# ------------------------------------------------------------------------
# Use fruits at the wholesale stage and adapt volatility by estimating the
# relationship between wholesale and retail potatoes.
# Use sample from 1950-2000 because of large outlier in potato wholesale price

# Plot
ts_plot(
  `Potato wholesale` = log(ts_index(pws, "1980-01-01")), 
  `Potato retail` = log(ts_index(prt, "1980-01-01")), 
  title = "Tests: Potatoes wholesale and retail prices",
  subtitle="in logs, not seasonally adjusted"
)
ts_save(filename = "./Results/Fig_B1//F11_Potatos_nsa.pdf", width = figw, height = figh, open = FALSE)

# Seasonally adjust potatoes to estimate relative variance of growth rates
prt <- final(seas(ts_ts(prt)))
pws <- final(seas(ts_ts(pws)))

# Plot
ts_plot(
  `Potato wholesale` = log(ts_index(pws, "1980-01-01")), 
  `Potato retail` = log(ts_index(prt, "1980-01-01")), 
  title = "Tests: Potatoes wholesale and retail prices",
  subtitle="in logs, seasonally adjusted"
)
ts_save(filename = "./Results/Fig_B1//F11_Potatos_sa.pdf", width = figw, height = figh, open = FALSE)

#FruitData <- data.frame(ts_c(ts_pc(fws), ts_pc(pws), lag(ts_pc(pws),1), lag(ts_pc(pws),2), ts_pc(prt)))
#colnames(FruitData)[1:5] <- c("Fruit", "PotWs", "PotWs.l", "PotWs.l2", "PotRt")
#regEq <- lm(PotRt ~ PotWs, data=FruitData) 
#summary(regEq)
#checkresiduals(regEq)

# Change variance while keeping the mean unchanged. First, check adjustment with potatoes
pwsSd <- sd(ts_pc(pws), na.rm=TRUE)
prtSd <- sd(ts_pc(prt), na.rm=TRUE)
pwsMn <- mean(ts_pc(pws), na.rm=TRUE)
prtMn <- mean(ts_pc(prt), na.rm=TRUE)

# This creates growth rates of wholesale prices with the same volatility of retail prices but the same mean growth rate as wholesale prices
pwsAdj <- (ts_pc(pws)-pwsMn)/pwsSd*prtSd+pwsMn/pwsSd*prtSd
pwsAdjLev <- ts_compound(pwsAdj)

# Plot
ts_plot(
  `Potato wholesale` = log(ts_index(pws, "1980-01-01")), 
  `Potato retail` = log(ts_index(prt, "1980-01-01")), 
  `Potato wholesale adjusted` = log(ts_index(pwsAdjLev, "1980-01-01")), 
  title = "Tests: Potatoes wholesale and retail prices",
  subtitle = "in logs"
)
ts_save(filename = "./Results/Fig_B1//F11_PotatoesCheck.pdf", width = figw, height = figh, open = FALSE)

# Do the same for fruit wholesale and retail
# But, no need to seasonally adjust because adjustment factor remains the same anyways
fwsSd <- sd(ts_pc(fws), na.rm =TRUE)
frtSd <- sd(ts_pc(frt), na.rm =TRUE)
fwsMn <- mean(ts_pc(fws), na.rm =TRUE)

# Compare relationship volatility
print(paste("Relative volatility fruit: ", fwsSd/frtSd, "   Relative volatility potato: ", pwsSd/prtSd, sep =""))

# This creates growth rates of wholesale prices with the same volatility of retail prices but the same mean growth rate as wholesale prices
fwsAdj <- (ts_pc(fws)-fwsMn)/pwsSd*prtSd+fwsMn/pwsSd*prtSd
fwsAdjLev <- ts_compound(fwsAdj)

# Plot
ts_plot(
  `Fruit wholesale` = (ts_index(fws, "1980-01-01")), 
  `Fruit retail` = (ts_index(frt, "1980-01-01")), 
  `Fruit wholesale adjusted` = (ts_index(fwsAdjLev, "1980-01-01")), 
  title = "F10: Fruit wholesale and retail prices"
)
ts_save(filename = "./Results/Fig_B1//F11_FruitCheck.pdf", width = figw, height = figh, open = FALSE)

# Save the final series for later use
F11w <- cbind(2.5, 0.555)
F11 <- fwsAdjLev


# F12: Eggs
# ------------------------------------------------------------------------
F12w <- cbind(2.3, 0.097)

# Plot
ts_plot(
  `Eggs chained` = ts_index(F12, "2010-01-01"), 
  title = "F12: Eggs"
)
ts_save(filename = "./Results/Fig_B1//F12_Eggs.pdf", width = figw, height = figh, open = FALSE)


# F13: Other beverages (Tea)
# ------------------------------------------------------------------------
F13w <- cbind(1.3, 0.093)

# Calculate linked series
F13 <- ts_chain(f131, f132)

# Plot
ts_plot(
  `Other beverages (Tea) chained` = F13, 
  `Nonalcoholic beverages`= f131,
  `Other beverage materials including tea` = f132,
  title = "F13: Other beverages (Tea)"
)
ts_save(filename = "./Results/Fig_B1//F13_Tea.pdf", width = figw, height = figh, open = FALSE)


# F14: Coffee
# ------------------------------------------------------------------------
F14w <- cbind(4.0, 0.168)

# Calculate linked series
F14 <- ts_chain(f141, f142)

# Plot
ts_plot(
  `Coffee chained` = F14, 
  `Roasted coffee`= f141,
  `Coffee` = f142,
  title = "F14: Coffee"
)
ts_save(filename = "./Results/Fig_B1//F14_Coffee.pdf", width = figw, height = figh, open = FALSE)


# F15: Margarine
# ------------------------------------------------------------------------
F15w <- cbind(1.5, 0.0305)

# Plot
ts_plot(
  `Margarine chained` = F15, 
  title = "F15: Margarine"
)
ts_save(filename = "./Results/Fig_B1//F15_Margarine.pdf", width = figw, height = figh, open = FALSE)


# F16: Sugar and sweets
# ------------------------------------------------------------------------
F16w <- cbind(4.8, 0.282)

# Plot
ts_plot(
  `Sugar and sweets chained` = F16, 
  title = "F16: Sugar and sweets"
)
ts_save(filename = "./Results/Fig_B1//F16_Sugar.pdf", width = figw, height = figh, open = FALSE)


# C1: Women's apparel
# ------------------------------------------------------------------------
C1w <- cbind(3.4, 1.044)

# Calculate linked series
C1 <- ts_chain(ts_span(c11, NULL, ts_summary(c12)$start), c12)

# Plot
ts_plot(
  `Women's apparel chained` = C1, 
  `Women's and girls' apparel`= c11,
  `Women's apparel` = c12,
  title = "C1: Women's apparel"
)
ts_save(filename = "./Results/Fig_B1//C1_WomenApparel.pdf", width = figw, height = figh, open = FALSE)


# C2: Men's apparel
# ------------------------------------------------------------------------
C2w <- cbind(3.6, 0.581)

# Calculate linked series
C2 <- ts_chain(ts_span(c21, NULL, ts_summary(c22)$start), c22)

# Plot
ts_plot(
  `Men's apparel chained` = C2, 
  `Men's apparel`= c21,
  `Men's apparel` = c22,
  title = "C2: Men's apparel"
)
ts_save(filename = "./Results/Fig_B1//C2_MenApparel.pdf", width = figw, height = figh, open = FALSE)


# C3: Footwear
# ------------------------------------------------------------------------
C3w <- cbind(4.0, 0.671)

# Calculate linked series
C3 <- ts_chain(ts_span(c31, NULL, ts_summary(c32)$start), c32)

# Plot
ts_plot(
  `Footwear chained` = C3, 
  `Footwear`= c31,
  `Footwear` = c32,
  title = "C3: Footwear"
)
ts_save(filename = "./Results/Fig_B1//C3_Footwear.pdf", width = figw, height = figh, open = FALSE)


# H1: Personal care products (Soap)
# ------------------------------------------------------------------------
H1w <- cbind(0.3, 0.704)

# Calculate linked series (with linear approximation for six missing months on database)
H1 <- ts_bind(h11, h14)
H1 <- ts_xts(na.approx(ts_ts(H1)))

# Plot
ts_plot(
  #`Footwear chained` = H1, 
  `Personal care products chained`= H1,
  `Toilet goods and personal care appliances`= h11,
  `Hair, dental, shaving, and miscellaneous personal care products`= h12,
  `Cosmetics, perfume, bath, nail preparations and implements`= h13,
  `Personal care products` = h14,
  title = "H1: Personal care products (Soap)"
)
ts_save(filename = "./Results/Fig_B1//H1_PersonalCare.pdf", width = figw, height = figh, open = FALSE)


# H2: Housekeeping supplies (Starch)
# ------------------------------------------------------------------------
H2w <- cbind(0.2, 0.835)

# Start and end dates
h20[1]
h21[1]
h22[1]
startNew <- as.Date(index(h22[1]))
startOld <- as.Date(index(h21[1]))
valOld <- h21[startOld]

# Calculate linked series and then reeindex to old value
h21b <- ts_chain(ts_span(h20, NULL, startOld), h21)
valNew <- h21b[startOld]
h21b <- h21b/as.numeric(valNew[1])*as.numeric(valOld[1])

# Calculate linked series (with linear approximation for six missing months on database)
H2 <- ts_bind(h21b, h22)
H2 <- ts_xts(na.approx(ts_ts(H2)))

# Plot
ts_plot(
  `Housekeeping supplies chained`= H2,
  `Soaps and detergents`= h20,
  `Housekeeping supplies`= h21,
  `Housekeeping supplies`= h22,
  title = "H2: Housekeeping supplies (Starch)"
)
ts_save(filename = "./Results/Fig_B1//H2_HousekeepingSupplies.pdf", width = figw, height = figh, open = FALSE)


# H3: Textile furnishings
# ------------------------------------------------------------------------
H3w <- cbind(3.5, 0.261)

# Calculate linked series (with linear approximation for two years on database)
# Note we have to repace first series to DECEMBER 1997=100.
H3 <- ts_bind(ts_index(h31, "1997-12-01")*100, h32)
H3 <- ts_xts(na.approx(ts_ts(H3)))

# Plot
ts_plot(
  `Textile house furnishings chained`= H3,
  `Textile housefurnishings`= h31,
  `Window and floor coverings and other linens` =h32,
  title = "H3: Textile house furnishings"
)
ts_save(filename = "./Results/Fig_B1//H3_TextileFurnishings.pdf", width = figw, height = figh, open = FALSE)


# E1: Energy commodities (Fuel and Light)
# ------------------------------------------------------------------------
E1w <- cbind(7, 4.679)

# Start and end dates
e10a[1]
e10b[1]
e11[1]
e12[1]

startNew <- as.Date(index(e11[1]))
valOld <- e11[startNew]

# Calculate linked series and then reeindex to old value
e11b <- ts_chain(ts_span(e10b, NULL, startNew), e11)
valNew <- e11b[startNew]
e11b <- e11b/as.numeric(valNew[1])*as.numeric(valOld[1])

# Calculate linked series (with linear approximation for two years on database)
# Note we have to repace first series to DECEMBER 1997=100.
E1 <- ts_bind(e11b, e12)
E1 <- ts_xts(na.approx(ts_ts(E1)))

# Plot
ts_plot(
  `Energy commodities chained`= E1,
 # `Fuels and other commodities`= e10a,
  `Fuel oil and other household fuel commodities`= e10b,
  `Energy commodities`= e11,
  `Energy commodities` =e12,
  title = "E1: Energy commodities (fuel and light)"
)
ts_save(filename = "./Results/Fig_B1//E1_Energy.pdf", width = figw, height = figh, open = FALSE)


# O1: Newspapers
# ------------------------------------------------------------------------
O1w <- cbind(1.1, 0.067)

# Calculate linked series (with linear approximation for two years on database)
# Note we have to repace first series to DECEMBER 1997=100.
O1 <- ts_bind(ts_index(o11, "1997-12-01")*100, o12)
O1 <- ts_xts(na.approx(ts_ts(O1)))

# Plot
ts_plot(
  `Newspapers chained`= O1,
  `Newspapers`= o11,
  `Newspapers and magazines` =o12,
  title = "O1: Newspapers"
)
ts_save(filename = "./Results/Fig_B1//O1_Newspapers.pdf", width = figw, height = figh, open = FALSE)


# O2: Repair of household items (Shoe repair)
# ------------------------------------------------------------------------
O2w <- cbind(0.7, 0.104)

# Start and end dates
o20[1]
o21[1]
o22[1]
startNew <- as.Date(index(o21[1]))

# Calculate linked series
o21b <- ts_chain(ts_span(o20, NULL, startNew), o21)

# Link the series
O2 <- ts_chain(o21b, o22)

# Plot
ts_plot(
  `Repair of household items chained`= O2,
  `Services`= o20,
  `Household maintenance and repair services`= o21,
  `Repair of household items` =o22,
  title = "O2: Repair of household items (Shoe repair)"
)
ts_save(filename = "./Results/Fig_B1//O2_Repair.pdf", width = figw, height = figh, open = FALSE)


# O3: Medical care services
# ------------------------------------------------------------------------
O3w <- cbind(1.1, 6.924)


# Link the series
O3 <- ts_chain(o31, o32)

# Plot
ts_plot(
  `Medical care services chained`= O3,
  `Medical care services`= o31,
  `Medical care services` =o32,
  `Medical care services Northeast US` =o33,
  title = "O3: Medical care services"
)
ts_save(filename = "./Results/Fig_B1//O3_MedicalCare.pdf", width = figw, height = figh, open = FALSE)


# R1: Rent
# ------------------------------------------------------------------------
R1w <- cbind(17.7, 30.099)

# Plot
ts_plot(
  `Rent chained`= R1,
  title = "R1: Rent"
)
ts_save(filename = "./Results/Fig_B1//R1_Rent.pdf", width = figw, height = figh, open = FALSE)


# ppiF: Food
# ------------------------------------------------------------------------
ppiFw <- cbind(57.4, 13.384)

# Plot
ts_plot(
  `PPI food`= ppiF,
  title = "PPI food"
)
ts_save(filename = "./Results/Fig_B1//ppiF_Food.pdf", width = figw, height = figh, open = FALSE)


# ppiC: Clothing (Apparel)
# ------------------------------------------------------------------------
ppiCw <- cbind(11.0,3.018)

# Plot
ts_plot(
  `PPI clothing`= ppiC,
  title = "PPI clothing"
)
ts_save(filename = "./Results/Fig_B1//ppiC_Clothing.pdf", width = figw, height = figh, open = FALSE)


# ppiH: Household commodities (textile house furnishings)
# ------------------------------------------------------------------------
ppiHw <- cbind(4.0, 1.778)

# Plot
ts_plot(
  `PPI textile house furnishings`= ppiH,
  title = "PPI textile house furnishings"
)
ts_save(filename = "./Results/Fig_B1//ppiH_HouseFurnishings.pdf", width = figw, height = figh, open = FALSE)


# ppiE: Energy (fuels and related products and power)
# ------------------------------------------------------------------------
ppiEw <- cbind(7.0,	4.634)

# Plot
ts_plot(
  `PPI fuel and related`= ppiE,
  title = "PPI fuel and related"
)
ts_save(filename = "./Results/Fig_B1//ppiE_Fuel.pdf", width = figw, height = figh, open = FALSE)


# ppiHh: Household furnishings historical missing data (link with best match)
# ------------------------------------------------------------------------
startNew <- as.Date(index(ts_na_omit(ppiHh)[1]))

# Calculate linked series
ppiHh <- ts_chain(ts_span(ppiCh, NULL, startNew), ppiHh)

# Plot
ts_plot(
  `Household furnishings`= ppiHh,
 # `Foods`= ppiFh,
  `Clothing`= ppiCh,
#  `Energy`= ppiEh,
  `All commodities`= ppiAllh,
  title = "ppiHh: Household furnishings (historical)"
)
ts_save(filename = "./Results/Fig_B1//ppiHh_Household.pdf", width = figw, height = figh, open = FALSE)


# All: CPI all items (for comparison)
# ------------------------------------------------------------------------
# Plot
ts_plot(
  `All items` = CPI, 
  title = "All items"
)
ts_save(filename = "./Results/Fig_B1//AllItems.pdf", width = 8, height = 5, open = FALSE)

# ------------------------------------------------------------------------
# 3) Aggregate to replication CPI with fixed weights (historical and modern)
# ------------------------------------------------------------------------
# CPI replication
P <- ts_c(F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, C1, C2, C3, H1, H2, H3, E1, O1, O2, R1)
W <- rbind(F1w, F2w, F3w, F4w, F5w, F6w, F7w, F8w, F9w, F10w, F11w, F12w, F13w, F14w, F15w, F16w, C1w, C2w, C3w, H1w, H2w, H3w, E1w, O1w, O2w, R1w)
rownames(W) <- colnames(P)
colnames(W) <- c("Hoover", "2018")

CPIw19 <- calcIndex(P, W[, 1], baseY = "2010-12-01")
CPIw21 <- calcIndex(P, W[, 2], baseY = "2010-12-01")

CPIw19 <- ts_span(CPIw19, "1960-01-01")
CPIw21 <- ts_span(CPIw21, "1960-01-01")

# Actual CPI and PPI data from BLS
CPI <- ts_span(CPI, "1960-01-01")
PPI <- ts_span(PPI, "1960-01-01")

# ------------------------------------------------------------------------
# 4) Construct proxy
# ------------------------------------------------------------------------
# Proxy
P  <- ts_c(ppiF, ppiC, ppiH, ppiE)
Ph <- ts_c(ppiFh, ppiCh, ppiHh, ppiEh)
W  <- rbind(ppiFw, ppiCw, ppiHw, ppiEw)
rownames(W) <- colnames(P)
colnames(W) <- c("Hoover", "2018")

# Modern replications
proxyw19 <- calcIndex(P, W[, 1], baseY = "2010-12-01")
proxyw21 <- calcIndex(P, W[, 2], baseY = "2010-12-01")

proxyw19 <- ts_span(proxyw19, "1960-01-01")
proxyw21 <- ts_span(proxyw21, "1960-01-01")

# Historical proxy
proxyhw19 <- calcIndex(Ph, W[, 1], baseY = "1890-01-01")

# ------------------------------------------------------------------------
# 5) Plots for paper in annual frequency
# ------------------------------------------------------------------------
# Plots for paper in annual frequency (Modern)
CPIa <- ts_pcy(ts_frequency(CPI, to="year", aggregate = "mean"))
PROXa <- ts_pcy(ts_frequency(proxyw21, to="year", aggregate = "mean"))
REPa <- ts_pcy(ts_frequency(CPIw21, to="year", aggregate = "mean"))
date <- as.Date(index(REPa))

data.df <- data.frame(date, CPIa, PROXa, REPa)
colnames(data.df) <- c("date", "CPI", "Proxy", "Replication")
melted.df = melt(data.df, id.vars="date")

g <- ggplot(data=melted.df, aes(x=date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line()
g <- g + scale_color_manual(values=c("blue", "red", "black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + ggtitle("") + ylab("Inflation, in %") + xlab("")
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1960-01-01"), as.Date("2017-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 20))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.65,.65))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_2b.pdf", width = 14, height = 9, units="cm")

# Plots for paper in annual frequency (Historical)
CPIa <- ts_pcy(cpiAllh)
PROXa <- ts_pcy(proxyhw19)
date <- as.Date(index(CPIa))

data.df <- data.frame(date, ts_c(CPIa, PROXa))
colnames(data.df) <- c("date", "CPI", "Proxy")
melted.df = melt(data.df, id.vars="date")

g <- ggplot(data=melted.df, aes(x=date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + scale_color_manual(values=c("blue", "red", "black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + ggtitle("") + ylab("Inflation, in %") + xlab("")
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1800-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-20, 40))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.65))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_2a.pdf", width = 14, height = 9, units="cm")


# ------------------------------------------------------------------------
# 6) Descriptive statistics used in text
# ------------------------------------------------------------------------
# Descriptive statistics for comparison in text
mean(CPIa["1800-01-01/1900-01-01",], na.rm=TRUE)
sd(CPIa["1800-01-01/1900-01-01",], na.rm=TRUE)

mean(PROXa["1800-01-01/1900-01-01",], na.rm=TRUE)
sd(PROXa["1800-01-01/1900-01-01",], na.rm=TRUE)

cor(CPIa["1800-01-01/1900-01-01",], PROXa["1800-01-01/1900-01-01",], use = "pairwise.complete.obs")

def_proxy <- PROXa["1800-01-01/1900-01-01",]<0
def_cpi <- CPIa["1800-01-01/1900-01-01",]<0

inf_proxy <- PROXa["1800-01-01/1900-01-01",]>=0
inf_cpi <- PROXa["1800-01-01/1900-01-01",]>=0

mean(def_proxy != def_cpi, na.rm = TRUE)

# ------------------------------------------------------------------------
# 6) Compute misclassification rates from 1960-2017 (Table 1)
# ------------------------------------------------------------------------
# x is the CPI deflation indicator, z the proxy deflation indicator, d the true deflation indicator
# Using modern weights
CPIa <- ts_pcy(ts_frequency(CPI, to="year", aggregate = "mean"))
PROXa <- ts_pcy(ts_frequency(proxyw21, to="year", aggregate = "mean"))
REPa <- ts_pcy(ts_frequency(CPIw21, to="year", aggregate = "mean"))
d <- (ts_span(CPIa, "1960-01-01", "2017-01-01") < 0)
x <- (ts_span(REPa, "1960-01-01", "2017-01-01") < 0)
z <- (ts_span(PROXa, "1960-01-01", "2017-01-01") < 0)

Px0d1 <- mean(x[d==1]==0)
Px1d0 <- mean(x[d==0]==1)

Pz0d1 <- mean(z[d==1]==0)
Pz1d0 <- mean(z[d==0]==1)

Pxz0d1 <- mean(z[d==1]==0 & x[d==1]==0)
Pxz1d0 <- mean(z[d==0]==1 & x[d==0]==1)

Table <- as.matrix(cbind(Px0d1, Px1d0, Pz0d1, Pz1d0, Px0d1*Pz0d1,  Px1d0*Pz1d0, Pxz0d1, Pxz1d0))

# Demeaned data
mCPIa <- mean(CPIa, na.rm = TRUE)
CPIa <- CPIa
PROXa <- PROXa
REPa <- REPa
d <- (ts_span(CPIa, "1960-01-01", "2017-01-01") < mCPIa)
x <- (ts_span(REPa, "1960-01-01", "2017-01-01") < mCPIa)
z <- (ts_span(PROXa, "1960-01-01", "2017-01-01") < mCPIa)

Px0d1 <- mean(x[d==1]==0)
Px1d0 <- mean(x[d==0]==1)

Pz0d1 <- mean(z[d==1]==0)
Pz1d0 <- mean(z[d==0]==1)

Pxz0d1 <- mean(z[d==1]==0 & x[d==1]==0)
Pxz1d0 <- mean(z[d==0]==1 & x[d==0]==1)

Table <- rbind(Table, as.matrix(cbind(Px0d1, Px1d0, Pz0d1, Pz1d0, Px0d1*Pz0d1,  Px1d0*Pz1d0, Pxz0d1, Pxz1d0)))

# Print the table
colnames(Table) <- c("$x=0|d=1$","$x=1|d=0$", "$z=0|d=1$","$z=1|d=0$", "prod|d=0", "prod|d=1" , "$x=z=0|d=1$","$x=z=1|d=0$")
rownames(Table) <- c("1960-2017 zero threshold", "1960-2017 higher threshold")
xTable <- xtable(Table, digits = 2, caption = "Misclassification rates", label = "tab:Misclassification")
align(xTable) <- c("l", "c", "c", "c", "c", "c", "c", "c", "c")
print(xTable,sanitize.text.function=function(x){x}, caption.placement = "top",booktabs = T)
write.table(xTable, "./Results/Tab_1.txt")

# ------------------------------------------------------------------------
# 7) Compute descriptive statistics of measurement error (Table B4)
# ------------------------------------------------------------------------
# Table with descriptive statistics
# The following is only used for calibratig the simulation exercise to test the codes
GDPD <- ts_fred('GDPDEF')
GDPD <- ts_pcy(ts_frequency(GDPD, to="year", aggregate = "mean"))
GDPD <- ts_xts(GDPD)
GDPD <- ts_span(GDPD, "1960-01-01", "2017-01-01")

NGDP <- ts_fred('GDPA')
NGDP <- ts_pcy(ts_frequency(NGDP, to="year", aggregate = "mean"))
NGDP <- ts_xts(NGDP)
NGDP <- ts_span(NGDP, "1960-01-01", "2017-01-01")

RGDP <- ts_fred('GDPCA')
RGDP <- ts_pcy(ts_frequency(RGDP, to="year", aggregate = "mean"))
RGDP <- ts_xts(RGDP)
RGDP <- ts_span(RGDP, "1960-01-01", "2017-01-01")

CPIa <- ts_pcy(ts_frequency(CPI, to="year", aggregate = "mean"))
PROXa <- ts_pcy(ts_frequency(proxyw21, to="year", aggregate = "mean"))
REPa <- ts_pcy(ts_frequency(CPIw21, to="year", aggregate = "mean"))

errx <- CPIa - REPa
errz <- CPIa - PROXa
errg <- GDPD - PROXa
  
ErrorDesc <- ts_c(CPIa, PROXa, REPa, errx, errz)
colnames(ErrorDesc) <- c("CPI", "Proxy", "Replication", "CPI-replication", "CPI-proxy")
ErrorDesc <- ts_span(ErrorDesc, "1960-01-01")
stargazer(as.data.frame(ErrorDesc), summary.stat = c("mean", "sd", "min", "max"), digits =2, out = "./Results/Tab_B4.txt")

vE <- var(errg, na.rm = TRUE)
vP <- var(GDPD, na.rm = TRUE)
sig2noise <- vP/vE
print(sig2noise)

# ------------------------------------------------------------------------
# 8) Do regressions properties of measurement error (Table B5)
# ------------------------------------------------------------------------

# Table with regressions
myData <- data.frame(REPa, PROXa, GDPD, errx, lag(errx), errz, lag(errz),errg, lag(errg), CPIa)
colnames(myData) <- c("replica", "proxy", "gdpd", "errx", "l.errx", "errz", "l.errz", "errg", "l.errg", "CPI")

CPIx <- lm(replica ~ CPI, data = myData)
CPIx.se <- sqrt(diag(NeweyWest(CPIx)))

CPIz <- lm(proxy ~ CPI, data = myData)
CPIz.se <- sqrt(diag(NeweyWest(CPIz)))

CPIg <- lm(proxy ~ gdpd, data = myData)
CPIg.se <- sqrt(diag(NeweyWest(CPIg)))

corxz <- lm(errx ~ errz+l.errz+l.errx, data = myData)
corxz.se <- sqrt(diag(NeweyWest(corxz)))

corzx <- lm(errz ~ errx+l.errx+l.errz, data = myData)
corzx.se <- sqrt(diag(NeweyWest(corzx)))

corgx <- lm(errg ~ errx+l.errx+l.errg, data = myData)
corgx.se <- sqrt(diag(NeweyWest(corgx)))

stargazer(CPIx, CPIz, corxz, corzx, title="Properties of replication and measurement errors", type = "text", out = "./Results/Tab_B5.txt",
          se=list(CPIx.se, CPIz.se, corxz.se, corzx.se),
          dep.var.labels = c("Replication", "Proxy", "CPI-replication", "CPI-proxy"),
          covariate.labels = c("CPI", "CPI-proxy", "CPI-replication",  "CPI-proxy (t-1)", "CPI-replication (t-1)", "Constant"),
          omit.stat = c("f", "ll", "sigma2", "ser", "adj.rsq"), label="tab:measerr",
          table.layout ="-d-t-s-n",
          model.numbers          = FALSE,
          font.size = "small",
          digits =2,
          notes        = "", style = "qje")


# ------------------------------------------------------------------------
# 7) Import remaining US data and export for STATA
# ------------------------------------------------------------------------
# Data for proxy (historical: Warren and Pearson linked with Hanes replication)
Data <- read.csv("./Data/RawDataUS.csv", skip = 1)

Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

RGDP <- ts_pcy(xts(Data$RGDP, order.by=Date))
GDPD <- ts_pcy(xts(Data$GDPD, order.by=Date))
RGDPPC <- ts_pcy(xts(Data$RGDPPC, order.by=Date))
CPIA <- ts_pcy(xts(Data$CPI, order.by=Date))
CPIF <- ts_pcy(xts(Data$CPIF, order.by=Date))
BANK <- xts(Data$BANK, order.by=Date)
IPROD <- ts_pcy(xts(Data$IPROD, order.by=Date))
IPFood <- ts_pcy(xts(Data$IPFood, order.by=Date))
IPTextiles <- ts_pcy(xts(Data$IPTextiles, order.by=Date))
IPLeather <- ts_pcy(xts(Data$IPLeather, order.by=Date))
IPChemicals <- ts_pcy(xts(Data$IPChemicals, order.by=Date))
IPMachinery <- ts_pcy(xts(Data$IPMachinery, order.by=Date))
IPMetals <- ts_pcy(xts(Data$IPMetals, order.by=Date))
IPWood <- ts_pcy(xts(Data$IPWood, order.by=Date))
PROXa <- ts_pcy(proxyhw19)

sd(ts_span(RGDP, "1800-01-01", "1899-01-01"))
sd(ts_span(IPROD, "1800-01-01", "1899-01-01"))

#Create error-ridden real GDP for the US
NGDP <- xts(Data$RGDP*Data$GDPD, order.by=Date)
RGDPPROXY <- ts_pcy(NGDP/proxyhw19)
RGDPCPI <- ts_pcy(NGDP/Data$CPI)

data.df <- data.frame(Date, ts_c(RGDP, RGDPPROXY, RGDPCPI))
colnames(data.df) <- c("date", "Real GDP", "NGPD/proxy", "NGDP/CPI")
melted.df = melt(data.df, id.vars="date")

g <- ggplot(data=melted.df, aes(x=date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + scale_color_manual(values=c("blue", "red", "black", "blue", "magenta", "indianred3", "lightgoldenrod3", "gray60"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + ggtitle("") + ylab("In %") + xlab("")
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1800-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-15, 30))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B3.pdf", width = 14, height = 12, units="cm")

SHARE <- ts_pcy(xts(Data$SHARE, order.by=Date))
M2 <- ts_pcy(xts(Data$M2, order.by=Date))
M3 <- ts_pcy(xts(Data$M3, order.by=Date))

mean(ts_span(M2, "1800-01-01", "1899-01-01"), na.rm =TRUE)

FiltGDP <- hpfilter(log(Data$RGDP[!is.na(Data$RGDP)]), freq = 100, type=c("lambda"))
FiltIP <- hpfilter(log(Data$IPROD[!is.na(Data$IPROD)]),freq = 100, type=c("lambda"))
GAPIP <- xts(FiltIP$cycle*100, order.by = Date[!is.na(Data$IPROD)])
GAPGDP <- xts(FiltGDP$cycle*100, order.by = Date[!is.na(Data$RGDP)])


# Export historical data for use in Stata
DataExport <- ts_c(xts(year(Date), order.by=(Date)), PROXa, RGDP, RGDPPROXY, RGDPCPI, GDPD, RGDPPC, GAPIP, GAPGDP, CPIA, CPIF, BANK, 
                   IPROD, IPFood, IPTextiles, IPLeather, IPChemicals, IPMachinery, IPWood, IPMetals, SHARE, M2, M3)
colnames(DataExport) <- c("date", "proxy", "rgdp", "rgdpprox", "rgdpcpi", "gdpd", "rgdppc", "gapip", "gap", "cpi", "cpif", "bank", "iprod", "ipfood", 
                          "iptex", "ipleat", "ipchem", "ipmach", "ipwood", "ipmet", "share", "m2", "m3")
write.xlsx(DataExport, "./Data/DataForStataUS.xlsx", sheetName="Sheet1")