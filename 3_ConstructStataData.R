# ------------------------------------------------------------------------
# Daniel Kaufmann, "Is Deflation Costly After All? The Perils of Erroneous 
#   Historical Classifications," Journal of Applied Econometrics, forthcoming
# ------------------------------------------------------------------------
#
# Constructs STATA data sets for all economies except for the US.
#
# ------------------------------------------------------------------------
# Daniel Kaufmann, 2020, daniel.kaufmann@unine.ch
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# 0) Define useful functions
# ------------------------------------------------------------------------
rm(list = ls())
library(jsonlite)
library(lubridate)
library(xts)
library(tsbox)
library(forecast)
library(seasonal)
library(quantmod)
library(ggplot2)   # Useful package for more sophisticated plots
library(reshape2)
library(xlsx)
library(xtable)
library(missMDA)
library(extrafont)


# ------------------------------------------------------------------------
# 1) Austria
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12
Data <- read.csv("./Data/RawDataAustria.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
MANUF <- ts_pcy(xts(Data$MANUF, order.by = Date))
GDP <- ts_pcy(xts(Data$GDPPC, order.by = Date))

cor(ts_c(CPI, GDP), use = "pairwise.complete.obs")
cor(ts_c(MANUF, GDP), use = "pairwise.complete.obs")
Data.df <- data.frame(Date, ts_c(CPI, GDEFL))
colnames(Data.df)<- c("Date", "CPI", "GDP deflator")
Data.df <- subset(Data.df, Date>="1871-01-01", Date<="1915-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Austria") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1871-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 10))+ scale_y_continuous(expand = c(0, 0))
g <- g + theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/AustriaInflation.pdf", width = figWidth, height = figHeight, units="cm")

Data.df <- data.frame(Date, ts_c(GDP, MANUF))
colnames(Data.df)<- c("Date", "GDP", "Manufacturing production")
Data.df <- subset(Data.df, Date>="1870-01-01", Date<="1915-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Austria") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1871-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/AustriaGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI), order.by=index(CPI)), CPI, GDEFL, GDP,  MANUF)
colnames(DataExport) <- c("date", "cpi", "gdefl", "gdp", "manuf")
write.xlsx(DataExport, "./Data/DataForStataAustria.xlsx", sheetName="Sheet1")

# ------------------------------------------------------------------------
# 2) Hungary
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12
Data <- read.csv("./Data/RawDataHungary.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
MANUF <- ts_pcy(xts(Data$MANUF, order.by = Date))
GDP <- ts_pcy(xts(Data$GDPPC, order.by = Date))

cor(ts_c(GDEFL, GDP), use = "pairwise.complete.obs")
cor(ts_c(MANUF, GDP), use = "pairwise.complete.obs")
Data.df <- data.frame(Date, ts_c(GDEFL))
colnames(Data.df)<- c("Date", "GDP deflator")
Data.df <- subset(Data.df, Date>="1870-01-01", Date<="1915-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Hungary") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1870-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-5, 10))+ scale_y_continuous(expand = c(0, 0))
g <- g + theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/HungaryInflation.pdf", width = figWidth, height = figHeight, units="cm")

Data.df <- data.frame(Date, ts_c(GDP, GDEFL))
colnames(Data.df)<- c("Date", "GDP", "MANUF")
colnames(Data.df)<- c("Date", "GDP", "Manufacturing production")
Data.df <- subset(Data.df, Date>="1870-01-01", Date<="1915-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Hungary") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1870-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-5, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/HungaryGDP.pdf", width = figWidth, height = figHeight, units="cm")


# Export historical data for use in Stata
DataExport <- ts_c(xts(year(GDEFL), order.by=index(GDEFL)), GDEFL, GDP, MANUF)
colnames(DataExport) <- c("date", "gdefl", "gdp", "manuf")
write.xlsx(DataExport, "./Data/DataForStataHungary.xlsx", sheetName="Sheet1")


# ------------------------------------------------------------------------
# 2) Argentina
# ------------------------------------------------------------------------
# Note: Not included in main analysis, because inflation exactly constant over long periods
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12
Data <- read.csv("./Data/RawDataArgentina.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
GDP <- ts_pcy(xts(Data$RGDPPC, order.by = Date))

Data.df <- data.frame(Date, ts_c(CPI, GDEFL))
colnames(Data.df)<- c("Date", "CPI", "GDP deflator")
Data.df <- subset(Data.df, Date>="1810-01-01", Date<="1915-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Argentina") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1810-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-50, 80))+ scale_y_continuous(expand = c(0, 0))
g <- g + theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/ArgentinaInflation.pdf", width = figWidth, height = figHeight, units="cm")

Data.df <- data.frame(Date, ts_c(GDP))
colnames(Data.df)<- c("Date", "GDP")
colnames(Data.df)<- c("Date", "GDP")
Data.df <- subset(Data.df, Date>="1810-01-01", Date<="1915-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Argentina") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1810-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-20, 35))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/ArgentinaGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(GDEFL), order.by=index(GDEFL)), GDEFL, GDP, CPI)
colnames(DataExport) <- c("date", "gdefl", "gdp", "cpi")
write.xlsx(DataExport, "./Data/DataForStataArgentina.xlsx", sheetName="Sheet1")


# ------------------------------------------------------------------------
# 3) Switzerland
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12
Data <- read.csv("./Data/RawDataSwitzerland.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

# Construct a proxy
Pmeat <- xts(Data$Pmeat, order.by = Date)
Pveget <- xts(Data$Pveget, order.by = Date)
Pheating <- xts(Data$Pheating, order.by = Date)
Ptextiles <- xts(Data$Ptextiles, order.by = Date)

Wmeat <- Data$Wmeat[1]
Wveget <- Data$Wveget[1]
Wheating <- Data$Wheating[1]
Wtextiles <- Data$Wtextiles[1]

# Calculate linked series (with linear approximation for two years on database)
Pmeat <- ts_xts(na.approx(ts_ts(Pmeat)))
Pveget <- ts_xts(na.approx(ts_ts(Pveget)))
Pheating <- ts_xts(na.approx(ts_ts(Pheating)))
Ptextiles <- ts_xts(na.approx(ts_ts(Ptextiles)))

# Calculate proxy
P <- ts_c(Pmeat, Pveget, Pheating, Ptextiles)
W <- rbind(Wmeat, Wveget, Wheating, Wtextiles)
rownames(W) <- colnames(P)
colnames(W) <- c("BaseW")
PROX <- calcIndex(P, W, "1850-01-01") 

# Other variables
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
WPI <- ts_pcy(xts(Data$WPI, order.by = Date))
PROX <- ts_pcy(PROX)
GDP1 <- ts_pcy(xts(Data$GDP1, order.by = Date))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
CPIDEFL <- ts_pcy(xts(Data$CPIH, order.by = Date))
NGDP <- ts_pcy(xts(Data$NGDP, order.by = Date))
GDP2 <- ts_pcy(xts(Data$GDPPC2, order.by = Date))

Test <- ts_c(CPI, PROX, NGDP, GDP2)
Test <- ts_span(Test, "1851-01-01", "1899-01-01")
cov(Test,  use = "pairwise.complete.obs")
cor(ts_c(CPI, GDP2), use = "pairwise.complete.obs")

# Plot
Data.df <- data.frame(Date, ts_c(PROX, CPI, GDEFL, CPIDEFL, WPI))
colnames(Data.df)<- c("Date", "Proxy", "CPI", "GDEFL", "CPIDEFL", "WPI")
Data.df <- subset(Data.df, Date>="1850-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Switzerland") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black", "gray60"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash", "solid")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1850-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-15, 30))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.8))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SwitzerlandInflation2.pdf", width = figWidth, height = figHeight, units="cm")

# Plot
Data.df <- data.frame(Date, ts_c(NGDP, GDP2))
colnames(Data.df)<- c("Date", "NGDP", "GDP2")
Data.df <- subset(Data.df, Date>="1850-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Switzerland") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black", "gray60"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash", "solid")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1850-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-15, 30))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.8))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SwitzerlandGDP2.pdf", width = figWidth, height = figHeight, units="cm")

# Plot
Data.df <- data.frame(Date, ts_c(PROX, CPI))
colnames(Data.df)<- c("Date", "Proxy", "CPI")
Data.df <- subset(Data.df, Date>="1850-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Switzerland") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1850-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-15, 30))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.8))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SwitzerlandInflation.pdf", width = figWidth, height = figHeight, units="cm")

# Plot
Data.df <- data.frame(Date, ts_c(GDP2))
colnames(Data.df)<- c("Date", "GDP")
Data.df <- subset(Data.df, Date>="1850-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Switzerland") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1850-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 20))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SwitzerlandGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI), order.by=index(CPI)), CPI, PROX, WPI, NGDP, GDP2)
colnames(DataExport) <- c("date", "cpi", "proxy", "wpi", "ngdp", "gdp2")
write.xlsx(DataExport, "./Data/DataForStataSwitzerland.xlsx", sheetName="Sheet1")

# ------------------------------------------------------------------------
# 4) Norway
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12

Data <- read.csv("./Data/RawDataNorway.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

# Calculate proxy
Pfish <- xts(Data$Pfish, order.by = Date)
Pmilk <- xts(Data$Pmilk, order.by = Date)
Pmeat <- xts(Data$Pmeat, order.by = Date)
Pcrop <- xts(Data$Pcrop, order.by = Date)
Pveget <- xts(Data$Pveget, order.by = Date)
Pcolonial <- xts(Data$Pcolonial, order.by = Date)
Pbever <- xts(Data$Pbever, order.by = Date)
Pwood <- xts(Data$Pwood, order.by = Date)
Poil <- xts(Data$Poil, order.by = Date)
Pclothing <- xts(Data$Pclothing, order.by = Date)

Wfish <- Data$Wfish[1]
Wmilk <- Data$Wmilk[1]
Wmeat <- Data$Wmeat[1]
Wcrop <- Data$Wcrop[1]
Wveget <- Data$Wveget[1]
Wcolonial <- Data$Wcolonial[1]
Wbever <- Data$Wbever[1]
Wwood <- Data$Wwood[1]
Woil <- Data$Woil[1]
Wclothing <- Data$Wclothing[1]

# Calculate linked series (with linear approximation for two years on database)
Pfish <- ts_xts(na.approx(ts_ts(Pfish)))
Pmilk <- ts_xts(na.approx(ts_ts(Pmilk)))
Pmeat <- ts_xts(na.approx(ts_ts(Pmeat)))
Pcrop <- ts_xts(na.approx(ts_ts(Pcrop)))
Pveget <- ts_xts(na.approx(ts_ts(Pveget)))
Pcolonial <- ts_xts(na.approx(ts_ts(Pcolonial)))
Pwood <- ts_xts(na.approx(ts_ts(Pwood)))
Poil <- ts_xts(na.approx(ts_ts(Poil)))
Pclothing <- ts_xts(na.approx(ts_ts(Pclothing)))

# Calculate proxy
P <- ts_c(Pfish, Pmilk, Pmeat, Pcrop, Pveget, Pcolonial, Pwood, Poil, Pclothing)
W <- rbind(Wfish, Wmilk, Wmeat, Wcrop, Wveget, Wcolonial, Wwood, Woil, Wclothing)
rownames(W) <- colnames(P)
colnames(W) <- c("BaseW")
PROX <- calcIndex(P, W, "1800-01-01") 

# Other variables
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
WPI <- ts_pcy(xts(Data$WPI, order.by = Date))
PPI <- ts_pcy(xts(Data$PPI, order.by = Date))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
CDEFL <- ts_pcy(xts(Data$CDEFL, order.by = Date))
PROX <- ts_pcy(PROX)
GDP <- ts_pcy(xts(Data$GDPPC, order.by = Date))
CONS <- ts_pcy(xts(Data$CONS, order.by = Date))
INV <- ts_pcy(xts(Data$INV, order.by = Date))

# Do a plot
Data.df <- data.frame(Date, ts_c(CPI, PROX))
colnames(Data.df)<- c("Date", "CPI", "Proxy")

Data.df <- subset(Data.df, Date>="1820-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Norway") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1820-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-25, 20))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.9,0.1))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/NorwayInflation.pdf", width = figWidth, height = figHeight, units="cm")

# Do a plot
Data.df <- data.frame(Date, ts_c(GDP, CONS))
colnames(Data.df)<- c("Date", "GDP", "Consumption")
Data.df <- subset(Data.df, Date>="1820-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Norway") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1820-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.9,0.7))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/NorwayGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI), order.by=index(CPI)), CPI, PROX, WPI, PPI, GDP, CONS, INV)
colnames(DataExport) <- c("date", "cpi", "proxy", "wpi", "ppi", "gdp", "cons", "inv")
write.xlsx(DataExport, "./Data/DataForStataNorway.xlsx", sheetName="Sheet1")

# ------------------------------------------------------------------------
# 4) Finland
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12

Data <- read.csv("./Data/RawDataFinland.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

# Other variables
COLI <- ts_pcy(xts(Data$COLI, order.by = Date))
WPI <- ts_pcy(xts(Data$WPI, order.by = Date))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
GDP <- ts_pcy(xts(Data$GDPPC, order.by = Date))

# Do a plot
Data.df <- data.frame(Date, ts_c(COLI, WPI))
colnames(Data.df)<- c("Date", "Cost-of-living index", "WPI")
Data.df <- subset(Data.df, Date>="1860-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Finland") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1860-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-25, 25))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(0.9,.8))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/FinlandInflation.pdf", width = figWidth, height = figHeight, units="cm")

# Do a plot
Data.df <- data.frame(Date, ts_c(GDP))
colnames(Data.df)<- c("Date", "GDP")
Data.df <- subset(Data.df, Date>="1860-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Finland") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1860-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 10))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.9,.02))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/FinlandGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(COLI), order.by=index(COLI)), COLI, WPI, GDEFL, GDP)
colnames(DataExport) <- c("date", "coli", "wpi", "gdefl", "gdp")
write.xlsx(DataExport, "./Data/DataForStataFinland.xlsx", sheetName="Sheet1")



# ------------------------------------------------------------------------
# 4) Chile
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12

Data <- read.csv("./Data/RawDataChile.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

# Other variables
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
GDP <- ts_pcy(xts(Data$GDPPC, order.by = Date))

# Do a plot
Data.df <- data.frame(Date, ts_c(CPI))
colnames(Data.df)<- c("Date", "CPI")
Data.df <- subset(Data.df, Date>="1810-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Chile") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1810-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-25, 25))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(0.9,.8))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/ChileInflation.pdf", width = figWidth, height = figHeight, units="cm")

# Do a plot
Data.df <- data.frame(Date, ts_c(GDP))
colnames(Data.df)<- c("Date", "GDP")
Data.df <- subset(Data.df, Date>="1810-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Chile") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1810-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-5, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.9,.02))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/ChileGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI), order.by=index(CPI)), CPI, GDP)
colnames(DataExport) <- c("date", "cpi", "gdp")
write.xlsx(DataExport, "./Data/DataForStataChile.xlsx", sheetName="Sheet1")


# ------------------------------------------------------------------------
# 5) Portugal
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12

Data <- read.csv("./Data/RawDataPortugal.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

#Other data
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
GDP <- ts_pcy(xts(Data$GDPPC, order.by = Date))

cor(ts_c(CPI, GDP), use = "pairwise.complete.obs")

# Do a plot
Data.df <- data.frame(Date, ts_c(GDEFL))
colnames(Data.df)<- c("Date", "GDP deflator")

Data.df <- subset(Data.df, Date>="1865-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Portugal") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1865-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-15, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.85))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/PortugalInflation.pdf", width = figWidth, height = figHeight, units="cm")

Data.df <- data.frame(Date, ts_c(GDP))
colnames(Data.df)<- c("Date", "GDP")
Data.df <- subset(Data.df, Date>="1865-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Portugal") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1865-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-25, 25))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.85))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/PortugalGDP.pdf", width = figWidth, height = figHeight, units="cm")


# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI), order.by=index(CPI)), CPI, GDEFL, GDP)
colnames(DataExport) <- c("date", "cpi", "gdefl", "gdp")
write.xlsx(DataExport, "./Data/DataForStataPortugal.xlsx", sheetName="Sheet1")

# ------------------------------------------------------------------------
# 6) Spain
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12

Data <- read.csv("./Data/RawDataSpain.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

#Other data
CPI1 <- ts_pcy(xts(Data$CPI1, order.by = Date))
CPI2 <- ts_pcy(xts(Data$CPI2, order.by = Date))
CPI3 <- ts_pcy(xts(Data$CPI3, order.by = Date))
CPI4 <- ts_pcy(xts(Data$CPI4, order.by = Date))
GDP <- ts_pcy(xts(Data$GDPPC, order.by = Date))
GNP <- ts_pcy(xts(Data$GNP, order.by = Date))

cor(ts_c(CPI1, GDP), use = "pairwise.complete.obs")

# Do a plot
Data.df <- data.frame(Date, ts_c(CPI1, CPI2))
Data.df <- subset(Data.df, Date>="1851-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Spain") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1850-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-25, 40))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SpainInflation.pdf", width = figWidth, height = figHeight, units="cm")

Data.df <- data.frame(Date, ts_c(GDP))
colnames(Data.df) <- c("Date", "GDP")
Data.df <- subset(Data.df, Date>="1851-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Spain") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1850-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.1))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SpainGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI1), order.by=index(CPI1)), CPI1, CPI2, CPI3, CPI4, GDP)
colnames(DataExport) <- c("date", "cpi1", "cpi2", "cpi3", "cpi4", "gdp")
write.xlsx(DataExport, "./Data/DataForStataSpain.xlsx", sheetName="Sheet1")

# ------------------------------------------------------------------------
# 7) UK
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12

Data <- read.csv("./Data/RawDataUK.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

# Construct proxy
Parable <- xts(Data$Parable, order.by = Date)
Ppasture <- xts(Data$Ppasture, order.by = Date)
Pwood <- xts(Data$Pwood, order.by = Date)

Warable <- Data$Warable[1]
Wpasture <- Data$Wpasture[1]
Wwood <- Data$Wwood[1]

# Calculate linked series (with linear approximation for two years on database)
Parable <- ts_xts(na.approx(ts_ts(Parable)))
Ppasture <- ts_xts(na.approx(ts_ts(Ppasture)))
Pwood <- ts_xts(na.approx(ts_ts(Pwood)))

# Compute proxy
P <- ts_c(Parable, Ppasture, Pwood)
W <- rbind(Warable, Wpasture, Wwood)
rownames(W) <- colnames(P)
colnames(W) <- c("BaseW")
PROX <- calcIndex(P, W, "1800-01-01") 

# Other variables
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
WPI <- ts_pcy(xts(Data$WPI, order.by = Date))
GDEFL <- ts_pcy(xts(Data$GDEFL, order.by = Date))
CDEFL <- ts_pcy(xts(Data$CDEFL, order.by = Date))
PROX <- ts_pcy(PROX)
GDP <- ts_pcy(xts(Data$GDP, order.by = Date))
GDPPC <- ts_pcy(xts(Data$GDPPC, order.by = Date))
CONS <- ts_pcy(xts(Data$CONS, order.by = Date))
INV <- ts_pcy(xts(Data$INV, order.by = Date))
GCONS <- ts_pcy(xts(Data$GCONS, order.by = Date))
EXP <- ts_pcy(xts(Data$EXP, order.by = Date))
IMP <- ts_pcy(xts(Data$IMP, order.by = Date))
URATE <- xts(Data$URATE, order.by = Date)
STX <- ts_pcy(xts(Data$STX, order.by = Date))
HOUSE <- ts_pcy(xts(Data$HOUSE, order.by = Date))
PROD <- ts_pcy(xts(Data$PROD, order.by = Date))

# Do a plot
Data.df <- data.frame(Date, ts_c(CPI, CDEFL))
colnames(Data.df)<- c("Date", "CPI", "PCE deflator")
Data.df <- subset(Data.df, Date>="1800-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("United Kingdom") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black", "magenta"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash", "solid")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1800-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-15, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.70))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/UKInflation.pdf", width = figWidth, height = figHeight, units="cm")

# Do a plot
Data.df <- data.frame(Date, ts_c(GDP, CONS, URATE))
colnames(Data.df)<- c("Date", "GDP","Consumption", "Unemployment rate")
Data.df <- subset(Data.df, Date>="1800-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("United Kingdom") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black", "magenta"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotted", "dotdash", "solid")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1800-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-5, 15))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.7))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/UKGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI), order.by=index(CPI)), CPI, WPI, PROX, GDEFL, CDEFL, GDP, GDPPC, PROD, CONS, INV, GCONS, EXP, IMP, URATE, STX, HOUSE)
colnames(DataExport) <- c("date", "cpi", "wpi", "proxy", "gdefl", "cdefl", "gdp", "gdppc", "prod", "cons", "inv", "gcons", "exp", "imp", "urate", "stx", "house")
write.xlsx(DataExport, "./Data/DataForStataUK.xlsx", sheetName="Sheet1")

# ------------------------------------------------------------------------
# 8) Sweden
# ------------------------------------------------------------------------
rm(list = ls())
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}
figWidth <- 13
figHeight <- 12

Data <- read.csv("./Data/RawDataSweden.csv", skip = 1)
Date <- as.Date(paste(Data$Year, "-01-01", sep = ""))

#Other data
CPI <- ts_pcy(xts(Data$CPI, order.by = Date))
GDP <- ts_pcy(xts(Data$GDP, order.by = Date))
GDPPC <- ts_pcy(xts(Data$GDPPC, order.by = Date))

cor(ts_c(CPI, GDP), use = "pairwise.complete.obs")

# Do a plot
Data.df <- data.frame(Date, ts_c(CPI))
colnames(Data.df)<- c("Date", "CPI")

Data.df <- subset(Data.df, Date>="1800-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Sweden") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1800-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-15, 25))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,0.05))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SwedenInflation.pdf", width = figWidth, height = figHeight, units="cm")

# Do a plot
Data.df <- data.frame(Date, ts_c(GDP))
colnames(Data.df)<- c("Date", "GDP")

Data.df <- subset(Data.df, Date>="1800-01-01", Date<="1899-01-01", )
melted.df = melt(Data.df, id.vars="Date")

g <- ggplot(data=melted.df, aes(x=Date, y=value, colour=variable, linetype = variable, shape = factor(variable))) + geom_line() 
g <- g + ggtitle("Sweden") + ylab("In %") + xlab("")
g <- g + scale_color_manual(values=c( "blue", "red", "green4", "black"))
g <- g + scale_linetype_manual(values = c( "solid", "longdash","twodash", "dotdash")) 
g <- g + theme(axis.line = element_line(colour = "black"))+ theme(panel.background = element_blank())+theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))
g <- g + scale_x_date(limits = c(as.Date("1800-01-01"), as.Date("1899-01-01")), expand = c(0, 0))  
g <- g + coord_cartesian(ylim=c(-10, 20))+ scale_y_continuous(expand = c(0, 0))
g <- g +  theme(legend.justification=c(1,0), legend.position=c(.95,.75))+theme(legend.title=element_blank())+ theme(legend.key = element_rect(fill = "white"))
g <- g  + theme(axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black"))
g <- g + theme(text = element_text(family = "Palatino"))
g
ggsave("./Results/Fig_B2/SwedenGDP.pdf", width = figWidth, height = figHeight, units="cm")

# Export historical data for use in Stata
DataExport <- ts_c(xts(year(CPI), order.by=index(CPI)), CPI, GDP, GDPPC)
colnames(DataExport) <- c("date", "cpi", "gdp", "gdppc")
write.xlsx(DataExport, "./Data/DataForStataSweden.xlsx", sheetName="Sheet1")
