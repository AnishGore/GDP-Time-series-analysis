#Install Package
install.packages("WDI")
install.packages("ggplot2")
install.packages("plotly")
install.packages("forecast")
library("WDI")
library("ggplot2")
library("plotly")
library("forecast")

# Extracting the data of GDP fro USA, India and Canada

WDIsearch('gdp')
USgdp <- WDI(country = c("US"),indicator = "NY.GDP.PCAP.CD", start = 1960,end = 2017) 
INgdp <- WDI(country = c("IN"),indicator = "NY.GDP.PCAP.CD", start = 1960,end = 2017)
CAgdp <- WDI(country = c("CA"),indicator = "NY.GDP.PCAP.CD", start = 1960,end = 2017)
summary(USgdp)
summary(INgdp)
summary(CAgdp)
USgdp[is.na(USgdp$NY.GDP.PCAP.CD), "NY.GDP.PCAP.CD"] <- median(USgdp$NY.GDP.PCAP.CD, na.rm = T)
INgdp[is.na(INgdp$NY.GDP.PCAP.CD), "NY.GDP.PCAP.CD"] <- median(INgdp$NY.GDP.PCAP.CD, na.rm = T)
CAgdp[is.na(CAgdp$NY.GDP.PCAP.CD), "NY.GDP.PCAP.CD"] <- median(CAgdp$NY.GDP.PCAP.CD, na.rm = T)

# renaming the column 'NY.GDP.PCAP.CD' to 'GDPperCAP'

names(USgdp) <- c("iso2c","country","GDPperCAP","year")
names(INgdp) <- c("iso2c","country","GDPperCAP","year")
names(CAgdp) <- c("iso2c","country","GDPperCAP","year")
head(CAgdp)

# re-structuring the dataframes as per the year in ascending order

USgdp <- USgdp[order(USgdp$year),]
INgdp <- INgdp[order(INgdp$year),]
CAgdp <- CAgdp[order(CAgdp$year),]
head(INgdp)

# combining the individual time series

gdp <- rbind(USgdp,INgdp,CAgdp)
head(gdp)
summary(gdp)

# Interactive visualization of time series.

visual1 <- ggplot(gdp, aes(year, GDPperCAP, color=country)) + geom_line() + 
           xlab('Year') + ylab('GDP per capita')
visshow <- ggplotly(visual1)
visshow

# Time series and Plots
USgdpts <- ts(USgdp$GDPperCAP)
plot(USgdpts)

INgdpts <- ts(INgdp$GDPperCAP)
plot(INgdpts)

CAgdpts <- ts(CAgdp$GDPperCAP)
plot(CAgdpts)

# Correlogram
UScorr <- acf(USgdpts,lag.max = 20)
INcorr <- acf(INgdpts,lag.max = 20)
CAcorr <- acf(CAgdpts,lag.max = 20)

# partial correlogram
USpacf <- pacf(USgdpts,lag.max = 20)
INpacf <- pacf(INgdpts,lag.max = 20)
CApacf <- pacf(CAgdpts,lag.max = 20)

# ARIMA
USgdpforecast <- arima(USgdp$GDPperCAP, order = c(1,0,0))
USgdpforecast

INgdpforecast <- arima(INgdp$GDPperCAP, order = c(1,0,0))
INgdpforecast

CAgdpforecast <- arima(CAgdp$GDPperCAP, order = c(1,0,0))
CAgdpforecast

# Forecast 

USgdpforecast1 <- forecast(USgdpforecast,h = 10)
USgdpforecast1
plot(USgdpforecast1)


INgdpforecast1 <- forecast(INgdpforecast,h = 10)
INgdpforecast1
plot(INgdpforecast1)

CAgdpforecast1 <- forecast(CAgdpforecast,h = 10)
CAgdpforecast1
plot(CAgdpforecast1)
