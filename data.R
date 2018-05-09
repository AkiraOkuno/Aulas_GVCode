#How to extract files from FRED, YAHOO FINANCE, GOOGLE
library(quantmod)
library(plotly)
library(PerformanceAnalytics)
library(reshape2)
library(ggplot2)

#Baixando os dados do Yahoo Finance
#Exemplos
# ex1 <- getSymbols("YHOO",src="google") # from google finance yahoo stocks info
# ex2 <- getSymbols("DEXJPUS",src="FRED") # from Federal Reserve taxa de cambio jpn us


Cisco <- getSymbols("CSCO", src = "yahoo", from = "1995-01-01", auto.assign = F)
Amazon <- getSymbols("AMZN", src ="yahoo", from = "1995-01-01", auto.assign = F)
Nasdaq <- getSymbols("^IXIC", src ="yahoo", from = "1995-01-01", auto.assign = F)
SandP500 <- getSymbols("^GSPC", src = "yahoo", from = "1995-01-01", auto.assign = F)
Google <- getSymbols("GOOG", src = "yahoo", from = "1995-01-01", auto.assign = F)
TISCALI <- getSymbols("TIS.MI", src = "yahoo", from = "1995-01-01", auto.assign = F)
MicroStrategy <- getSymbols("MSTR", src = "yahoo", from = "1995-01-01", auto.assign = F)
Apple <- getSymbols("AAPL", src = "yahoo", from = "1995-01-01", auto.assign = F)
IBM <- getSymbols("IBM", src = "yahoo", from = "1995-01-01", auto.assign = F)


#Observando os dados
chartSeries(Cisco)
chartSeries(Amazon)
chartSeries(Nasdaq)
chartSeries(Google)
chartSeries(TISCALI)
chartSeries(MicroStrategy)
chartSeries(SandP500)
chartSeries(Apple)
chartSeries(IBM)

#como os dados vem?
#xts format
Cisco

#Limpando os dados e agrupando em um DF
cls.Cisco <- Cisco[,4]
cls.Amazon <- Amazon[,4]
cls.Nasdaq <- Nasdaq[,4]
cls.MicroStrategy <- MicroStrategy[,4]
cls.SandP <- SandP500[,4]
cls.Apple <- Apple[,4]
cls.IBM <- IBM[,4]

data <- cbind.xts(cls.Amazon, cls.Cisco, cls.MicroStrategy, cls.Nasdaq, cls.SandP, cls.Apple, cls.IBM)
df <- data.frame(date = index(data), coredata(data))
#Remover tudo que tem NA
df <- na.omit(df)
colnames(df) <- c("date", "Amazon", "Cisco", "MiscroStrategy", "Nasdaq", "S&P500", "Apple", "IBM")


#calculo dos retornos
returnsdf <- data.frame(lapply(df[2:8], function(x) diff(log(x))))
returnsdf$date <- df$date[-1]
returnsdf <- na.omit(returnsdf)

colnames(returnsdf) <- c("Amazon", "Cisco", "MiscroStrategy", "Nasdaq", "S&P500", "Apple", "IBM", "date")

#Correlation matrix - stocks
cormat <- round(cor(returnsdf[1:7]))
melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x= Var1, y = Var2, fill = value)) + geom_tile()

chart.Correlation(returnsdf[1:7]) 
