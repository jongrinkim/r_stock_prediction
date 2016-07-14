library(forecast)
library(fpp)

setwd("/Users/jongrinkim/Desktop/Ray/Stock/")
Data = read.table("BigDataDaumann/r-stockPrediction/ray_stocks_data/EVRI.csv", 
                   sep=",", header=TRUE)
#cut off the data at 2008 January
Data = Data[1:103,]
#flip it upside down to set it up for Time Series model
Stock = Data[order(nrow(Data):1),]
tsStock = ts(Stock$Close, start=c(2008, 1), frequency=12)

plot(tsStock)

########################Generalization########################
# 1. polynomial fit
t1 = seq(2008, 2016.6, length=length(tsStock))
t12 = t1^7
polystock = lm(tsStock ~ t1 + t12)
#prediction of polynomial
tsStocktrend1=ts(polystock$fit, start=c(2008,1), frequency=12)
plot(tsStock,lw=2, col="blue", xlim=c(2008,2018))
lines(tsStocktrend1, lw=2, col="red")
abline(v=2016.5,lty =3)

# 2. STL fit (Decompose the price level into seasonal) 
stlStock = stl(tsStock, s.window="periodic")
plot(stlStock,col="blue",lw=2)
#prediction of STL
tsStocktrend2 = stlStock$time.series[,2]
plot(forecast(stlStock))

#data summary
plot(tsStock,lw=1)
lines(tsStocktrend1, col='purple',lw=1)
lines(tsStocktrend2, col='red',lw=2)
abline(v=2016.5,lty =3)
legend("topleft",legend=c("Actual Function", "STL Trend", "Polynomial Trend"),
       col=c("black","red","purple"), lw=2, cex = 1)

##########################Start Predicting#############
#USING polynomial
HWStock1_ng = HoltWinters(tsStocktrend1, gamma=FALSE)
HWStock1 = HoltWinters(tsStocktrend1)
NETfit1 <- nnetar(tsStocktrend1)
autofit1 = auto.arima(tsStocktrend1)
fit1 <- arima(tsStocktrend1, order=c(1,0,0), list(order=c(2,1,0), period=12))
fitl1 <- tslm(tsStocktrend1 ~ trend + season, lambda = 0)
stlStock1 = stl(tsStocktrend1, s.window = "periodic")

#plot
plot(forecast(autofit1,h=24),xlim=c(2008,2018),ylim=c(0,10),
     lw=2, col="red",xlab="Time", ylab="Stock Price", main = "Predictions of the Polynomial Trend")
lines(tsStock, lw=2)
#lines(tsStocktrend2, col="red", lw=2)
lines(forecast(stlStock1, h=24) $mean, col="yellow",lw=2)
lines(forecast(fitl1, h=24)$mean,col="orange") #tslm
lines(forecast(fit1, h=24)$mean, lw=2, col="purple") #arima
lines(forecast(NETfit1,h=24)$mean, lw=2, lty="longdash", col="brown")
lines(predict(HWStock1_ng, n.ahead=24),lw=2,col="black") #exponential smoothing
lines(predict(HWStock1, n.ahead=24),lw=2,col="green") #non-exponential smoothing
#Fitted Holt Winters model
lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[,1], lw=2, col="green")
#Upper boundary of Holt Winters
#lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[,2], col="orange")
#Lower boundary of Holt Winters
#lines(predict(HWStock1, n.ahead = 24, prediction.interval = T, level = 0.95)[,3], col="orange")
legend("bottomleft",legend=c("Prediction - Arima (auto)",
                             "STL trend",
                             "Holt Winter",
                             "Holt Winter (exponential smoothing)",
                             "Prediction -Arima (fixed)",
                             "Neural Nets",
                             "Linear model w/ time series components"),
       col=c("red", "yellow","green", "black","purple","brown","orange"), lw=2, cex=1)
abline(v=2016.5, lty=2)

#USING STL
HWStock2_ng = HoltWinters(tsStocktrend2, gamma=FALSE) #gamma = false means exponential smoothing is used
HWStock2 = HoltWinters(tsStocktrend2)
NETfit2 <- nnetar(tsStocktrend2)
autofit2 = auto.arima(tsStocktrend2)
fit2 <- arima(tsStocktrend2, order=c(1,0,0), list(order=c(2,1,0), period=12))
fitl2 <- tslm(tsStocktrend2 ~ trend + season, lambda = 0)
stlStock2 = stl(tsStocktrend2, s.window = "periodic")

#plot
plot(forecast(autofit2,h=24),xlim=c(2008,2018),ylim=c(-5,30),
     lw=2, col="red",xlab="Time", ylab="Stock Price", main = "Predictions of the STL")
lines(tsStock, lw=2)
#lines(tsStocktrend2, col="red", lw=2)
lines(forecast(stlStock2, h=24) $mean, col="yellow",lw=2)
lines(forecast(fitl2, h=24)$mean,col="orange") #tslm
lines(forecast(fit2, h=24)$mean, lw=2, col="purple") #arima
lines(forecast(NETfit2,h=24)$mean, lw=2, lty="longdash", col="brown")
lines(predict(HWStock2_ng, n.ahead=24),lw=2,col="black") #exponential smoothing
lines(predict(HWStock2, n.ahead=24),lw=2,col="green") #non-exponential smoothing
#Fitted Holt Winters model
lines(predict(HWStock2, n.ahead = 24, prediction.interval = T, level = 0.95)[,1], lw=2, col="green")
#Upper boundary of Holt Winters
#lines(predict(HWStock2, n.ahead = 24, prediction.interval = T, level = 0.95)[,2], col="orange")
#Lower boundary of Holt Winters
#lines(predict(HWStock2, n.ahead = 24, prediction.interval = T, level = 0.95)[,3], col="orange")
legend("bottomleft",legend=c("Prediction - Arima (auto)",
                             "STL trend",
                             "Holt Winter", 
                             "Holt Winter (exponential smoothing)", 
                             "Prediction -Arima (fixed)", 
                             "Neural Nets",
                             "Linear model w/ time series components"), 
       col=c("red", "yellow","green", "black","purple","brown","orange"), lw=2)
abline(v=2016.5, lty=2)
