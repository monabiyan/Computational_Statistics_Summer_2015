library(WDI)
gdp <- WDI(country=c("US","CA"),indicator="NY.GDP.PCAP.CD",start=1960,end=2011)
colnames(gdp)[3] <- "PerCapGDP"
library(ggplot2)
ggplot(gdp,aes(year,PerCapGDP,color=country)) + geom_line()
gdpUS <- gdp$PerCapGDP[gdp$country=="United States"]
gdpCA <- gdp$PerCapGDP[gdp$country=="Canada"]
summary(lm(gdpUS~gdpCA))
us <- rev(gdpUS)
us <- ts(us,start=1960,end=2011)
us.d1 <- diff(us)

lag0 <- us[2:(length(us) - 0)] # the original series
lag1 <- us[1:(length(us) - 1)] # the series "lagged" or offset by one year
us.d1a <- lag0 - lag1
cor(us.d1,us.d1a)
plot(us.d1) #Still has upward trend

install.packages("forecast")
library(forecast)
ndiffs(us)
us.d2 <- diff(us,2)
plot(us.d2)

# Doing the same work for Canada data

ca <- rev(gdpCA)
ca <- ts(ca,start=1960,end=2011)
ndiffs(ca)
ca.d2 <- diff(ca,2)
summary(lm(us.d2 ~ ca.d2))
#So they do appear to be correlated, even after differencing twice. But which one is causing the other? One simple way to examine this is to look at which is leading and which is lagging - that is, let's regress one on the lag of the other, and then reverse the comparison; whichever model fits better suggests which variable is the lead (cause) and which variable is the effect. Is today's US GDP better seen as a function of yesterday's CA GDP, or is today's CA GDP better seen as a function of yesterday's US GDP?

#First we construct the sequences and lagged sequences:

us.d2.l0 <- us.d2[1:(length(us.d2)-1)] # original, but with one observation removed
ca.d2.l0 <- ca.d2[1:(length(ca.d2)-1)] # original, but with one observation removed
us.d2.l1 <- us.d2[2:(length(us.d2)-0)] # lag 1
ca.d2.l1 <- ca.d2[2:(length(ca.d2)-0)] # lag 1
summary(lm(us.d2.l0 ~ca.d2.l1 ))
summary(lm(ca.d2.l0 ~us.d2.l1 ))

pacf(us.d2)  #Yt=??1Y(t???1)+??2Y(t???2)+...
#We can replicate this more or less using regression, shown here for the first four lags:

us.d2.l0 <- us.d2[5:(length(us.d2) - 0)]
us.d2.l1 <- us.d2[4:(length(us.d2) - 1)]
us.d2.l2 <- us.d2[3:(length(us.d2) - 2)]
us.d2.l3 <- us.d2[2:(length(us.d2) - 3)]
us.d2.l4 <- us.d2[1:(length(us.d2) - 4)]
summary(lm(us.d2.l0 ~ us.d2.l1 + us.d2.l2 + us.d2.l3 + us.d2.l4))


#Y might  be a moving average (MA) of its past values
#Rather than having a very short memory, where the value Yt is only a function of Yt???1 and a few more lags, instead Yt is a function of lots of past lags, all contributing very small amounts. 

acf(us.d2)
#Of course, as with all models, in the real world we can't really assess one or the other model - AR or MA - independently. Instead, our process could be a mix of both - a little bit of auto-regression, where the immediate past values of Y influence the next values; and a little bit of moving average, where the long-term history of Y tends to have a gradual effect on the future. So to estimate the best model, we need to try a variety of mixtures of AR and MA, and see which model best fits the data (using, for instance, the Akaike Information Criterion, which is like R2 but generalizes to all kinds of models, not just linear regression).

#As usual, R will do our hard work for us. In the forecast package is the auto.arima function, which will find the best mix of AR and MA. ARIMA, of course stands for AR+MA, plus the I, which stands for "integrated" but usually is equivalent to how many differencings you have to do to the data first before doing the AR and MA analysis. Thus an ARIMA (1,2,0) model would be a model with 2 differencings, an AR(1) process (ie, Yt is partially correlated with Yt???1 but none of the other lags), and a MA(0) (ie, no moving average) part. A (2,1,1) ARIMA process would have an AR(2) component, 1 first-differencing, and a MA(1) component.
auto.arima(us)
################
auto.arima(us,xreg=ca)

auto.arima(ca,xreg=us)
##############
#Prediction
arus <- auto.arima(us)
predict(arus,n.ahead=5)

usforecast <- forecast(object = arus, h=10)
plot(usforecast)
