getwd()
p='C:/Users/Admin/Downloads'
list.dirs()
list.files()
library(readr)
AAPLOCT01toNov012018 <- read.csv("AAPLOCT01toNov012018.csv")
View(AAPLOCT01toNov012018)
df<-AAPLOCT01toNov012018
head(df)
str(df)
new_date <- as.Date(df$Date)
new_date
str(df)
format(new_date,format="%B %d %Y")
data = ts(df$Close,frequency =6)

plot(data,main="Monthly Closing Prices")
log(data)
decompose(data)
decompose(data, type='multi')
par(mfrow=c(1,2))
plot(decompose(data, type='multi'))
library(forecast)
seasonplot(data)

lag(data,10)
lag.plot(data)
pac<-pacf(data)
model <- lm(data~c(1:length(data)))

summary(model)
plot(resid(model),type='l')
tbl <- stl(data,'periodic')

stab<-seasadj(tbl)

seasonplot(stab,6)
library(tseries)

adf.test(data)
model2<-auto.arima(data)
accuracy(model2)
plot(forecast(model2,h=6))

adf.test(diff(data))
plot(diff(data))
diff(data,differences = 3)
model3<-auto.arima(diff(data))

accuracy(model3)
acf(diff(data))

pacf(diff(data))
model4 <- Arima(diff(data),order=c(4,0,5))
model4
plot(log(data))
model4<-ets(data)
summary(model4)
accuracy(model4)
plot(log(data))
