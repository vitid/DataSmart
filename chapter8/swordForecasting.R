library(forecast)
df = read.csv("/home/vitidn/mydata/repo_git/DataSmart/chapter8/swordData.csv")
ts_data = ts(df[,"Demand"])
plot(ts_data)

#simple exponential smoothing
ses = ets(ts_data,model = "ZNN")
plot(forecast(ses,3))

#T-test for lm's slope
lm_model = lm(Demand ~ t, data = df)
summary(lm_model)

#fit with double exponential smoothing(incorporating trend)
ses = ets(ts_data,model = "ZAN")
plot(forecast(ses,3))

#investigate auto-correlation
acf(ses$residuals)

#we now know that the frequency = 12
ts_data = ts(df[,"Demand"],frequency = 12)

#fit with triple exponential smoothing(incorporating trend)
ses = ets(ts_data,model = "ZAM")
plot(forecast(ses,12))

#investigate auto-correlation
acf(ses$residuals,lag.max = 36)
