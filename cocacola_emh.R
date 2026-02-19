library(quantmod)
library(tseries)
library(lmtest)
library(urca)
library(forecast)
library(ggplot2)
library(extrafont)
library(dplyr)

###AUTHOR: BRUINSMA PATRIK R11V3S
setwd("C:\\Econometrics2")
if(!dir.exists("figures")) dir.create("figures")
loadfonts(device = "win")
ko <- getSymbols("KO", from="2014-01-01", to="2019-01-01",
           auto.assign = FALSE)
sp <- getSymbols("^GSPC", from="2014-01-01", to="2019-01-01",
                 auto.assign = FALSE)
pep <- getSymbols("PEP", from="2014-01-01", to="2019-01-01",
                          auto.assign = FALSE)
sugar <- getSymbols("SB=F", from="2014-01-01", to="2019-01-01",
                    auto.assign = FALSE)
sugar <- sugar$`SB=F.Close`
sugar <- na.approx(sugar)



ko <- ko$KO.Close
sp <- sp$GSPC.Close
pep <- pep$PEP.Close
df <- data.frame(date=index(ko), coredata(ko))
df$sugar <- sugar
df$pep <- pep
df$sp <- sp
df$ld_ko <- c(NA, diff(log(df$KO.Close)))
df$ld_pep <- c(diff(log(df$pep)))
df$ld_sp <- c(diff(log(df$sp)))
df$ld_sugar <- diff(log(df$sugar))
df <- df[-1,]

jo_test <- ca.jo(df[c(2,4)],type = "eigen")
summary(jo_test)
 
ggplot(df, aes(x=date))+
  geom_line(aes(y=log(pep), colour = "PEPSI"), size=1)+
  geom_line(aes(y=log(KO.Close), colour = "COLA"), size=1)+
  geom_line(aes(y=log(sugar), colour = "Sugar"), size=1)+
  geom_line(aes(y=log(sp), colour = "S&P500"), size=1)+
  theme_minimal()+
  labs(title = "Log Return of Stocks",
       y="Log return",
       x="Date",
       caption = "Source: Yahoo Finance",
       color="Stock")+
  theme(text = element_text(family = "Time New Roman"))
ggsave("figures/log_returns.png", width = 10, height = 6)

png("figures/ko_ts.png", width = 800, height = 600)
plot.ts(df$KO.Close)
dev.off()

adf.test(df$ld_ko)
adf.test(df$ld_pep)
adf.test(df$ld_sp)
summary(ur.df(df$ld_ko, type ="trend", lags = 20, selectlags = "BIC"))
summary(ur.df(df$ld_pep, type ="trend", lags = 20, selectlags = "BIC"))
summary(ur.df(df$ld_sp, type ="trend", lags = 20, selectlags = "BIC"))
summary(ur.df(df$ld_sugar, type ="trend", lags = 20, selectlags = "BIC"))


png("figures/acf_pacf.png", width = 800, height = 800)
par(family = "Times New Roman")
par(mfrow=c(2,1))
acf(df$ld_ko, main="Autocorrelation of Log Difference of Coca-Cola")
pacf(df$ld_ko, main="Partial Autocorrelation of Log Difference of Coca-Cola")
dev.off()


fit1 <- arima(df$ld_ko, order = c(2,0,0))
bgtest(fit1$residuals~1, order = 15)
#MA2
fit2 <- arima(df$ld_ko, order = c(0,0,2))
bgtest(fit2$residuals~1, order = 15)
#arma 2,2
fit3 <- arima(df$ld_ko, order = c(2,0,2))
ft1 <- arima(df$ld_ko, order = c(2,0,1))
ft2 <- arima(df$ld_ko, order =c(1,0,1))
BIC(ft2, ft1, fit1, fit2, fit3)
AIC(fit1, fit2, fit3)

fit4 <- Arima(df$ld_ko, order = c(2,0,0), xreg = df$ld_pep)
fit5 <- Arima(df$ld_ko, order = c(2,0,0), xreg = df$ld_sp)
fit6 <- Arima(df$ld_ko, order = c(2,0,0), xreg = df$ld_pep+df$ld_sp)
fit8 <- Arima(df$ld_ko, order = c(2,0,0), xreg = df$ld_sugar)


BIC(fit4, fit5, fit6,  fit1,fit8 )
AIC(fit4, fit5, fit6, fit8, fit1)

ggplot(df, aes(x=date))+
  geom_line(aes(y=ld_ko, colour = "Original"), size=1)+
  geom_line(aes(y=fitted(fit4), colour = "Estimated"), size=1) 
ggsave("figures/original_vs_estimated.png", width = 10, height = 6)

#final model
model <- Arima(log(df$KO.Close), order = c(2,1,0), xreg = df$ld_pep)
bgtest(model$residuals~1, order = 15)
shapiro.test(resid(model))

model2 <- Arima(log(df$KO.Close), order = c(2,1,0), xreg = df$ld_pep)
model2$fitted <- exp(model2$fitted)
ar <- Arima(log(df$KO.Close), order = c(2,1,0))
ggplot(df, aes(x=date))+
  geom_line(aes(y=log(df$KO.Close), color = "Log Return of Coca-Cola"), size = 1)+
  geom_line(aes(y=model$fitted, color = "Estimated Log Return"), size = 1)+
  theme_minimal()+
  labs(title = "Model Performance",
       y = "Log Return of Coca-Cola",
       color= "",
       caption = "Source: Yahoo Finance")+
  theme(text = element_text(family = "Time New Roman"))
ggsave("figures/model_performance_log.png", width = 10, height = 6)

ggplot(df, aes(x=date))+
  geom_line(aes(y=df$KO.Close, color = "Stock Price of Coca-Cola"), size = 1)+
  geom_line(aes(y=model2$fitted, color = "Estimated Stock Price"), size = 1)+
  theme_minimal()+
  labs(title = "Model Performance on Stock Price",
       y = "Log Return of Coca-Cola",
       color= "",
       caption = "Source: Yahoo Finance")+
  theme(text = element_text(family = "Time New Roman"))
ggsave("figures/model_performance_stock.png", width = 10, height = 6)

#forecasting
predar <- predict(ar, n.ahead = 5)
predar$pred <- exp(predar$pred)
predar$pred

pred <- predict(model, n.ahead = 5, newxreg = df$ld_pep)
pred$pred
dfp <- data.frame(coredata(pred$pred))
dfp <- exp(dfp)
dfp$date <- as.Date(row.names(dfp), format = "%Y-%m-%d")
dfp <- dfp[dfp$date>="2018-12-24",]
forecast <- exp(tail(pred$pred))


dffilter <- df[df$date>="2018-12-01", c(1,2)]


dffilter <- merge(dffilter, dfp, by="date", all = TRUE)

ggplot(dffilter, aes(x=date))+
  geom_line(aes(y=dffilter$KO.Close, colour = "Stock Price"), size=1)+
  geom_line(aes(y =coredata.pred.pred., colour = "Forecast"), size =1)+
  theme_minimal()+
  labs(title = "Forecast of Stock Price",
       y= "Stock Price",
       x= "Date",
       caption="Source: Yahoo Finance",
       color="")+
  theme(text = element_text(family = "Times New Roman"))
ggsave("figures/forecast.png", width = 10, height = 6)

summary <- capture.output(summary(model))

writeLines(summary, "model_summary.txt")

