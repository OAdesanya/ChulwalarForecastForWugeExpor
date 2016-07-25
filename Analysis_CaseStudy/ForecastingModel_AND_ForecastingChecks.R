#Forecasts with the models 
#Shorten the time series in order to test the forecasts
# Shortening the exports data in the Time Series in order to be able to compare the produced forecasts with the 
# As Is data.
WugeAsIs_2012 <- ts(WugeAsIsVector, start=c(2008,1), end=c(2012,12), frequency=12)

#Exponential Smoothing                                                    
# Exponential Smoothing uses past values to calculate a forecast. The strength 
# with which each value influences the forecast is weakened with help of a 
# smoothing parameter. Thus we are dealing with a weighted average, whose 
# values fade out the longer ago they were in the past.
# Formula: ses(). It must be decided if alpha (the smoothing parameter
# should be automatically calculated. If initial=simple, the alpha value can 
# be set to any chosen value, if initial=optimal (or nothing, as this is the 
# default), alpha will be set to the optimal value based on ets().
# h=12 gives the number of cycles for the forecast.
Model_ses <- ses(WugeAsIs, h=12)
summary(Model_ses)
plot(Model_ses)
#AIC     AICc      BIC       RMSE
#1981.370 1981.544 1985.923 108380.5

# The Akaike's Information Criterion(AIC/AICc) or the Bayesian Information 
# Criterion (BIC) should be at minimum.
plot(Model_ses, plot.conf=FALSE, ylab="Exports Chulwalar  )", xlab="Year", main="", fcol="white", type="o")
lines(fitted(Model_ses), col="green", type="o")
lines(Model_ses$mean, col="blue", type="o")
legend("topleft",lty=1, col=c(1,"green"), c("data", expression(alpha == 0.671)),pch=1)


#Holt's linear trend method   
Model_holt_1 <- holt(WugeAsIs,h=12)
summary(Model_holt_1)
plot(Model_holt_1)
#AIC     AICc      BIC      RMSE
#1984.278 1984.875 1993.385  107561.6

Model_holt_2<- holt(WugeAsIs, exponential=TRUE,h=12)
summary(Model_holt_2)
plot(Model_holt_2)
#AIC     AICc      BIC       RMSE
#1992.221 1992.818 2001.327 107620.5

 
Model_holt_3 <- holt(WugeAsIs, damped=TRUE,h=12)
summary(Model_holt_3)
plot(Model_holt_3)
#AIC     AICc      BIC      RMSE
#1986.510 1987.419 1997.893 107735

Model_holt_4 <- holt(WugeAsIs, exponential=TRUE, damped=TRUE,h=12)
summary(Model_holt_4)
plot(Model_holt_4)
#AIC     AICc      BIC      RMSE
#1995.499 1996.408 2006.883 108003.1

#Holt-Winter's seasonal method 
Model_hw_1 <- hw(WugeAsIs ,seasonal="additive",h=12)
summary(Model_hw_1)
plot(Model_hw_1)
#AIC     AICc      BIC      RMSE
#1910.881 1920.771 1947.307 54690.67


Model_hw_2 <- hw(WugeAsIs ,seasonal="multiplicative",h=12)
summary(Model_hw_2)
plot(Model_hw_2)
#AIC     AICc      BIC       RMSE
#1910.516 1920.407 1946.942  54484.25


# The Holt-Winter's seasonal method Model_hw_2 is the best model because it has the lowest AIC and RMSE number compared to the rest models.
# The additive model gives slightly better results than the multiplicative model.

plot(Model_hw_1, ylab="Exports Chulwalar  ", plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(Model_hw_1), col="red", lty=2)
lines(fitted(Model_hw_2), col="green", lty=2)
lines(Model_hw_1$mean, type="o", col="red")
lines(Model_hw_2$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))



