library(fpp)
require(fpp)
require(forecast)

# Basic data analysis

#Correlation between As Is and Plan Data
cor(WugeAsIs, WugePlan)

WugeAsIs_lm <- lm(WugeAsIs ~ WugePlan , data = WugeAsIs)
summary(WugeAsIs_lm)
WugeAsIs_tslm <- tslm(WugeAsIs ~ WugePlan )
summary(WugeAsIs_tslm)


# The time series can be analysed using the stl function in order to seperate the trend, seasonality and remainder (remaining coincidential) components from
# one another.
WugeAsIs_stl <- stl(WugeAsIs, s.window=5)

#The individual time series can be shown graphically and tabularly.
par(mfrow=c(3,2))
plot(WugeAsIs_stl, col="black", main="WugeAsIs_stl")
WugeAsIs_stl
