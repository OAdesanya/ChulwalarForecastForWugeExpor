## Set directory variables
## main directory
dirr <- "/FOrecastChulwalarExportofWuge"

## data directory
datadirr <- paste(dirr, "DATA", sep = "/")

## analysis directory
analysisdirr <- paste(dirr, "ANALYSIS", sep = "/")


# The Export data for Chulwalar   are in two .csv files.
# One file for the as is data: ImportedAsIsDataChulwalar.csv
# and another one for the plan data: ImportedPlanDataChulwalar.csv
ImportedAsIsData <- read.csv("ImportedAsIsDataChulwalar.csv", header = F, sep=";", fill = T) #chose ImportedAsIsDataChulwalar.csv
ImportedPlanData <- read.csv("ImportedPlanDataChulwalar.csv", header = F, sep=";", fill = T) #chose ImportedPlanDataChulwalar.csv


# The indicators data is also in a file: ImportedIndicatorsChulwalar.csv
ImportedIndicators <- read.csv("ImportedIndicatorsChulwalar.csv", header = F, sep=";", fill = T) # chose ImportedIndicatorsChulwalar.csv
ImportedAsIsData
ImportedPlanData
ImportedIndicators


# Transformation the data into vectors and time series
# In order to be able to work with the partial data sets later, these need to
# be split into individual vectors and converted into times series.
WugeAsIsVector <- c(ImportedAsIsData [30:41,2],ImportedAsIsData [30:41,3],ImportedAsIsData [30:41,4],ImportedAsIsData [30:41,5],ImportedAsIsData [30:41,6],ImportedAsIsData [30:41,7])
WugePlanVector <- c(ImportedPlanData[30:41,2],ImportedPlanData[30:41,3],ImportedPlanData[30:41,4],ImportedPlanData[30:41,5],ImportedPlanData[30:41,6],ImportedPlanData[30:41,7])

# The data is saved as a vector and needs to be converted into a time series
WugeAsIs <- ts(WugeAsIsVector, start=c(2008,1), end=c(2013,12), frequency=12)
WugePlan <- ts(WugePlanVector, start=c(2008,1), end=c(2013,12), frequency=12)

# to get total asis for 2014 for forecasr comaprison
TotalAsIsVector_2014 <- c(ImportedAsIsData[2:13,8])
TotalAsIs_2014 <- ts(TotalAsIsVector_2014, start=c(2014,1), end=c(2014,12), frequency=12)

# Call up the time series to check everything has worked.
WugeAsIs
WugePlan

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

# Basic data analysis
par(mfrow=c(3,2))
plot(WugeAsIs_stl$time.series[,"trend"], col="blue")

#  2.3.2 Modification of the seasonal componant to a monthly base  

# The modification of the seasonlity component can also be changed into a
# monthly view. It only makes sense to do this if the seasonality componant as
# the trend looks almost identical and the remainder is then randomly spread. 

monthplot(WugeAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")

#Correlation of different external indicators
# The indicators will be converted into individual  vectors and subsequentlyconverted into time series. The correlation of the indicators will then be
# tested against the As Is exports for Chulwalar. 

# Monthly Change in Export Price Index (CEPI)
CEPIVector <- c(ImportedIndicators[2:13,2],ImportedIndicators[2:13,3],ImportedIndicators[2:13,4],ImportedIndicators[2:13,5],ImportedIndicators[2:13,6],ImportedIndicators[2:13,7])
CEPI <- ts(CEPIVector , start=c(2008,1), end=c(2013,12), frequency=12)
plot(CEPI, main="CEPI")
cor(WugeAsIs, CEPI)

# Monthly Satisfaction Index (SI) government based data
# The Satisfaction Index does not show any particular correlation with any of 
# the exports data.
SIGovVector <- c(ImportedIndicators[16:27,2],ImportedIndicators[16:27,3],ImportedIndicators[16:27,4],ImportedIndicators[16:27,5],ImportedIndicators[16:27,6],ImportedIndicators[16:27,7])
SIGov <- ts(SIGovVector , start=c(2008,1), end=c(2013,12), frequency=12)
plot(SIGov, main="SIGov")
cor(WugeAsIs, SIGov)

# Average monthly temperatures in Chulwalar
#The temperatures have a negative correlation for Wugeasis.
TemperatureVector <- c(ImportedIndicators[30:41,2],ImportedIndicators[30:41,3],ImportedIndicators[30:41,4],ImportedIndicators[30:41,5],ImportedIndicators[30:41,6],ImportedIndicators[30:41,7])
Temperature <- ts(TemperatureVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(Temperature, main="Temperature")
cor(WugeAsIs, Temperature)

# Monthly births in Chulwalar 
BirthsVector <- c(ImportedIndicators[44:55,2],ImportedIndicators[44:55,3],ImportedIndicators[44:55,4],ImportedIndicators[44:55,5],ImportedIndicators[44:55,6],ImportedIndicators[44:55,7])
Births <- ts(BirthsVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(Births, main="Births")
cor(WugeAsIs, Births)

# Monthly Satisfaction Index (SI) external index 
SIExternVector <- c(ImportedIndicators[58:69,2],ImportedIndicators[58:69,3],ImportedIndicators[58:69,4],ImportedIndicators[58:69,5],ImportedIndicators[58:69,6],ImportedIndicators[58:69,7])
SIExtern <- ts(SIExternVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(SIExtern, main="SIExtern")
cor(WugeAsIs, SIExtern)

# Yearly exports from Urbano
UrbanoExportsVector <- c(ImportedIndicators[72:83,2],ImportedIndicators[72:83,3],ImportedIndicators[72:83,4],ImportedIndicators[72:83,5],ImportedIndicators[72:83,6],ImportedIndicators[72:83,7])
UrbanoExports <- ts(UrbanoExportsVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(UrbanoExports, main="UrbanoExports")
cor(WugeAsIs, UrbanoExports)


# Yearly number of Globalisation Party members in Chulwalar
GlobalisationPartyMembersVector <- c(ImportedIndicators[86:97,2],ImportedIndicators[86:97,3],ImportedIndicators[86:97,4],ImportedIndicators[86:97,5],ImportedIndicators[86:97,6],ImportedIndicators[86:97,7])
GlobalisationPartyMembers <- ts(GlobalisationPartyMembersVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(GlobalisationPartyMembers, main="GlobalisationPartyMembers")
cor(WugeAsIs, GlobalisationPartyMembers)

# Monthly Average Export Price Index for Chulwalar
AEPIVector <- c(ImportedIndicators[100:111,2],ImportedIndicators[100:111,3],ImportedIndicators[100:111,4],ImportedIndicators[100:111,5],ImportedIndicators[100:111,6],ImportedIndicators[100:111,7])
AEPI <- ts(AEPIVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(AEPI, main="AEPI")
cor(WugeAsIs, AEPI)


# Monthly Producer Price Index (PPI) for Etel in Chulwalar
PPIEtelVector <- c(ImportedIndicators[114:125,2],ImportedIndicators[114:125,3],ImportedIndicators[114:125,4],ImportedIndicators[114:125,5],ImportedIndicators[114:125,6],ImportedIndicators[114:125,7])
PPIEtel <- ts(PPIEtelVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(PPIEtel, main="PPIEtel")
cor(WugeAsIs, PPIEtel)

# National Holidays
NationalHolidaysVector <- c(ImportedIndicators[170:181,2],ImportedIndicators[170:181,3],ImportedIndicators[170:181,4],ImportedIndicators[170:181,5],ImportedIndicators[170:181,6],ImportedIndicators[170:181,7])
NationalHolidays <- ts(NationalHolidaysVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(NationalHolidays, main="NationalHolidays")
cor(WugeAsIs, NationalHolidays)

# Chulwalar Index (Total value of all companies in Chulwalar)
ChulwalarIndexVector <- c(ImportedIndicators[128:139,2],ImportedIndicators[128:139,3],ImportedIndicators[128:139,4],ImportedIndicators[128:139,5],ImportedIndicators[128:139,6],ImportedIndicators[128:139,7])
ChulwalarIndex <- ts(ChulwalarIndexVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(ChulwalarIndex, main="ChulwalarIndex")
cor(WugeAsIs, ChulwalarIndex)

# Monthly Inflation rate in Chulwalar 
InflationVector <- c(ImportedIndicators[142:153,2],ImportedIndicators[142:153,3],ImportedIndicators[142:153,4],ImportedIndicators[142:153,5],ImportedIndicators[142:153,6],ImportedIndicators[142:153,7])
Inflation <- ts(InflationVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(Inflation, main="Inflation")
cor(WugeAsIs, Inflation)


# Proposed spending for Independence day presents
IndependenceDayPresentsVector <- c(ImportedIndicators[156:167,2],ImportedIndicators[156:167,3],ImportedIndicators[156:167,4],ImportedIndicators[156:167,5],ImportedIndicators[156:167,6],ImportedIndicators[156:167,7])
IndependenceDayPresents <- ts(IndependenceDayPresentsVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(IndependenceDayPresents, main="IndependenceDayPresents")
cor(WugeAsIs, IndependenceDayPresents)

# Influence of National Holidays :
# This indicator is an experiment where the influence of National Holidays is extended into the months leading up to the holiday. 
# However later tests show that this indicator is no better for forecasting than the orignial National Holidays indicator.    

InfluenceNationalHolidaysVector <- c(ImportedIndicators[184:195,2],ImportedIndicators[184:195,3],ImportedIndicators[184:195,4],ImportedIndicators[184:195,5],ImportedIndicators[184:195,6],ImportedIndicators[184:195,7])
InfluenceNationalHolidays <- ts(InfluenceNationalHolidaysVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(InfluenceNationalHolidays, main="InfluenceNationalHolidays")
cor(WugeAsIs, InfluenceNationalHolidays)

# Check that the data import has worked

CEPIVector 
SIGovVector  
TemperatureVector 
BirthsVector 
SIExternVector 
UrbanoExportsVector 
GlobalisationPartyMembersVector 
AEPIVector 
PPIEtelVector 
NationalHolidaysVector 
ChulwalarIndexVector 
InflationVector 
IndependenceDayPresentsVector


#Correlation of WugeAsis against the various indicators.

# There is a positive correlation 
cor(WugeAsIs, CEPI)

# There is a positive correlation
cor(WugeAsIs, SIGov)

# There is a negative correlation
cor(WugeAsIs, Temperature)

# There is a substantial negative correlation
cor(WugeAsIs, Births)

# There is a positive correlation
cor(WugeAsIs, SIExtern)

# There is a positive correlation
cor(WugeAsIs, UrbanoExports)

# There is a positive correlation
cor(WugeAsIs, GlobalisationPartyMembers)

# There is a positive correlation
cor(WugeAsIs, AEPI)

#There is a positive correlation
cor(WugeAsIs, PPIEtel)

#There is a positive correlation
cor(WugeAsIs, NationalHolidays)

# There is a substantial positive correlation
cor(WugeAsIs, ChulwalarIndex)

#There is a positive correlation
cor(WugeAsIs, Inflation)

#There is a positive correlation
cor(WugeAsIs, IndependenceDayPresents)


#There is a relative small positive correlation
cor(WugeAsIs, InfluenceNationalHolidays)


