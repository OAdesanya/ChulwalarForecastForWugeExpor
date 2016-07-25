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