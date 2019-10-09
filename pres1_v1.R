#EBGN 594 -- Presentation 1 -- Andrew Moore


#specify packages
require(quantmod)
require(fBasics)
require(tseries)
require(forecast)

#----------------------------------------------------------------------------
# Read in price series and clean up data
#----------------------------------------------------------------------------

#read in price series
#gold and Royal Gold (NASDAQ:RGLD)
getSymbols("GOLDAMGBD228NLBM",src="FRED")
getSymbols("RGLD",src="yahoo")
getSymbols("DGS10",src="FRED")
getSymbols("DFII10",src="FRED")
getSymbols("T10YIE",src="FRED")

#look at data
head(GOLDAMGBD228NLBM)
head(RGLD)
head(DGS10)
head(DFII10)
head(T10YIE)

#merge series together and omit NAs
goldRGLDrts <- merge.xts(RGLD$RGLD.Adjusted, GOLDAMGBD228NLBM$GOLDAMGBD228NLBM, 
                         DGS10$DGS10, DFII10$DFII10, T10YIE$T10YIE, all=TRUE, 
                         fill=NA, retclass="xts")
goldRGLDrts <- na.omit(goldRGLDrts)

#plot series
ts.plot(goldRGLDrts$RGLD.Adjusted)
ts.plot(goldRGLDrts$GOLDAMGBD228NLBM)
ts.plot(goldRGLDrts$DGS10)
ts.plot(goldRGLDrts$DFII10)
ts.plot(goldRGLDrts$T10YIE)

#----------------------------------------------------------------------------
# Obtain stationary series by taking differences (several data types)
#----------------------------------------------------------------------------

#convert each series to log
#goldRGLDrts$l.RGLD.Adjusted <- log(goldRGLDrts$RGLD.Adjusted)
#goldRGLDrts$l.GOLDAMGBD228NLBM <- log(goldRGLDrts$GOLDAMGBD228NLBM)

#go monthly
#convert to monthly
gold.mon <- to.monthly(goldRGLDrts$GOLDAMGBD228NLBM)
RGLD.mon <- to.monthly(goldRGLDrts$RGLD.Adjusted)
tenyr.mon <- to.monthly(goldRGLDrts$DGS10)
TIPyr.mon <- to.monthly(goldRGLDrts$DFII10)
BEI10.mon <- to.monthly(goldRGLDrts$T10YIE)

gold.mon <- log(gold.mon)
RGLD.mon <- log(RGLD.mon)

#take differences
Dgold.mon <- na.omit(diff(gold.mon))
DRGLD.mon <- na.omit(diff(RGLD.mon))
Dtenyr.mon <- na.omit(diff(tenyr.mon))
DTIPyr.mon <- na.omit(diff(TIPyr.mon))
DBEI10.mon <- na.omit(diff(BEI10.mon))

#just grab close column (converts to numeric)
Dgold.mon <- as.numeric(Dgold.mon[,4])
DRGLD.mon <- as.numeric(DRGLD.mon[,4])
Dtenyr.mon <- as.numeric(Dtenyr.mon[,4])
DTIPyr.mon <- as.numeric(DTIPyr.mon[,4])
DBEI10.mon <- as.numeric(DBEI10.mon[,4])


#examine gold ACF and PACF
acf(Dgold.mon)
pacf(Dgold.mon)


#regress gold on ten-year yield
xreg.mon <- cbind(Dtenyr.mon)
gold.reg1.m = auto.arima(Dgold.mon, max.order=100, xreg = xreg.mon)
gold.reg1.m

tsdisplay(residuals(gold.reg1.m))
tsdiag(gold.reg1.m)


#regress gold on ten-year BEI
xreg.mon <- cbind(DBEI10.mon)
gold.reg2.m = auto.arima(Dgold.mon, max.order=100, xreg = xreg.mon)
gold.reg2.m

tsdisplay(residuals(gold.reg2.m))
tsdiag(gold.reg2.m)


#regress gold on ten-year TIPS yield
xreg.mon <- cbind(DTIPyr.mon)
gold.reg3.m = auto.arima(Dgold.mon, max.order=100, xreg = xreg.mon)
gold.reg3.m

tsdisplay(residuals(gold.reg3.m))
tsdiag(gold.reg3.m)