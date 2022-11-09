#load libraries and data
COweather = read.csv('CO_Climate02.csv')
CO.shp = readOGR(dsn = getwd(), layer = "CO_County03")
library(maptools)
library(rgdal)

#explore data
head(COweather)
plot(CO.shp)

#plot the pre-projected station locations
plot(COweather$CO_X, COweather$CO_Y)

#setting up a map of points to help visualize data
COweather2 = SpatialPoints(cbind(COweather$CO_X, COweather$CO_Y))
COweather2@proj4string = CO.shp@proj4string
COweather2@bbox = CO.shp@bbox
COweather2 = SpatialPointsDataFrame(COweather2, data.frame(T_avg = COweather$ANN.TAVG.NORMAL))
plot(COweather2, pch = 16)                                  #Plot station locations
plot(CO.shp, add = TRUE)                                    #Add county boundaries for reference
library(RColorBrewer)                                       #Load the colors
pal = brewer.pal(5, "PuBu")                                 #Select the palette
spplot(COweather2, "T_avg", col.regions = pal, cuts = 7)    #Plot the map

#more libraries
install.packages('gstat', dep = TRUE)
library(lattice)
library(gstat)

#set the data, set up graphics parameters
attach(COweather)
plotvar = ANN.PRCP.NORMAL
plottitle = "CO Annual Average Precipitation"

#cloud plot
vgm.cloud = variogram(plotvar~1, loc = ~CO_X + CO_Y, data = COweather, cloud = T)
plot(vgm.cloud, main = paste("Variogram:", plottitle), identify = FALSE)

#build a basic variogram
vgm = variogram(plotvar~1, loc = ~CO_X + CO_Y, data = COweather)
plot(vgm, pch = 16, type = "b", col = "red", main = paste("Variogram:", plottitle))

#linear regression model of average annual temperature and elevation
plot(ELEVATION, ANN.TAVG.NORMAL, cex = 0.7, pch = 19, xlab = "Elevation", ylab = "Annual Average Temperature",
     main = "Elevation and Temperature in CO")
Mod01 = lm(ANN.TAVG.NORMAL ~ ELEVATION)
summary(Mod01)
abline(lm(ANN.TAVG.NORMAL ~ ELEVATION), col="red")

#examining residuals
TempMod = lm(ANN.TAVG.NORMAL ~ ELEVATION)
CO_resid = resid(TempMod)
par(mfrow = c(1,2))
plot(CO_X, CO_resid)
abline(lm(CO_resid ~ CO_X), col = "red")
plot(CO_Y, CO_resid)
abline(lm(CO_resid ~ CO_Y), col = "red")

#3D plot
cloud(CO_resid ~ CO_X*CO_Y, pch = 19, cex = 0.8, col = "red")

#semivariogram for the residuals
plotvar = CO_resid
plottitle = "Residuals of CO Average Annual Temperature"
vgm2 = variogram(plotvar~1, loc = ~CO_X + CO_Y, data = COweather)
plot(vgm2, pch = 16, type = "b", col = "red", main = paste("Variogram:", plottitle))

#directional semivariogram of temperature, normalized for elevation
dir.vgm  = variogram(plotvar ~ 1, loc = ~CO_X + CO_Y, data = COweather, alpha = c(0,45,90,135))
plot(dir.vgm, main = paste("Directional Variogram:", plottitle))

#part 3: characterizing your own data

#clearing workspace
dev.off()
rm(list = ls())

#loading in data
cars = read.csv("2020vehicledata.csv")
attach(cars)
head(cars)

#measures of central tendency, mpg
mean(comb_mpg)
median(comb_mpg)
mode(comb_mpg)
summary(comb_mpg)

#some plots checking normalcy
hist(comb_mpg)
qqnorm(comb_mpg, pch = 16, main = "Combined MPG")
qqline(comb_mpg, col = "steelblue", lwd = 2)
boxplot(comb_mpg, main = "Boxplot: Combined MPG")

#some plots for co2
hist(comb_co2)
qqnorm(comb_co2, pch = 16, main = "Combined C02 Emissions")
qqline(comb_co2, col = "steelblue", lwd = 2)
