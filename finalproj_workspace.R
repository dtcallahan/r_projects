#signing on
setwd("F:/CUDenver_Fall2021/GEOG5050_AppliedSpatialStatistics/final_project/data")
dev.off()
rm(list = ls())
load(".Rdata")

#signing off
save.image()


###########################################################################################


#KDE USING ADEHABITATHR

#libraries
library(adehabitatHR)
library(sp)
library(rgdal)
library(raster)

#prep
jaguars = read.csv("jaguar_pantanal.csv", header=T)
names(jaguars)    #checks
jaguars.sp = jaguars[, c("name", "long", "lat")]
coordinates(jaguars.sp) = c("long", "lat")
proj4string(jaguars.sp) = CRS("+proj=longlat +datum=WGS84")
jaguars.sp2 = spTransform(jaguars.sp, CRS("+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"))
is.projected(jaguars.sp2)   #checks
wkt(jaguars.sp2)   #checks

#kde
kde_href = kernelUD(jaguars.sp2, h="href", grid=200, same4all=FALSE, kern=c("bivnorm"), extent=0.55)
image(kde_href)   #checks

#isopleths
ver95 = getverticeshr(kde_href, percent = 95, unin=c("m"), unout=c("km2"), standardize=FALSE)
ver50 = getverticeshr(kde_href, percent = 50, unin=c("m"), unout=c("km2"), standardize=FALSE)
plot(ver95)   #checks

#calculating area
kde_href_area = kernel.area(kde_href, percent=seq(50, 95, by=5), unin=c("m"), unout=c("km2"), standardize=FALSE)
print(kde_href_area)    #checks

#final output
kde_href_volume = getvolumeUD(kde_href, standardize=FALSE)
image(kde_href_volume)    #checks


#exporting output
write.csv(kde_href_area, "kde_areas_output.csv", row.names=TRUE)

aliceUD = raster(as(kde_href_volume$Alice, "SpatialPixelsDataFrame"))
andersonUD = raster(as(kde_href_volume$Anderson, "SpatialPixelsDataFrame"))
caimanUD = raster(as(kde_href_volume$Caiman, "SpatialPixelsDataFrame"))
daleUD = raster(as(kde_href_volume$Dale, "SpatialPixelsDataFrame"))
darylUD = raster(as(kde_href_volume$Daryl, "SpatialPixelsDataFrame"))
feraUD = raster(as(kde_href_volume$Fera, "SpatialPixelsDataFrame"))
fiaoUD = raster(as(kde_href_volume$Fiao, "SpatialPixelsDataFrame"))
lindaUD = raster(as(kde_href_volume$Linda, "SpatialPixelsDataFrame"))
marruaUD = raster(as(kde_href_volume$Marrua, "SpatialPixelsDataFrame"))
milagreUD = raster(as(kde_href_volume$Milagre, "SpatialPixelsDataFrame"))
picoleUD = raster(as(kde_href_volume$Picole, "SpatialPixelsDataFrame"))
selemaUD = raster(as(kde_href_volume$Selema, "SpatialPixelsDataFrame"))
wendyUD = raster(as(kde_href_volume$Wendy, "SpatialPixelsDataFrame"))

tmp = "F:/CUDenver_Fall2021/GEOG5050_AppliedSpatialStatistics/final_project/data"
writeRaster(aliceUD, filename=file.path(tmp, "aliceUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(andersonUD, filename=file.path(tmp, "andersonUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(caimanUD, filename=file.path(tmp, "caimanUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(daleUD, filename=file.path(tmp, "daleUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(darylUD, filename=file.path(tmp, "darylUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(feraUD, filename=file.path(tmp, "feraUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(fiaoUD, filename=file.path(tmp, "fiaoUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(lindaUD, filename=file.path(tmp, "lindaUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(marruaUD, filename=file.path(tmp, "marruaUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(milagreUD, filename=file.path(tmp, "milagreUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(picoleUD, filename=file.path(tmp, "picoleUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(selemaUD, filename=file.path(tmp, "selemaUD.tif"), format="GTiff", overwrite=TRUE)
writeRaster(wendyUD, filename=file.path(tmp, "wendyUD.tif"), format="GTiff", overwrite=TRUE)

writeOGR(obj=ver95, dsn=tmp, layer="ver95", driver="ESRI Shapefile")
writeOGR(obj=ver50, dsn=tmp, layer="ver50", driver="ESRI Shapefile")


###################################################################################


#making plots/graphs

image(kde_href)
plot(kde_href)
class(kde_href)

plot(kde_href_area$Alice)

head(ver95)
class(ver95)
plot(ver95$id == "Alice")
plot(ver80, col="blue")
plot(ver95) + plot(ver50, add=TRUE, col=1:13)

plot(kde_href_area)
print(kde_href_area)

image(kde_href_volume)
plot(kde_href_volume$Alice)
class(kde_href_volume)


###################################################################################


#tried using lscv, doesnt seem to be the best option - also takes a long ass time
kde_lscv = kernelUD(panthers.sp, h="LSCV", grid=200, same4all=FALSE, kern=c("bivnorm"), extent=0.25)
image(kde_lscv)
plotLSCV(kde_lscv)


#############################################################################################


#kde using GISTools - exercise 4
#good for individual home ranges - outputs a raster/spatial pixels data frame

library(sf)
library(GISTools)

all_cats.sf = st_read("jaguar_pantanal.shp")
all_cats.sp = as(all_cats.sf, "Spatial")
class(all_cats.sp)

plot(all_cats.sp)

all_cats_kde = kde.points(all_cats.sp, h=7500, n=200, lims=NULL)
level.plot(all_cats_kde)
class(all_cats_kde)

all_cats_raster = raster(as(all_cats_kde, "SpatialPixelsDataFrame"))
tmp = "F:/CUDenver_Fall2021/GEOG5050_AppliedSpatialStatistics/final_project/data"
writeRaster(all_cats_raster, filename = file.path(tmp, "all_catsUD.tif"), format="GTiff", overwrite=TRUE)


#############################################################################################


#moveVis
install.packages("moveVis")
library(moveVis)
library(sp)
is.element("move", installed.packages())   #checks

tracks = read.csv("tracks.csv", header=T)
tracks$timestamp = as.POSIXct(tracks$timestamp, format = "%m/%d/%Y %H:%M", tz = "UTC")
class(tracks$timestamp)   #checks
head(tracks)   #checks

tracks_df = methods::as(tracks, "data.frame")
class(tracks_df)
move_data = df2move(tracks_df,
                    proj = "+proj=longlat +datum=WGS84",
                    x = "long", y = "lat", time = "timestamp", track_id = "name")
class(move_data)    #checks

move_data = align_move(move_data, res = 1, unit = "hours")
frames = frames_spatial(move_data, path_colours = c("darkcyan", "gold1", "firebrick4"), map_service = "osm",
                        map_type = "terrain", alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(move_data, type = "label") %>%
  add_progress()

frames[[500]]   #checks

animate_frames(frames, out_file = "movement_vis.gif", width = 700, height = 500, res = 80)


#############################################################################################

#relationship between # of locations and area size

locs = read.csv("locs.csv", header=T)
names(locs)
attach(locs)

plot(area~locations, cex = 0.7, pch = 19, xlab = "# of Locations", ylab = "Total Area (km2)",
     main = "Number of Locations and Total Area of Jaguar Home Range")
Mod01 = lm(area~locations)
summary(Mod01)

abline(lm(area~locations), col = "red")


#############################################################################################


#DONE
