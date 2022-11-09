#loading it up, set working directory
is.element("sf", installed.packages())
library(sf)
library(tmap)
library(GISTools)
library(spatstat)

#reading in the files for use
tornus.sf = st_read("torn_tchdown.shp")
states.sf = st_read("States.shp")
studyframe.sf = st_read("StudyFrame.shp")

#check the type of object
typeof(tornus.sf)
class(tornus.sf)

#need to convert sf format to sp format to use the GISTools package
tornus.sp = as(tornus.sf,"Spatial")
states.sp = as(states.sf,"Spatial")
frame.sp = as(studyframe.sf,"Spatial")

#check that everything is working by plotting it
plot(tornus.sp, pch=16, cex=0.5)

#basic isopleth map of tornadoes
torn_kde = kde.points(tornus.sp, h=250000, n=200, lims=NULL)
level.plot(torn_kde)
plot(states.sp, alpha=0, add=T)

#moving onto K-, G-, L-functions to determine at what distances clustering of tornadoes is significant

#clip tornadoes to the study area
tornus_clip = tornus.sf[studyframe.sf,]
states_clip = st_intersection(studyframe.sf, states.sf)

#checking everything is good by plotting a map
tm_shape(studyframe.sf) +
  tm_borders(col="black") +
  tm_layout(frame=F) +
  tm_shape(tornus_clip) +
  tm_dots(col=NA, size=0.02, shape=16, title=NA, legend.show=TRUE, legend.is.portrait=TRUE) +
  tm_shape(states_clip) +
  tm_borders(col="grey70", lw=2) +
  tm_layout(frame=F)

#in order to use the spatstat package, must first convert sf object to a ppp object
#do this by: sf to sp > then sp to ppp
tornus.sp = as(tornus_clip, "Spatial")
tornus.ppp = as(tornus.sp, "ppp")
#check it
class(tornus.ppp)

#ripleys K
kf = Kest(tornus.ppp, correction='border')
plot(kf)
kf.env = envelope(tornus.ppp, Kest, correction='border')
plot(kf.env, main="K-Function of Tornado Touchdowns around Iowa", xlab="bandwidth")

#l-function
lf.env = envelope(tornus.ppp, Lest, correction='border')
plot(lf.env, main="L-Function of Tornado Touchdowns around Iowa", xlab="bandwidth")

#g-plot
gf.env = envelope(tornus.ppp, Gest, correction='border')
plot(gf.env, main="G-Function of Tornado Touchdowns around Iowa", xlab="bandwidth")

#test to see if there is statistically significant clustering
mad.test(tornus.ppp, Kest, verbose=FALSE)
dclf.test(tornus.ppp, Kest, verbose=FALSE)
