#PART 1

#start with setting the working directory, saving your working script, loading data
CO = read.csv("CO_County.csv")
names(CO)

#calculating a new column that is going to tell us the percentage of population that votes
#exploring basic descriptive statistics for new column with some functions
CO$PctVote = CO$TotVote/CO$TotPop
summary(CO$PctVote)
hist(CO$PctVote)
boxplot(CO$PctVote)

#just changing the columns from proportions to percents
CO$PctVote2 = CO$PctVote * 100
CO$PctPov2 = CO$PctPov * 100

#we are attaching the table now b/c the command only attaches the field present at the time you attach it
attach(CO)

#scatterplot(x-axis variable, y-axis variable) - workhorse among bivariate plots
#can also use the ~ (tilde) b/w x and y which can be read as "as it varies with"
#plus some other parameters to mess with: cex=point size, pch=a nominal number that changes point symbol
plot(PctVote2 ~ PctPov2, cex=0.7, pch=19, xlab="Percent in Poverty",
     ylab="Percent of Population Voting in the 2012 Election",
     main="Voting and Poverty By County in Colorado")

#basic univariate linear regression
#abline() plots the linear model
Mod01 = lm(PctVote2~PctPov2)
summary(Mod01)
abline(lm(PctVote2~PctPov2), col="red")

#going to do the same thing as above but for looking at the percent of the population over 65
detach(CO)
CO$PctOld = CO$TotOv65/CO$TotPop
CO$PctOld2 = CO$PctOld * 100
attach(CO)
plot(PctVote2 ~ PctOld2, cex=0.7, pch=19, xlab="Percent of Population Over 65", 
     ylab="Percent of Population Voting in the 2012 Election", 
     main="Voting and Age Over 65 by County in Colorado")
abline(lm(PctVote2~PctOld2), col="red")

#test the correlation coefficients (pearsons r)
cor(PctOld2, PctVote2)
cor(PctPov2, PctVote2)

#other summary explorations of old people voting
Mod02 = lm(PctVote2~PctOld2)
summary(Mod02)

#being able to stratify data by groups/subsets of data is VERY important
#tapply() function gets statistics by group
#tapply(name of the variable, grouping variable, function to apply)
tapply(PctVote2, Region, summary)
tapply(PctVote2, Region, sd)
tapply(PCTOBM, Region , summary)
tapply(PCTOBM, Region, sd)

#group by boxplot
boxplot(PctVote2~Region)
boxplot(PCTOBM~Region)

#ANOVA test between multiple groups will be helpful here
anov1 = aov(PctVote2 ~ Region)
anov1
summary(anov1)

#bartlett test looks at if the groups have equal variance
#the null hypothes is that the groups HAVE equal variance (homoscedasticity)
hov1 = bartlett.test(PctVote2~Region)
hov1

#kruskal-wallis the non-parametric alternative to ANOVA
kruskal.test(PctVote2~Region)

#checking to see if ggplot2 is there, installing, and loading
is.element("ggplot2", installed.packages())
install.packages("ggplot2")
library(ggplot2)

#how to display voting differences by region in a linear model and on a scatterplot...
ggplot(CO, aes(x=PctPov2, y=PctVote2, color=Region)) + geom_point() + geom_smooth(method="lm")

#devising my own question and hypothesis with this data
#linear regression model looking at percent high school diploma and percent voters
detach(CO)
CO$PctHS = CO$NumHS/CO$TotPop
CO$PctHS2 = CO$PctHS * 100
attach(CO)
plot(PctVote2 ~ PctHS2, cex=0.7, pch=19, col="tan", xlab="Percentage of Population with a High School Diploma",
     ylab="Percentage of Population Voting in the 2012 Election",
     main="Voting and High School Diploma by County in Colorado")
mod03 = lm(PctVote2 ~ PctHS2)
summary(mod03)
abline(lm(PctVote2 ~ PctHS2), col="blue")
cor(PctHS2, PctVote2)

#switching gears: loading climate data
#as.is function tells R not to convert text information into group-membership factors
orstationc = read.csv("orstationc.csv", as.is=T)
attach(orstationc)

#crude map in a graphical space
plot(lon, lat)
text(lon, lat, labels=station)
plot(elev, pann)
text(elev, pann, labels=station)

#a quick way to see lots of plots at once - scatterplot matrix
#[2,:10] means all rows and colunms 2 through 10 of the dataframe
plot(orstationc[,2:10])

#other explorations of the data
plot(elev, lat)

#correlation matrix
cor(elev, pann)
cor(orstationc[,2:10])

#done
detach(orstationc)

#load data
orsim = read.csv("orsim.csv")
attach(orsim)
names(orsim)
boxplot(TJan1x, TJan2x)

#the temperature difference
print(mean(TJan2x) - mean(TJan1x))

#two-sample t-test to examine groups with different variances
tt.TJan = t.test(TJan2x, TJan1x)
tt.TJan

#two tailed test on winter precipitation - checking for any difference, less or more
boxplot(PJan1x, PJan2x)
print(mean(PJan2x) - mean(PJan1x))
tt.PJan.1 = t.test(PJan2x, PJan1x)
tt.PJan.1

#one tailed test - testing if current precip is less than future
tt.PJan.2 = t.test(PJan2x, PJan1x, alternative='greater')
tt.PJan.2

detach(orsim)


#PART 3

#creating a function to calculate and print the distance between two coordinate pairs
#euclidean and manhattan

distance.calc = function(x1,y1,x2,y2)
        {ed = sqrt((x2-x1)^2 + (y2-y1)^2)
        md = abs(x1-x2) + abs(y1-y2)
        cat ("Euclidean distance:", ed,
             "\nManhattan distance:", md)
        }

distance.calc(145,11,3,-4)


#PART 2

#checking and installing packages
is.element("sf", installed.packages())
is.element("GISTools", installed.packages())
install.packages("sf", dep=TRUE)
library(tmap)
library(GISTools)
library(sf)
library(tmap)

#newhaven stuff - adding it in and getting it ready
data(newhaven)
ls()

#converting to sf
blocks_sf = st_as_sf(blocks)
breach_sf = st_as_sf(breach)
tracts_sf = st_as_sf(tracts)

#exploration stuff
names(blocks_sf)
data.frame(blocks_sf)

#mapping
tmap_mode('plot')
tm_shape(blocks_sf) +
        tm_polygons("P_18_24", breaks = c(5,10,20,40), title = "Population Age 18 to 24",
                    palette = "Purples", legend.hist = T) +
        tm_scale_bar(width = 0.20, position = "right") +
        tm_compass(position = c(0.8, 0.07)) +
        tm_layout(frame = T, title = "New Haven", title.size = 2, title.position = c(0.50, "top"),
                          legend.hist.size = 0.5)
        

#color pallettes
display.brewer.all()
brewer.pal(5, 'Blues')

help("tm_compass")
