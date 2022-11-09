#setting up the workspace
setwd("F:/CUDenver_Fall2021/GEOG5050_AppliedSpatialStatistics/exercises/exercise06/ex06_data")
dev.off()
rm(list = ls())
load(".Rdata")

#libraries
library(stats)

#read in data
vote = read.csv("vote.csv")
attach(vote)

#checking data
names(vote)

#checking correlation matrix to test for multicollinearity
cor(vote[, c(6:10, 13)])

#univariate linear models for both % unemployed and % urban population
plot(E2016_RPct2 ~ Pct_Urban, cex=0.5, pch=19, col="darkgray", xlab="% Urban", ylab="Republican Vote %",
     main="Republican Voting and Urban Areas")
abline(lm(E2016_RPct2 ~ Pct_Urban), col="red")
Mod01 = lm(E2016_RPct2 ~ Pct_Urban)
summary(Mod01)

plot(E2016_RPct2 ~ Pct_Unemp, cex=0.5, pch=19, col="darkgray", xlab="% Unemployed", ylab="Republican Vote %",
     main="Republican Voting and Unemployment")
abline(lm(E2016_RPct2 ~ Pct_Unemp), col="red")
Mod02 = lm(E2016_RPct2 ~ Pct_Unemp)
summary(Mod02)

#multivariate linear model examining both variable
model.mvlm = lm(E2016_RPct2 ~ Pct_Urban + Pct_Unemp, data=vote)
summary(model.mvlm, corr=TRUE)

#another linear model for minority population and republican voting
plot(E2016_RPct2 ~ SEth_NWhitePct, cex=0.5, pch=19, col="darkgray", xlab="% Minority", ylab="Republican Vote %",
     main="Republican Voting and Minorities")
abline(lm(E2016_RPct2 ~ SEth_NWhitePct), col="red")
Mod03 = lm(E2016_RPct2 ~ SEth_NWhitePct)
summary(Mod03)


#signing off
save.image()
