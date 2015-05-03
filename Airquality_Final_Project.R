data(airquality)

#names of the variables we will be working with
names(airquality)
# [1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"   

#Coding removes the NA's of the data, data cleaning
airquality=na.omit(airquality)
airquality

#Command allows us to see the structure of the data
str(airquality)

#Convert Month and Day to factors
factor(airquality$Month)
levels(airquality$Month)
airquality$Month=factor(airquality$Month)
str(airquality)
airquality$Day=factor(airquality$Day)

# Plotting Univariate Relationships
plot(Ozone~Solar.R,data=airquality)

#use lm to plot a regression line
Model1=lm(Ozone~Solar.R,data=airquality)
abline(Model1,col="Red")
#also w/ GGPLOT2
ggplot(airquality,aes(x=Solar.R,y=Ozone)) + geom_point() + geom_smooth(method="lm", se=FALSE)

#Summary Statistics for Regression line
summary(Model1)
Model1
#Coefficients:
# (Intercept)      Solar.R  
#     18.5987       0.1272 

#Assumptions
plot(Model1)

#Box Plot of Months (X) & Ozone (Y)
library(ggplot2)
ggplot(airquality, aes(x = Month, y = Ozone)) + geom_boxplot()

#Facet
ggplot(airquality, aes(x = Month)) + geom_dotplot(dotsize = 0.4)
ggplot(airquality, aes(x = Day)) + geom_dotplot(dotsize = 0.4)

#Multilinear Regression w/o DAY
airquality
names(airquality)
Model2=lm(Ozone~Solar.R+Wind+Temp+Month,data=airquality)
summary(lm(Ozone~Solar.R+Wind+Temp+Month,data=airquality))
Model2$residuals
plot(predict(Model2), Model2$residuals)
abline(h=0)
hist(Model2$residuals)

#Anova for Multi-Regression Model
anova(Model2)

#MLR model with a interation
Model3=lm(Ozone~Solar.R+Wind+Temp+Month+Solar.R:Wind,data=airquality)
summary(Model3)
anova(Model3)

#******************************************************************************
# VIF

# Load the "car" package
library(car)
# check for multicollinearity using vif
vif(Model3)
# Solar.R and wind are both found have a high vif (<5)
# compute vif w/o Solar.R
vif(lm(formula = Ozone ~ Wind + Temp + Month + Solar.R:Wind, data = airquality))
# coupute  vif w/o wind
vif(lm(formula = Ozone ~ Solar.R + Temp + Month + Solar.R:Wind, data = airquality))


#******************************************************************************
# Verify the CLT

# initilize parameters
mu = 100; sigma=10

# Create an empty vector of zeros, with 500 ellements
xbar5 = rep(0, 500)
# populate the vector by taking 5 samples from the normal population
for (i in 1:500)
  xbar5[i] = mean(rnorm(5, mean=mu, sd=sigma))
# plot a histogram
hist(xbar5, prob=TRUE, breaks=12, ylim=c(0, 0.5), xlim=c(90, 110), main="500 samplings each w/5 observations")

# Create an empty vector of zeros, with 500 ellements
xbar10 = rep(0, 500)
# populate the vector by taking 10 samples from the normal population
for (i in 1:500)
  xbar10[i] = mean(rnorm(10, mean=mu, sd=sigma))
# plot a histogram
hist(xbar10, prob=TRUE, breaks=12, ylim=c(0, 0.5), xlim=c(90, 110), main="500 samplings each w/10 observations")

# Create an empty vector of zeros, with 500 ellements
xbar111 = rep(0, 500)
# populate the vector by taking 111 samples from the normal population
for (i in 1:500)
  xbar111[i] = mean(rnorm(111, mean=mu, sd=sigma))
# plot a histogram
hist(xbar111, prob=TRUE, breaks=12, ylim=c(0, 0.5), xlim=c(90, 110), main="500 samplings each w/111 observations")
