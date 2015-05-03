# install neeed packages
#install.packages(c("ggplot2, car"))

# import finance data
# finance = read.csv("Finance data Reshaped.csv")
#A-TEAM

# Notes
# View availabule Datasets
data()

# load airquality
data(airquality)

# get variabule names
names(airquality)
# "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day" 

# remove missing values
airquality = na.omit(airquality)

# plot Data
plot(Ozone~Solar.R, data=airquality)

# calulate ozone mean
mean.Ozone = mean(airquality$Ozone)

# add ozone mean to the plot
abline(h=mean.Ozone)

# use lm to fit regression line
model1 = lm(Ozone~Solar.R, data=airquality)

# view intersept and slope of the model
model1

# plot the regression line
abline(model1, col="red")

# view plots for this model
plot(model1)
