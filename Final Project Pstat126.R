library(readxl)
SeoulBikeData <- read_excel("Desktop/SeoulBikeData.xlsx", 
                            skip = 1)
View(SeoulBikeData)

bike = SeoulBikeData

# Renaming the variables for ease of access
cnt = bike$`Rented Bike Count`
hour = bike$Hour
seasons = bike$Seasons
holiday = bike$Holiday

temp = bike$`Temperature(°C)`
hum = SeoulBikeData$`Humidity(%)`
wind = bike$`Wind speed (m/s)`
rain = bike$`Rainfall(mm)`
snow = bike$`Snowfall (cm)`




#Running a quick and dirty visual check for possible transformations and/or general patterns in the data
pairs(SeoulBikeData[c('Rented Bike Count','Temperature(°C)', 'Rainfall(mm)','Wind speed (m/s)', 'Snowfall (cm)', 'Humidity(%)','Visibility (10m)')])

cnt1 = cnt+1 # There were some days with count = 0, we wanted to get rid of these
rain.new = (rain*1000 + 1)/1000 # lots of days with rainfall = 0
snow.new = (snow*1000 + 1)/1000 # lots of days with snowfall = 0
temp1 = temp+20 # We want to get rid of any negative temp values
hum1 = hum + 1 # We want to eliminate any zero humidity values
wind1 = wind + 1 # Lots of the windspeed values are equal to zero

#Looking at the viability of temperature as a predictor

temp.lm = lm(cnt1~temp1)
summary(temp.lm)
invTranPlot(cnt1~temp1, lambda = c(0,0.5,1,2), optimal = T) # check for possible transformations
plot(temp.lm)



#Looking at the viability of humidity as a predictor

hum.lm = lm(cnt1 ~ hum1)
summary(hum.lm)
invTranPlot(cnt1 ~ hum1, lambda = c(-1, 0, 0.5, 1)) # check for possible transformations
plot(cnt1 ~ hum1)


#Looking at the viability of wind speed as a predictor

wind.lm = lm(cnt1~wind1)
summary(wind.lm)
invTranPlot(cnt1~wind1, lambda = c(-1, 0, 0.5,1), optimal = T) # check for possible transformations
plot(cnt1~wind)
wind.inv = 1/wind1 # we found an optimal lambda value of -1.12, we decided to use -1 as a transformation
wind.inv.lm = lm(cnt1 ~ wind.inv)
summary(wind.inv.lm)


#Checking the viability of visibility as a predictor

vis.lm = lm(cnt1~visibility)
summary(vis.lm)
invTranPlot(cnt~visibility, lambda = c(0, 0.5,1), optimal = T) # check for possible transformations
plot(cnt~visibility)
vis.log.lm = lm(cnt~log(visibility)) # we decided to log transform since optimal lambda was -0.24
summary(vis.log.lm)


#Checking the viability of rainfall as a predictor

rain.lm = lm(cnt1 ~ rain.new)
summary(rain.lm) # this isn't great. We probably need to transform

# based on the pairs data, this looks like a candidate for a log transformation
invTranPlot(cnt1 ~ rain.new, lambda = c(-1,0,0.5,1)) # checking for possible transformations
rain.lm.new = lm(cnt1 ~ log(rain.new))
summary(rain.lm.new) # this is quite a bit better
plot(rain.lm.new)



#Checking the viability of snowfall as a predictor

snow.lm = lm(cnt1 ~ snow.new)
summary(snow.lm) # this is pretty bad. We'll probably need to transform
# similar to rainfall, this looks like a potential log transformation
invTranPlot(cnt1 ~ snow.new, lambda = c(-1,0,1))
snow.lm.new = lm(cnt1 ~ snow.new)
summary(snow.lm.new)
plot(snow.lm.new)


#Checking the viability of seasons as a predictor, note: seasons is categorical predictor

seasons.lm = lm(cnt1 ~ seasons)
summary(seasons.lm)
# This is a really strong predictor with an R squared north of 0.2. When we start to run AIC and BIC we'll use this as the smallest possible model

# checking the viability of holiday as a predictor, note: holiday is a categorical predictor
holiday.lm = lm(cnt1~holiday)
summary(holiday.lm)

# checking the viability of hour as a predictor. Although it's presented like a numeric predictor, hour seems a lot more logical as a categorical variable.
new.hour = as.factor(hour)
hour.lm = lm(cnt1 ~ new.hour)
summary(hour.lm)



#Question 1:
  
#Now we start to move into AIC/BIC in order to create the best possible model

# For forward selection, the first step is defining the range of models. f below is the maximum size of the model, including all variables listed above, as well as interactions between each numeric and each categorical variable.

f = ~ temp1*holiday + hum1*holiday + wind.inv*holiday + log(visibility)*holiday + log(rain.new)*holiday + log(snow.new)*holiday + seasons + temp1*seasons + hum1*seasons + wind.inv*seasons + log(visibility)*seasons + log(rain.new)*seasons + log(snow.new)*seasons +temp1*new.hour + hum1*new.hour + wind.inv*new.hour + log(visibility)*new.hour + log(rain.new)*new.hour + log(snow.new)*new.hour + seasons*new.hour + holiday*new.hour



# m0 is the smallest possible model we'll consider. Since seasons was a very strong predictor, we decided to use this to create the base model.
m0 = lm(cnt1 ~ seasons)

# define n for the purpose of calculating BIC. n is just the sample size of the data set.
n = 8760

# forward selection, BIC
m.bic.forward = step(m0, f , direction = 'forward', k = log(n))

# forward selection, AIC
m.aic.forward = step(m0, f, direction = 'forward', k = 2)


#Turning f from above into a linear model so we can run backwards selection

f.lm = lm(cnt1 ~ temp1*holiday + hum1*holiday + wind.inv*holiday + log(visibility)*holiday + log(rain.new)*holiday + log(snow.new)*holiday + seasons + temp1*seasons + hum1*seasons + wind.inv*seasons + log(visibility)*seasons + log(rain.new)*seasons + log(snow.new)*seasons +temp1*new.hour + hum1*new.hour + wind.inv*new.hour + log(visibility)*new.hour + log(rain.new)*new.hour + log(snow.new)*new.hour + seasons*new.hour + holiday*new.hour)

# backward selection, AIC
m.aic.backwards = step(f.lm, scope = c(lower = ~ temp1, upper = f), k = 2)

# backward selection, BIC
m.bic.backwards = step(f.lm, scope = c(lower = ~ temp1, upper = f), k = log(n) )



#Ultimately, we chose to go with BIC (forward and backward selection yielded the same results). We felt that it would be better to sacrifice some amount of goodness of fit for the sake of making the model less complex. 

m.optimal = lm(cnt1 ~ seasons + new.hour + log(rain.new) + temp1 + hum1 + holiday + 
                 log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                 seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility) + 
                 holiday:log(visibility))

summary(m.optimal)
plot(m.optimal) # This is really bad. Normality especially seems violated. We'll try and transform the response variable now that we've transformed the rest of the predictors


#We will run a Box Cox transformation to find the best suited transformation for our response 

boxCox(m.optimal, optimal = T) # look at transforming the response variable
# boxcox looks about halfway in between 0 and 1. Taking the square root of cnt1 might be a viable option.


# New model after taking the square root of the response 
m.more.optimal = lm(sqrt(cnt1) ~ seasons + new.hour + log(rain.new) + temp1 + hum1 + holiday + 
                      log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                      seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility) + 
                      holiday:log(visibility))

summary(m.more.optimal)
plot(m.more.optimal) # This still isn't great, and our adjusted R squared has gone down, but definitely an improvement over the untransformed response variable from a diagnostic perspective. Also, our RSE seems unreasonably high.



# While trying to fix the diagnostics and the high RSE, we played around with removing some of the predictors. Particularly, we noticed that holdiay on its own had a pretty low R squared and we felt that removing holiday and its interactions might yield better results.
m.maybe.more.optimal = lm(sqrt(cnt1) ~ seasons + new.hour + log(rain.new) + temp1 + hum1  + 
                            log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                            seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility))



summary(m.maybe.more.optimal)
# R squared is down slightly but we've drastically reduced RSE. We'll keep holiday out of the model.
plot(m.maybe.more.optimal)
# The QQ plot still looks really problematic. However, the other graphs seem to have a marked improvement.


#Questions 2:

#  adding transformed predictors from our optimal model into the data set
bike$cnt1 = cnt + 1

bike$temp1 = temp + 20

hum1 = SeoulBikeData$`Humidity(%)` + 1
bike$hum1 = hum1

rain1 = rain + 1
bike$rain.inv = 1/rain1
bike$rain.new = (rain*1000 + 1)/1000

bike$wind1 = wind + 1
bike$wind.inv = 1/wind1

bike$holiday = SeoulBikeData$Holiday
bike$visibility = SeoulBikeData$`Visibility (10m)`

bike$new.hour = as.factor(hour)

m.optimaler = lm(cnt1 ~ temp1 + hum1 + log(rain.new) + seasons + wind.inv + holiday + 
                   log(visibility) + temp1:seasons + hum1:seasons + seasons:wind.inv, data = bike)

m.maybe.more.optimal = lm(sqrt(cnt1) ~ seasons + new.hour + log(rain.new) + temp1 + hum1+ 
                            log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                            seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility), data = bike)

hour1 = subset(bike, Hour < 6)
hour1.lm = lm(sqrt(cnt1) ~ seasons + new.hour + log(rain.new) + temp1 + hum1+ 
                log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility), data = hour1)
summary(hour1.lm)


#Second hour interval: 6am-11am 

hour2 = subset(bike, (Hour > 5 & Hour < 12))
hour2.lm = lm(sqrt(cnt1) ~ seasons + new.hour + log(rain.new) + temp1 + hum1+ 
                log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility), data = hour2)
summary(hour2.lm)
#  most weight is spring + wind, least weight is winter + wind 


#Third hour interval: 12pm-5pm

hour3 = subset(bike, (Hour > 11 & Hour < 18))
hour3.lm = lm(sqrt(cnt1) ~ seasons + new.hour + log(rain.new) + temp1 + hum1+ 
                log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility), data = hour3)
summary(hour3.lm)


#Fourth hour interval: 6pm-11pm

hour4 = subset(bike, (Hour >17 & Hour<=23))
hour4.lm = lm(sqrt(cnt1) ~ seasons + new.hour + log(rain.new) + temp1 + hum1+ 
                log(visibility) + seasons:new.hour + seasons:temp1 + new.hour:log(rain.new) + 
                seasons:log(rain.new) + new.hour:temp1 + seasons:hum1 + seasons:log(visibility), data = hour4)
summary(hour4.lm)
