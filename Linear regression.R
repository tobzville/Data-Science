install.packages("C50")
install.packages("gmodels")
install.packages("GGally")

library(C50)
library(gmodels)
library(GGally)

# import carbon_nanotubes.csv file
day = read.csv(file.choose())

# Explore the dataset
str(day)
names(day)
head(day)
day[1:5,]
hist(day$registered)

# check relationship between predictor and variables
ggpairs(data=day, columns=10:13, title="day data")

# plot scatterplot
attach(day)
plot(temp, atemp, main="Normalized Feeling Temperature vs 
     Normalized Temperature for bikers", col=c("red","blue"))
cor(temp, atemp)

# create a random sample for training and testing
set.seed(1)
day_rand = day[order(runif(731)),]

# split the data frames
day_train <- day[1:500, ]
day_test <- day[501:731, ]

# build the model
day_model = lm(atemp ~ temp, data=day_train)

# information about the model
day_model
names(day_model)
day_model$coefficients
confint(day_model)
day_model$fitted.values
day_model$residuals
summary(day_model)

errors <- residuals(day_model)
squared.errors <- errors ^ 2
mse <- mean(squared.errors)
rmse <- sqrt(mse)
rmse

# add line of best fit to the scatterplot
abline(day_model)

# use the model to predict atemp values for the temp values
testdata = data.frame(temp=0.2334567, atemp=0.2123425)
prediction = predict(day_model, testdata, interval = 'confidence')
prediction
names(prediction)
head(prediction)

prediction = predict(day_model, testdata, interval = 'prediction')
prediction
