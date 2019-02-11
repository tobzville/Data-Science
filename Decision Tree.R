install.packages("C50")
install.packages("gmodels")
library(C50)
library(gmodels)


# import carbon_nanotubes.csv file
adult = read.csv(file.choose())

# Explore the dataset
str(adult)
head(adult)
table(adult$marital_status)
table(adult$race)
table(adult$income)
summary(adult$occupation)

# set aside some data for testing purposes
set.seed(42)
adult_rand = adult[order(runif(48842)), ]
summary(adult$workclass)
summary(adult_rand$workclass)
head(adult$workclass)
head(adult_rand$workclass)

#split the dataframe
adult_train = adult_rand[1:40000, ]
adult_test = adult_rand[40001:48842,]
prop.table(table(adult_train$income)) 
prop.table(table(adult_test$income))

# build the model
adult_model <- C5.0(income~., data=adult_train)
adult_model
# display detailed information about the tree
summary(adult_model)
plot(adult_model)

# Create a factor vector of predictions on test data 
adult_predict <- predict(adult_model, adult_test)
adult_predict
summary(adult_predict)

# cross tabulation of predicted versus actual classes
CrossTable(adult_predict, adult_test$income,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('predicted income', 'actual income'))

