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

# create normalization function 
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

# normalize the adult data
adult_n <- as.data.frame(lapply(adult[c(1,3,5,11,12,13)], normalize))

# set aside some data for testing purposes
set.seed(42)

#split the dataframe
adult_train = adult_n[1:30000, ]
adult_test = adult_n[30001:32561,]

# create labels for training and test data 
adult_train_labels <- adult[1:30000, 15] 
adult_test_labels <- adult[30001:32561, 15]

# Training a model on the data
library(class)
adult_test_pred <- knn(train = adult_train, test =
                        adult_test, cl = adult_train_labels, k=25)

summary(adult_test_pred)

# Create the cross tabulation of predicted vs. actual 
CrossTable(x = adult_test_labels, y = adult_test_pred, prop.chisq=FALSE)
