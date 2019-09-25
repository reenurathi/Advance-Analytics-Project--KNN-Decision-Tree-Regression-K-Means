##data source: https://archive.ics.uci.edu/ml/datasets/Wine
#178 rows, 14 columns on chemical analysis of wines grown in the same region in Italy but derived from three different cultivars
#load(".RData")


# load the necessary libraries
install.packages("gmodels")
install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)
library(gmodels)
library(class)

wine <- read.csv("DATA/wine.data", stringsAsFactors = FALSE, header = FALSE)
str(wine)
names(wine) <- c("Class","Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Magnesium",
                 "Total_phenols","Flavanoids","Nonflavanoid_phenols",
                 "Proanthocyanins","Colour_intensity","Hue","OD280/OD315","Proline")
nrow(wine)
ncol(wine)
str(wine)

#table of wine class
table(wine$Class)

# Show classes as percent of total
round(prop.table(table(wine$Class)) * 100, digits = 1)

# summarize the 12 numeric features
summary(wine[c("Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Magnesium",
               "Total_phenols","Flavanoids","Nonflavanoid_phenols",
               "Proanthocyanins","Colour_intensity","Hue","OD280/OD315","Proline")])


# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the data so all numeric values are between 0 and 1
wine_norm <- as.data.frame(lapply(wine[2:14], normalize))

# confirm that normalisation worked
summary(wine_norm$Ash)

# separate the data into two groups to be used for training and test sets
set <- createDataPartition(wine$Class, p=0.6, list=FALSE)
# create training and test data
wine_train <- wine_norm[set, ]
wine_test <- wine_norm[-set, ]

# create labels for training and test data
wine_train_labels <- wine[set, 1]
wine_test_labels <- wine[-set, 1]


## Step 3: Training a model on the data ----
# an initial K value of 3 is used, as it is common to use a value between 3 and 9
# the value for K should be an odd number to avoid draws in the classification
wine_test_pred <- knn(train = wine_train, test = wine_test,
                      cl = wine_train_labels, k=3)

## Step 4: Evaluating model performance 

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wine_test_labels, y = wine_test_pred,
           prop.chisq=FALSE)

confusionMatrix(table(wine_test_pred,wine_test_labels))
# using 3 nearest neighbours the accuracy for this training and test set is 94.37%

## Step 5: Improving model performance 

# use the scale() function to z-score standardize a data frame
wine_z <- as.data.frame(scale(wine[-1]))
View(wine_z)
# confirm that the transformation was applied correctly
summary(wine_z$Alcohol)

# create training and test datasets
wine_train1 <- wine_z[set, ]
wine_test1 <- wine_z[-set, ]

# re-classify test cases
wine_test_pred22 <- knn(train = wine_train1, test = wine_test1,
                      cl = wine_train_labels, k=3)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wine_test_labels, y = wine_test_pred22,
           prop.chisq=FALSE)

confusionMatrix(table(wine_test_pred22,wine_test_labels))
# the accuracy for this training and test set using z-scores is 94.37%, which is the same as previously.
# using z-scores will usually improve the accuracy of the algorithm, but in this case it had no effect.
# this is most likely due to the size of the dataset being not enough to show a difference when z-score is used.

# try several different values of k to find the highest accuracy, and compare to the previous value of 94.37% where k=3.

wine_test_pred5 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=5)
confusionMatrix(table(wine_test_pred5,wine_test_labels))
#using a k value of 5, the accuracy was 97.18%

wine_test_pred7 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=7)
confusionMatrix(table(wine_test_pred7,wine_test_labels))
#using a k value of 7, the accuracy was 95.77%

wine_test_pred9 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=9)
confusionMatrix(table(wine_test_pred9,wine_test_labels))
#using a k value of 9, the accuracy was 97.18%

wine_test_pred11 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=11)
confusionMatrix(table(wine_test_pred11,wine_test_labels))
#using a k value of 11, the accuracy was 94.37%

wine_test_pred19 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=19)
confusionMatrix(table(wine_test_pred,wine_test_labels))
#using a k value of 19, the accuracy was 95.77%

wine_test_pred29 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=29)
confusionMatrix(table(wine_test_pred29,wine_test_labels))
#using a k value of 29, the accuracy was 95.77%

wine_test_pred37 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=37)
confusionMatrix(table(wine_test_pred37,wine_test_labels))
#using a k value of 37, the accuracy was 95.77%

wine_test_pred47 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=47)
confusionMatrix(table(wine_test_pred47,wine_test_labels))
#using a k value of 47, the accuracy was 91.55%

wine_test_pred59 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=59)
confusionMatrix(table(wine_test_pred59,wine_test_labels))
#using a k value of 59, the accuracy was 94.37%

wine_test_pred69 <- knn(train = wine_train, test = wine_test,cl = wine_train_labels, k=69)
confusionMatrix(table(wine_test_pred69,wine_test_labels))
#using a k value of 69, the accuracy was 69.01%

## k=5 and k=9 returned the highest accuracy from the values we tested.


