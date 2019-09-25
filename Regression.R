# Importing the packages and libarary for regression analysis
install.packages("psych")
install.packages("corrplot")
install.packages("regclass")
install.packages("leaps")
install.packages("bestglm")
library(psych)
library(corrplot)
library(regclass)
library(leaps)
library(bestglm)

#Regresssion- We are implementing the linear regression for which the Wine Quality datasets has taken from following link(https://archive.ics.uci.edu/ml/datasets/wine+quality).
#This has two different dataset, one is for white wine and another one is for red wine.The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine.For creating the model, both datasets are merges together two carry out a combined analysis.
#Red wine data has 1,599 observation and quality variable has levels from 3 to 8. White wine data has 4898 observation and quality variable has levels from 3 to 9. 
#Combined red and wine data has 6,497 observation and has levels from 3 to 9.
#Datasets contains 12 numerical fields which are #1 - fixed acidity #2 - volatile acidity #3 - citric acid #4 - residual sugar #5 - chlorides #6 - free sulfur dioxide #7 - total sulfur dioxide #8 - density #9 - pH #10 - sulphates #11 - alcohol 
#Output variable (based on sensory data): 
#12 - quality (score between 0 and 10)

#Importing red wine quality csv file.

redwine<- read.csv("DATA/winequality-red.csv")
redwine <- read.csv("DATA/winequality-red.csv", sep= ";")
#Viewing the imported red wine quailty file to make sure it is correctly imported
View(redwine)
# checking the  Structure and Summary for red wine
str(redwine)
summary(redwine)

#Importing white wine quality csv file
whitewine<- read.csv("DATA/winequality-white.csv")
whitewine <- read.csv("DATA/winequality-white.csv", sep=";")

#Viewing the imported red wine quailty file to make sure it is correctly imported
View(whitewine)

#checking the structure and Summary for white wine
str(whitewine)
summary(whitewine)

#Merging both red and white wine csv for analysis.

wine_redwhite <- rbind(whitewine, redwine)
#checking the calls of merged file.
class(wine_redwhite) # its outputted as a data frame

#Viewing the merged file
View(wine_redwhite)
head(wine_redwhite)

#checking the structure and Summary for merged file.

str(wine_redwhite) # Str results says the dataset is formatted the way it required for analysis.
summary(wine_redwhite) #Red wine data has 1,599 observation and quality variable has levels from 3 to 8. White wine data has 4898 observation and quality variable has levels from 3 to 9.#Combined red and wine data has 6,497 observation and has levels from 3 to 9.

#First Doing some initail exploratory data analysis to get a sense of data
#Checking for missing values in merged dataset.

colSums(is.na(wine_redwhite)) # no missing value found

#Exploring Relationship among all features using correaltion matrix
wine_corr <- cor(wine_redwhite)
summary(wine_corr)
#Plotting the scallertplot matrix to visualize using the corplot function of corplot libarary.
corrplot(wine_corr,method="number") #From the scatterplot matrix, we observed that there is not a linear relationship between the Quality(dependent) and the Predictors(independnet variables).

#checking Skewness and kurtosis of dataset using the describe function of psych library.
describe(wine_redwhite) #From describe function output, we observe that skewness coefficient for quality is 0.19 and kurtosis is 0.23 which shows that the distribution is approximately symmetric.

#As quality is our dependent variable chceking its distribution by Histogram and

hist(wine_redwhite$quality)
#printing the breaks of histogram to see where it is changing 
print(hist_1$breaks)

# Changing the number of breaks and adding labels and color to make it look good and understandable
hist_2 <- hist(wine_redwhite$quality, breaks = 5, col ="lightblue", xlab = "Quality ", main= " Histogram of Quality rating")

table(wine_redwhite$quality) #table to get frquency of different qualities in dataset

#Rather than using the entire dataset as its very larges dataset This is one of the reasons we need to use training and test set separation.
#After we have trained our model, we should test it on fresh examples so that we can avoid problems like overfitting. We can also estimate how well our model is performing given that it is facing new inputs. Based on the performance, we can proceed on developing our system further.
#so i am spliting the data into a training set and a testing set.
#As their names imply, the training set is used to train and build the model, and then this model is tested on the testing set
#Sampling the data randomly using seed and sample function
set.seed(1235)
wine_redwhite <- wine_redwhite[sample(nrow(wine_redwhite)),]

#Now Selecting 50% of data as sample from total number of rows of the data
split <- floor(nrow(wine_redwhite)/2)

wine_train <- wine_redwhite[0:split,] # Training Data
wine_test <- wine_redwhite[(split+1):(nrow(wine_redwhite)-1),]# Testing Data

# Checking the structure and summary of Wine training data.
str(wine_train)
summary(wine_train)

#Checking the structure and summary of Wine training data.
str(wine_test)
summary(wine_test)  

#First Regression Model using multiple variable
#Treating quality as a continuous variable, Estimating a multiple linear regression model.
#Creating the initial multinominal Linear Model with quality as a continuous dependent variable and Model with all other variables as predictors

lm1 <- lm(as.numeric(quality) ~ alcohol + volatile.acidity + free.sulfur.dioxide + sulphates + total.sulfur.dioxide + density + residual.sugar+ fixed.acidity + citric.acid+ chlorides + pH, data=wine_train)

summary(lm1) #Evaluation calculates Residual standard error = 0.74, Multi R squared = 0.28, and Adj R squared = 0.28
confint(lm1, level=0.95) # checking the model on 95% CI
#ANOVA's null hypothesis is whether group mean values of the dependent variable are not significantly different,
#while an alternative hypothesis is just that at least one of the factor level forms a group of observations which mean value is different from overall mean.
#So using anova on this multiple regression helps us indentifying the variable which are not significant for the  model.
anova(lm1) #Anova evaluation finds Citric Acid, Chlorides, and Fixed acidity are not significant in this model.
#ploting from model.
plot(lm1)
#Testing the prediction model
prediction1 <- predict(lm1, newdata = wine_test)
#testing the data with fresh value to prdict the quailty of wine
data123 <- data.frame(alcohol=10, volatile.acidity = .28, free.sulfur.dioxide = 34, sulphates =.48, total.sulfur.dioxide =120, density =.9856, residual.sugar=9, fixed.acidity = 7.2, citric.acid =.28, chlorides = .045, pH =3.02)
predict(lm1, data123)

#Graphical Analysis of Linear Regression assumption
#Assumption 1- residuals plot can be used to assess the assumption that the variables have a linear relationship
unstandardizedPredicted <- predict(lm1) # predicting the unstandardize quailty value using the above train model
unstandardizedResiduals <- resid(lm1) # calcuating the unstandardardize residuals values.
#calculating standardized values using the formula.
standardizedPredicted <- (unstandardizedPredicted - mean(unstandardizedPredicted)) / sd(unstandardizedPredicted)
standardizedResiduals <- (unstandardizedResiduals - mean(unstandardizedResiduals)) / sd(unstandardizedResiduals)
#creating standardized residuals plot
plot(standardizedPredicted, standardizedResiduals, main = "Standardized Residuals Plot", xlab = "Standardized Predicted Values", ylab = "Standardized Residuals")
#adding horizontal line to above plot
abline(0,0)

#Explanation of above Residual plot- values that are close to the horizontal line are predicted well. The points above the line are underpredicted and the ones below the line are overpredicted. 
#The linearity assumption is supported to the extent that the amount of points scattered above and below the line is equal.
#Residual histogram- using a histogram we may assess the assumption that the residuals are normally distributed

hist(standardizedResiduals, freq = FALSE)#creating residuals histogram

curve(dnorm, add = TRUE)#adding normal curve. it shows that Points in the residual plot are dispersed randomly or normally distributed.


#I am creating one more Linear Model 2 using only Volatile Acidity, Alcohol, free suphur dioxide and Sulphates as independent variables as linear model one was not significant enough
lm2 <-lm(as.numeric(quality)~ alcohol + free.sulfur.dioxide + sulphates + volatile.acidity, data=wine_train)

summary(lm2)#Residual standard error = 0.74, Multi Rsquared = 0.28, and Adj R sqaured = 0.27
confint(lm2, level=0.95) # checking the 95 CI
anova(lm2) #Anova indicates th all variables are significant in this model.
plot(lm2)
#An evaluation of both Linear Model 1 and Linear Model 2 concludes that neither model is well suited for predicting this data


#Testing the prediction model
prediction2 <- predict(lm2, newdata = wine_test)
#predicting the quality of wine with second model with fresh values of predictors
data1234 <- data.frame(alcohol=10, free.sulfur.dioxide = 34, sulphates =.48, volatile.acidity =.19)
predict(lm2,data1234)
#Root mean square Error of linear model
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- lm2$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error) 
predictionRMSE 
#cONCLUSION - Error is 74.12%- It seems that Liner regression is underestimating the true nature of the relationship and interesting things are being overlooked.
#An evaluation of both Linear Model 1 and Linear Model 2 concludes that neither model is well suited for predicting this data.
