install.packages("rattle")
install.packages("tidyverse")
library(rattle)
library(ggplot2)
library(e1071)
library(stringr)
library(rpart)
library(rattle)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

#Load the dataset
mushrooms<- read.csv("DATA/mushrooms.csv")

#-----------------------------------
#description

#Relevant Information:
#This data set includes descriptions of hypothetical samples
#corresponding to 23 species of gilled mushrooms in the Agaricus and
#Lepiota Family (pp. 500-525).  Each species is identified as
#definitely edible, definitely poisonous, or of unknown edibility and
#not recommended.  This latter class was combined with the poisonous
#one.  The Guide clearly states that there is no simple rule for
#determining the edibility of a mushroom; no rule like ``leaflets
#three, let it be'' for Poisonous Oak and Ivy.
#Number of Instances: 8124
#Number of Attributes: 22 (all nominally valued)
#Attribute Information: (classes: edible=e, poisonous=p)
#1. cap-shape:                bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s
#2. cap-surface:              fibrous=f,grooves=g,scaly=y,smooth=s
#3. cap-color:        brown=n,buff=b,cinnamon=c,gray=g,green=r, pink=p,purple=u,red=e,white=w,yellow=y
#4. bruises?:                 bruises=t,no=f
#5. odor:                     almond=a,anise=l,creosote=c,fishy=y,foul=f, musty=m,none=n,pungent=p,spicy=s
#6. gill-attachment:          attached=a,descending=d,free=f,notched=n
#7. gill-spacing:             close=c,crowded=w,distant=d
#8. gill-size:                broad=b,narrow=n
#9. gill-color:               black=k,brown=n,buff=b,chocolate=h,gray=g,
#green=r,orange=o,pink=p,purple=u,red=e,white=w,yellow=y
#10. stalk-shape:              enlarging=e,tapering=t
#11. stalk-root:               bulbous=b,club=c,cup=u,equal=e,rhizomorphs=z,rooted=r,missing=?
#12. stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s
#13. stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s
#14. stalk-color-above-ring:   brown=n,buff=b,cinnamon=c,gray=g,orange=o,pink=p,red=e,white=w,yellow=y
#15. stalk-color-below-ring:   brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
#16. veil-type:                partial=p,universal=u
#17. veil-color:               brown=n,orange=o,white=w,yellow=y
#18. ring-number:              none=n,one=o,two=t
#19. ring-type:                cobwebby=c,evanescent=e,flaring=f,large=l, none=n,pendant=p,sheathing=s,zone=z
#20. spore-print-color:        black=k,brown=n,buff=b,chocolate=h,green=r,orange=o,purple=u,white=w,yellow=y
#21. population:               abundant=a,clustered=c,numerous=n, scattered=s,several=v,solitary=y
#22. habitat:                  grasses=g,leaves=l,meadows=m,paths=p, urban=u,waste=w,woods=d
#-----------------------------------------------------

#from the data set i am going to try predict the edibility of a mushroom
#fist we must look and investigate the dataset to try identify what may need to be changed

#inspect the data
str(mushrooms)
summary(mushrooms)
head(mushrooms)
View(mushrooms)

#Count NAs
sapply(mushrooms, function(x){sum(is.na(x))})

#count blank entries
sapply(mushrooms, function(x){sum(x=='', na.rm=T)})

# Rename the variables. We have to give names to each variable and then specify the category for each variable.
#This adds meaning tho what we are doing but it is not neccessary for what we need to do

colnames(mushrooms) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")

# Defining the levels for the categorical variables 
## We make every variable into a factor so they are the same and easy to work with
mushrooms <- mushrooms %>% map_df(function(.x) as.factor(.x))

## We redefine each of the category for each of the variables
levels(mushrooms$edibility) <- c("edible", "poisonous")
levels(mushrooms$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushrooms$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushrooms$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushrooms$bruises) <- c("no", "yes")
levels(mushrooms$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushrooms$gill_attachement) <- c("attached", "free")
levels(mushrooms$gill_spacing) <- c("close", "crowded")
levels(mushrooms$gill_size) <- c("broad", "narrow")
levels(mushrooms$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushrooms$stalk_shape) <- c("enlarging", "tapering")
levels(mushrooms$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushrooms$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushrooms$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushrooms$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushrooms$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushrooms$veil_type) <- "partial"
levels(mushrooms$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushrooms$ring_number) <- c("none", "one", "two")
levels(mushrooms$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushrooms$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushrooms$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushrooms$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")



#check to make sure that all the variables are indeed changed to factors
glimpse(mushrooms)
#From the the results we can see that every variable is marke dwith a <fct> which states they are all indeed changed to factors

#every variable we have is categorical, so we need to see the number of categories we are talking about
numClass <- function(x){
  x <- length(levels(x))
}

x <- mushrooms %>% map_dbl(function(.x) numClass(.x)) %>% as_tibble() %>% 
  rownames_to_column() %>% arrange(desc(value))
colnames(x) <- c("Variable name", "Number of levels")
print(x)
#From the results we can see that gill_color has the most levels with 12 followed by cap_color and both stalk_colors

#the veil type only has one categorical factor therefore its completely useless to us and may even negatively effect the outcome of the modelling stage.So i had to take away the column
mushrooms <- mushrooms %>% select(- veil_type)

#i then have to see if there is any missing data as algorithm will not work otherwise
map_dbl(mushrooms, function(.x) {sum(is.na(.x))})
#from the results we can see that there is luckily no data missing


#########################################################################################

#for us to continue with the model i have to split the data into test and train sets
set.seed(1810)
mushsample <- caret::createDataPartition(y = mushrooms$edibility, times = 1, p = 0.8, list = FALSE)
trainMush <- mushrooms[mushsample, ]
testMush <- mushrooms[-mushsample, ]


# now i can check the splits in regards to the predicted variable

#original
round(prop.table(table(mushrooms$edibility)), 2)
#train
round(prop.table(table(trainMush$edibility)), 2)
#test
round(prop.table(table(testMush$edibility)), 2)


#############     Regression Tree    ####################
#because of the amount of categorical variables regression trees are the ideal amount
set.seed(1810)
regTree <- rpart(edibility ~ ., data = trainMush, method = "class")
regTree

#A confusion/error matrix that lets us know what the model is getting right and what it is getting wrong / errors its making
#it also states how accurate the algorithm is so you can see whether the the algorithm is worthwhile and results are trustworthy
caret::confusionMatrix(data=predict(regTree, type = "class"), 
                       reference = trainMush$edibility, 
                       positive="edible")

#from the results it shows us that 41 mushrooms were put as edible but they were actually poisonous
#i will us cp and penalty matrix parameters in order to give out predictions.
#the best way to change this is to set up a penalty matrix in the rpart of the function
#the penalty matrix penalizes the higher order differences to allow for a more accurate result

penalty_matrix <- matrix(c(0, 1, 10, 0), byrow = TRUE, nrow = 2)
penalty_mod_tree <- rpart(edibility ~ ., data = trainMush, method = "class", 
                            parms = list(loss = penalty_matrix))

caret::confusionMatrix(data=predict(penalty_mod_tree, type = "class"), 
                       reference = trainMush$edibility, 
                       positive="edible")

#introducing the penalty matrix gives out a perfect prediction
#100% accuracy with 3367 edible and 3133 poisonous


#using a cp parameter also increases the accuracy of the model
#the cp model basically controls and measures the complexity of the tree based off how large the user may want it
#in order to get the cp we need to idenify the cross validation errors, which is basically a technique used to help estimate the test error of a predictive model such as the one we are implimenting
#we start off with a very deep tree that can be pruned later
regTree <- rpart(edibility ~ ., data = trainMush, 
                    method = "class", cp = 0.00001)

#to use cp we have to to identify  the cp with the smallest cross validation error which can be doen using printcp and plotcp

#printcp
printcp(regTree)
#the results show that the smallest cp happened after the 5th split
plotcp(regTree)
regTree$cptable[which.min(regTree$cptable[, "xerror"]), "CP"]

#pruning is done to trim down the tree to try make is as simple as possible in order to reduce the overallcomplexity of the tree
#its mostly done in order to limit the chances of overfitting a tree and making it useless to user
#now we can start pruning the tree with the cp that gives lowest cross validation error

bestcp <- round(regTree$cptable[which.min(regTree$cptable[, "xerror"]), "CP"], 4)
regTree_pruned <- prune(regTree, cp = bestcp)

#now we can have a look at the tree as it stands now
rpart.plot(regTree_pruned, extra = 104, box.palette = "GnBu", 
           branch.lty = 3, shadow.col = "gray", nn = TRUE)

#now we have to see how the model performs on the train data
#table(trainMush$edibility, predict(regTree, type="class"))

caret::confusionMatrix(data=predict(regTree_pruned, type = "class"), 
                       reference = trainMush$edibility, 
                       positive="edible")

#we can see that there are no poisonous values that should have been edible and visa versa.
#3367 edible
#3133 poisonous
testTree <- predict(regTree, newdata = testMush)
caret::confusionMatrix(data = predict(regTree, newdata = testMush, type = "class"), 
                       reference = testMush$edibility, 
                       positive = "edible")
#from the results we can now see that we have a near enough perfect accuracy on the result on the training accuracy
#841 edible mushrooms and 783 poisonous

#the reason why i chose a regression tree was based off the amount of categorical variables within the dataset
#


