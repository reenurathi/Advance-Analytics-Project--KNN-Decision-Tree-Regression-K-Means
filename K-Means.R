#Online shoppers intention dataset will carry out the clustering analysis
#Due to the heuristic nature of k-means, making any changes could have a different results
#for that reason, we try a cluster analysis more than once to test the robustness of our findings

#importing the data from CSV file
online_intention <- read.csv("DATA/online_shoppers_intention.csv")
#---------------------------------------------------------------------------------------
#before start the analysis. Exploring and Preparing data
#---------------------------------------------------------------------------------------
#exploring and preparing the data
str(online_intention)
summary(online_intention)
# as we can see there is not NA values 
#but there are four categorical values - Month, VisitorType, Weekend and Revenue- 
#to hold the original data, we create a new variable to preparing the data
intention <- online_intention
#Data	preparation	–	dummy	coding for categorical variable
#visitor type
intention$Return_visitor	<-	ifelse(intention$VisitorType	==	"Returning_Visitor",	1,	0) 
intention$New_visitor	<-	ifelse(intention$VisitorType	==	"New_Visitor",	1,	0)
#weekend
intention$no_weekend	<-	ifelse(intention$Weekend	==	"FALSE",	1,	0)
#Revenue
intention$no_renevue	<-	ifelse(intention$Revenue	==	"FALSE",	1,	0)
#month
#changing the values from months to number of month,
#changing the colum type from factor to numeric. due to K-means use numerical value
intention$Month <- factor(intention$Month, levels= c("Feb","Mar", "May", 
                                                             "June","Jul", "Aug", 
                                                             "Sep","Oct", "Nov", "Dec"),
                              labels = c("2","3","5","6","7","8","9","10","11","12"))
intention$Month <- as.numeric(as.character(intention$Month))
table(intention$Month)
#as K-means should be used only numerics values
#removing the categorical variables that were become to numeric in the step before.
intention <- intention[c(-16,-17,-18)]

#To	apply	z-score	standardization	to	the	interests	data	frame,	
#we	can	use	the	scale() function	with	lapply()
intention_z	<-	as.data.frame(lapply(intention,	scale))
#checking that the standarization is correct with the 5th columns 
summary(intention_z[1:5])
#All variables are numerical for the clustering analysis

#this second subset is for SECOND CLUSTER ANALYSIS when using any categorical variable
intention_z2 <- intention_z[c(-11,-16,-17,-18,-19)]
#------------------------------------------------------------------------------------
#WHAT IS THE BEST VALUE OF K - NUMBER OF CLUSTER
#-------------------------------------------------------------------------------------
#GENERAL INFORMATION
#because there is not an exactly idea how many cluster should be 
#we use this code which found on the website 
#https://analytics4all.org/2016/12/11/r-k-means-clustering-deciding-how-many-clusters/
wss <- (nrow(intention)-1)*sum(apply(intention,2,var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(intention,
                       centers=i)$withinss)
}
plot(1:10, wss, type="b", main= "K value", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#-------------------------------------------------------------------------------------
install.packages("stats") #we have the function kmeans
library(stats)
install.packages("ggplot2") #make the clusters graphs
library(ggplot2)
#-------------------------------------------------------------------------------------
#FIRST CLUSTER ANALYSIS - INCLUDE ALL THE VARIABLES IN THE ALGORITHM
#-------------------------------------------------------------------------------------
# aplied the K-means algorithm
#we are using three values for K (2,3,4) beacuse the elbow point calculate above.
#divide	online shoppers intention	into	two CLUSTERS
intention_clusters	<-	kmeans(intention_z,	2)
# exploring the clusters - size, centroids 
intention_clusters$size
intention_clusters$centers
#in order to plot the cluster, using different variables we apply a function
#m = x-axis - using the column on online_intention$ + any column that would like 
#n = y-axis - using the column on online_intention$ + any column that would like 
#o = colour of graphs - in this case we are using a cluster column as colour
#p = number of the graphs -  include on the title
funct_plot <- function(m,n,o,p){
  legend1 <- table(o)
  legend_num <- (names(legend1))
  plot(x = m, y = n, main = paste("Clusters Graph",p), xlab = names(m), 
       ylab = names(n), col=o)
  
  legend("bottomright", legend = names(legend1), fill =(legend_num), title = "Clusters", 
         col=(legend_num),lty=1:2, cex=0.8,box.lty=0) 
}

funct_plot(online_intention$BounceRates, online_intention$ExitRates,
          intention_clusters$cluster, 1.1) # graph 1
funct_plot(online_intention$Administrative, online_intention$ExitRates,
           intention_clusters$cluster, 1.2)# graph 2
funct_plot(online_intention$ProductRelated, online_intention$ProductRelated_Duration,
           intention_clusters$cluster, 1.3)# graph 3
#get the clusters means
aggregate(intention, by=list(intention_clusters$cluster), FUN = mean)
#append cluster assignment 
online_intention <- data.frame(online_intention, intention_clusters$cluster)

#-----------------------------------------------------------------------------------
#divide	online shoppers intention	into	three CLUSTERS
#THE SAME FUNCTION kmeans, but with another parameters
intention_clusters2	<-	kmeans(intention_z,	centers = 3, nstart = 25)
# exploring the clusters - size, centroids 
intention_clusters2$size
intention_clusters2$centers
#PLOTS
funct_plot(online_intention$BounceRates, online_intention$ExitRates,
           intention_clusters2$cluster, 1.4) # graph 4
funct_plot(online_intention$Administrative, online_intention$ExitRates,
           intention_clusters2$cluster, 1.5)# graph 5
funct_plot(online_intention$ProductRelated, online_intention$ProductRelated_Duration,
           intention_clusters2$cluster, 1.6)# graph 6
#get the clusters means
aggregate(intention, by=list(intention_clusters2$cluster), FUN = mean)
#append cluster assignment 
online_intention <- data.frame(online_intention, intention_clusters2$cluster)
#---------------------------------------------------------------------------------
#divide	online shoppers intention	into	four CLUSTERS
#THE SAME FUNCTION kmeans, but with another parameters
intention_clusters3	<-	kmeans(intention_z,	4)
# exploring the clusters - size, centroids 
intention_clusters3$size
intention_clusters3$centers
#PLOTS
funct_plot(online_intention$BounceRates, online_intention$ExitRates,
           intention_clusters3$cluster, 1.7) # graph 7
funct_plot(online_intention$Administrative, online_intention$ExitRates,
           intention_clusters3$cluster, 1.8)# graph 8
funct_plot(online_intention$ProductRelated, online_intention$ProductRelated_Duration,
           intention_clusters3$cluster, 1.9)# graph 9
#get the clusters means
aggregate(intention, by=list(intention_clusters3$cluster), FUN = mean)
#append cluster assignment 
online_intention <- data.frame(online_intention, intention_clusters3$cluster)
#---------------------------------------------------------------------------------------
#SECOND CLUSTER ANALYSIS
#---------------------------------------------------------------------------------------
# aplied the K-means alghoritm
#divide	online shoppers intention	into	two	clusters
intention_clusters4	<-	kmeans(intention_z2,	2)
# exploring the clusters - size, centroids 
intention_clusters4$size
intention_clusters4$centers
#improving model performance
funct_plot(online_intention$BounceRates, online_intention$ExitRates,
           intention_clusters4$cluster, 2.1) # graph 10
funct_plot(online_intention$Administrative, online_intention$ExitRates,
           intention_clusters4$cluster, 2.2)# graph 11
funct_plot(online_intention$ProductRelated, online_intention$ProductRelated_Duration,
           intention_clusters4$cluster, 2.3)# graph 12
#get the clusters means
aggregate(intention, by=list(intention_clusters4$cluster), FUN = mean)
#append cluster assignment 
online_intention <- data.frame(online_intention, intention_clusters4$cluster)
#------
#divide	online shoppers intention	into	three	clusters
intention_clusters5	<-	kmeans(intention_z2,	3)

# exploring the clusters - size, centroids 
intention_clusters5$size
intention_clusters5$centers
#PLOTS
funct_plot(online_intention$BounceRates, online_intention$ExitRates,
           intention_clusters5$cluster, 2.4) # graph 13
funct_plot(online_intention$Administrative, online_intention$ExitRates,
           intention_clusters5$cluster, 2.5)# graph 14
funct_plot(online_intention$ProductRelated, online_intention$ProductRelated_Duration,
           intention_clusters5$cluster, 2.6)# graph 15
#get the clusters means
aggregate(intention, by=list(intention_clusters5$cluster), FUN = mean)
#append cluster assignment 
online_intention <- data.frame(online_intention, intention_clusters5$cluster)
#-----
#divide	online shoppers intention	into	four	clusters
intention_clusters6	<-	kmeans(intention_z2, 4)

# exploring the clusters - size, centroids 
intention_clusters6$size
intention_clusters6$centers
#PLOT
funct_plot(online_intention$BounceRates, online_intention$ExitRates,
           intention_clusters6$cluster, 2.7) # graph 16
funct_plot(online_intention$Administrative, online_intention$ExitRates,
           intention_clusters6$cluster, 2.8)# graph 17
funct_plot(online_intention$ProductRelated, online_intention$ProductRelated_Duration,
           intention_clusters6$cluster, 2.9)# graph 18
#get the clusters means
aggregate(intention, by=list(intention_clusters6$cluster), FUN = mean)
#append cluster assignment 
online_intention <- data.frame(online_intention, intention_clusters6$cluster)