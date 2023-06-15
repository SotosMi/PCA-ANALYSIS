# PCA ANALYSIS ON IRIS DATA

data("iris")
str(iris)

#The 3 classes are setosa , versicolor and virginica
table(iris$Species)
# n = 50

#There are 4 Features Sepal.Length , Sepal.Width, Petal.Length , Petal.Width 
head(iris)

#Step 1: Storing the dataset into a 150x4 by removing labels
iris2 <- iris[-5] #Removed species

#Whether to standardize depends on the measurment scales
#transform data onto unit scale mean = 0 and variance = 1
iris2 <- scale(iris2)

round(apply(iris2,2,mean,na.rm=TRUE),4) # All features mean = 0
round(apply(iris2,2,sd,na.rm=TRUE),4) # All featurs sd = 1


#covariance matrix in order to find eigenvectors : principal components
A <-cov(iris2)
round(A,4)

#Find the eigenvalues and eigenvectors
ev <- eigen(A)
round(ev$values,4) #eigenvalues that need to be sorted in order to find the highest
#eigenvector
round(ev$vectors,4)

# CALCULATING THE PROPORTION VARIANCE PVE 
# Tells us how many information can be attributed to each value
PVE <-ev$values/sum(ev$values)
round(PVE,4)
# we notice that the 1st has 72% of the variance
#2nd only 2%


#PVE SCREE PLOT
library(ggplot2)
PVEplot <-qplot(c(1:4),PVE)+
  geom_line()+
  xlab("Principal Component")+
  ylab("PVE")+
  ggtitle("Scree Plot")+
  ylim(0,1)
PVEplot

#The projection matrix W
#is the ev$vectors (eigenvectors we calculated) 
# we drop the last 2 collumns since their variance is low

W <- ev$vectors[,1:2]
W

#Faster way to
# perform principal components analysis
pca <- prcomp(iris2) 
pca # the Pricipal Components (eigenvector matrix)

# we actually capture 75% of variance in the entire dataset 
#by just using those two principal component
var_explained = pca$sdev^2 / sum(pca$sdev^2) # PVE



#for projecting newdata onto the PCA space.
#scale(newdata, pca$center, pca$scale) %*% pca$rotation 



#In the last step we use W to transform our samples onto a new subspace
# Y = SXW where S = standardized original dataset iris2 (150x4)
# Y = 150x2 matrix of our Transformed samples

pca<-prcomp(iris,Scale = T,Center = T) 
#standardizes automatically data



