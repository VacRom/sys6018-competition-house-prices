wd = "F:/2018 Fall/SYS 6018/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

# install.packages("caret")
library(caret)

# Make sure to run explore.R first!
temp = data
Y = train.Y

# Remove all of the categorical variables
temp = temp[!sapply(temp, is.factor)]

# Standardize the data 
X.standard = as.data.frame(scale(temp))

train.standard = X.standard[1:1460,]
test.standard = X.standard[1461:2919,]

# Plot histograms
par(mfrow=c(3,4))
for (i in names(train.standard)){
  hist(train.standard[[i]], xlab=i, main=c("Histogram of ",i))
  print(i)}
par(mfrow=c(1,1))

# PCA 
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
PCA = prcomp(train.standard, scale.=TRUE)
variances = PCA$sdev**2
prop.variances = variances/sum(variances)

# Scree plot
plot(prop.variances, xlab="PC", ylab="Proportion of Variance", type="b",
     main = "Variance Explained by Principal Component")

plot(cumsum(prop.variances), xlab="PC", ylab="Cumulative Prop. of Var.", type="b",
     main = "Cumulative Variance Explained by Principal Component")
abline(h=0.80)

# This is not good, we can't get enough information with only a handful of variables.

summary(PCA)
subset = PCA$x[,1:17] # Cumulative 80%

# Plot principal components
biplot(PCA, scale=0)

# Clearly this will not work since we need to reduce the number of features down to <4.
# I can still see certain groupings of variables so I can see which values are
# correlated with one another.

all=cbind(Y,train.out)

# Some variables to consider
all.subset=all[,c("SalePrice","OverallQual","GarageArea","YearBuilt","GrLivArea","TotRmsAbvGrd")]
cor(all.subset)

# If I have to reduce to only 3 predictors
some.subset=all[,c("SalePrice","OverallQual","GarageArea","GrLivArea")]
cor(some.subset)

# Try searching by correlation?
subset = cbind(train.Y,train.standard)
corr=cor(subset)
# Check for correlations at least at .6
index=findCorrelation(corr, cutoff=0.6)
names(subset)[index]
cor(subset[index])
# The best features to use are those which are most correlated with
# SalePrice and least correlated with the other variables
# In this case it seems that the best predictors would be
# OverallQual, X1stFlrSF, and GarageCars
index = c("SalePrice","OverallQual","X1stFlrSF","GarageCars")
cor(subset[index])

### Functions for KNN

# Minkowski Distance
minkowski = function(end, start,p) {
  # Computes the Minkowski distance between two points.
  #
  # x: Point A, vector
  # y: Point B, vector
  # p: Parameter value p (p=1 corresponds to Manhattan distance, and p=2
  #        corresponds to the Euclidean distance.)
  
  dist = sum(abs(end-start)**p)**(1/p)
  return(dist)
}

minkowski_list = function(pt, A, p) {
  # Computes the Minkowski distance between a point and a set of points.
  #
  # pt: Point A, vector
  # A: Set of points, list of vectors
  # p: Parameter value p
  out=apply(A, 1, function(x) minkowski(pt,x,p))
  return(as.matrix(out))
}

# Mahalanobis Distance
mahal = function(x,y,cov) {
  # Computes the Mahalanobis distance.
  #
  # x: Point A
  # y: Point B
  # cov: covariance of the dataset
  
  z = as.matrix(x-y)
  a = solve(cov,z)
  dist = sqrt(sum(z*a))
  return(dist**2)
}

mahal_list = function(pt, A, cov) {
  # Computes the Mahalanobis distance between a point and a set of points.
  #
  # pt: Point A, vector
  # A: Set of points, list of vectors
  # p: Parameter value p
  out=apply(A, 1, function(x) mahal(pt,x,cov))
  return(as.matrix(out))
}

avg_neighbor = function(list, k, Y) {
  # Computes the average value of the response variables of k nearest neighbors.
  #
  # list: List of distances
  # k: Number of neighbors
  # Y: list of response variables
  
  values=head(sort(list),k)
  index=match(values,list)
  out=mean(Y[index])
  return(out)
}

# Temporary matricies with the data
sub.train = as.matrix(subset[index])
sub.test = as.matrix(test.standard[index[-1]])
Y = as.matrix(Y)

# Knn function

knn = function(k,train,test,func,param) {
  tr.X = train[,-1]
  tr.Y = train[,1]
  if (func=="minkowski") {
    out=apply(test, 1, function(x) avg_neighbor(minkowski_list(x, tr.X, param), k, tr.Y))
  } else if (func=="mahalanobis") {
    out=apply(test, 1, function(x) avg_neighbor(mahal_list(x, tr.X, param), k, tr.Y))
  } else {
    print("Unknown function")
  }
return(out)
}


tempcov = cov(sub.train[,-1])
tempcov

knn(5,sub.train,sub.test,"mahalanobis",tempcov)


###
# First attempt: 5-NN Minkowski 2 (Euclidean Distance)
###

output=knn(5,sub.train,sub.test,"minkowski",2)
out=as.data.frame(output)
out=data.frame(names=row.names(out),out)
names(out) = c("Id","SalePrice")
out$SalePrice = exp(out$SalePrice)

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(out, file="first_knn.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

###
# Second attempt: 15-NN Minkowski 2 (Euclidean Distance)
###

output=knn(15,sub.train,sub.test,"minkowski",2)
out=as.data.frame(output)
out=data.frame(names=row.names(out),out)
names(out) = c("Id","SalePrice")
out$SalePrice = exp(out$SalePrice)

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(out, file="second_knn.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

###
# Third attempt: 5-NN Minkowski 5
###

output=knn(5,sub.train,sub.test,"minkowski",5)
out=as.data.frame(output)
out=data.frame(names=row.names(out),out)
names(out) = c("Id","SalePrice")
out$SalePrice = exp(out$SalePrice)

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(out, file="third_knn.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

# So all of these models are pretty bad right now. We probably need a better
# selection of predictors, or maybe k-NN just can't capture all of the variance
# with such a few number of predictors (curse of dimensionality)

###
# Fourth attempt: 5-NN Mahalanobis
###

tempcov = cov(sub.train[,-1])

output=knn(5,sub.train,sub.test,"mahalanobis",tempcov)
out=as.data.frame(output)
out=data.frame(names=row.names(out),out)
names(out) = c("Id","SalePrice")
out$SalePrice = exp(out$SalePrice)

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(out, file="fourth_knn.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

