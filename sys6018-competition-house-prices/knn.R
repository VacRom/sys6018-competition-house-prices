dir = "F:/2018 Fall/SYS 6018/assignments/kaggle/02_Housing/data"
setwd(dir)

# Read in the data
train = read.csv("train_cleaned_dummy.csv")

# Remove all of the categorical variables OR use the new list w/ dummies
# cols = unlist(lapply(train, is.numeric))
# train = train[cols]

Y = train$SalePrice
train$SalePrice = NULL

### Here will be the data analyzed using all variables given to us including dummies

# Standardize the data
train.standard = as.data.frame(scale(train))

# Plot histograms
par(mfrow=c(3,4))
for (i in names(train.standard)){
  hist(train.standard[[i]], xlab=i, main=c("Histogram of ",i))
  print(i)}
par(mfrow=c(1,1))

# PCA 
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
PCA = prcomp(train, scale.=TRUE)
variances = PCA$sdev**2
prop.variances = variances/sum(variances)

# Scree plot
plot(prop.variances, xlab="PC", ylab="Proportion of Variance", type="b",
     main = "Variance Explained by Principal Component")

plot(cumsum(prop.variances), xlab="PC", ylab="Cumulative Prop. of Var.", type="b",
     main = "Cumulative Variance Explained by Principal Component")
abline(h=0.95)

# This is not good, we can't get enough information with only a handful of variables.

summary(PCA)
subset = PCA$x[,1:92] # Cumulative 80%

# Plot principal components
biplot(PCA, scale=0)

###

# If we are limiting our variables for selection, how about we try just omitting
# the categorical variables which we cannot order?

train.2 = read.csv("train_cleaned.csv")
Y.2 = train.2$SalePrice
train.2$SalePrice = NULL

name = names(Filter(is.numeric, train.2))
train.numerical = train.2[,name]

# Repeat the above
# Standardize the data
train.numerical.standard = as.data.frame(scale(train.numerical))

# Plot histograms
par(mfrow=c(3,4))
for (i in names(train.numerical.standard)){
  hist(train.numerical.standard[[i]], xlab=i, main=c("Histogram of ",i))
  print(i)}
par(mfrow=c(1,1))

# PCA 
PCA.2 = prcomp(train.numerical, scale.=TRUE)
variances = PCA.2$sdev**2
prop.variances = variances/sum(variances)

# Scree plot
plot(prop.variances, xlab="PC", ylab="Proportion of Variance", type="b",
     main = "Variance Explained by Principal Component")

plot(cumsum(prop.variances), xlab="PC", ylab="Cumulative Prop. of Var.", type="b",
     main = "Cumulative Variance Explained by Principal Component")
abline(h=0.95)

summary(PCA.2)
subset.2 = PCA.2$x[,1:25] # Cumulative 80%

# This is a little better but we still need 40 PC to explain about 95% of the variance

# Plot principal components
biplot(PCA.2, scale=0)

### Functions for KNN

# Minkowski Distance
minkowski = function(x,y,p){
  # Computes the Minkowski distance between two points.
  #
  # x: Point A, vector
  # y: Point B, vector
  # p: Parameter value p (p=1 corresponds to Manhattan distance, and p=2
  #        corresponds to the Euclidean distance.)
  
  dist = sum((end-start)**2)**(1/p)
  return(dist)
  }

# Mahalanobis Distance
mahal = function(x,y,cov){
  # Computes the Mahalanobis distance.
  #
  # x: Point A
  # y: Point B
  # cov: covariance of the dataset
  
  z = as.matrix(x-y)[1,]
  a = solve(cov,z)
  dist = sqrt(sum(z*a))
  return(dist)
}

# Remove outliers
train.mat = as.matrix(train)
means = colMeans(train.mat)
covar = cov(train.mat)

### ERROR ###

# mahalanobis(train, means, covar)
# We have a singular matrix! Let's try using the results from PCA above
# in order to get a more appropriate solution

### Let's try both datasets, just to show that this can be used to find outliers

par(mfrow=c(1,2))

subset.mat= as.matrix(subset)
means = colMeans(subset.mat)
covar = cov(subset.mat)
mahalanobis(subset, means, covar)
lists = mahalanobis(subset, means, covar)
plot(lists)
# See that there are a few outliers
threshold = 800
which(lists>threshold)

# Subset the previous dataset with those rows removed
sub = subset[-which(lists>threshold),]
lists = mahalanobis(sub, means, covar)
plot(lists)

# Repeat for the second data set
subset.2.mat= as.matrix(subset.2)
means = colMeans(subset.2.mat)
covar = cov(subset.2.mat)
mahalanobis(subset.2, means, covar)
lists = mahalanobis(subset.2, means, covar)
plot(lists)
which(lists>threshold)

# Subset this one as well
sub.2 = subset.2[-which(lists>threshold),]
lists = mahalanobis(sub.2, means, covar)
plot(lists)

### We need a different method of dimensionality reduction. How about using
# something along the lines of linear regression to find which variables are
# 'of the most importance'?

# Check to see which variables are most correlated with price
cor=t(as.data.frame(cor(Y,train)))
cor.abs=as.data.frame(unlist(lapply(cor[,1], abs)))
names(cor.abs) = c("abs.cor")

values = cor.abs[order(-cor.abs),,drop=FALSE][1:20,,drop=FALSE]
values

X.select = train[,c(rownames(values))]
Y.val = as.data.frame(Y)
names(Y.val) = c("SalesPrice")
# Here is the correlation matrix
cormat=cor(cbind(Y.val,X.select))
cormat