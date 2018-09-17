# Implementation of kNN

##########
# Step 1 #
##########
# Make sure to run explore.R first!!!
# Then use linear_model.R!!!

wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

install.packages(c("caret","plotly","randomForest"))
library(caret)
library(plotly)
library(randomForest)

# Use the same dataset as from the last linear_model.R model
SalePrice = training$SalePrice
training$SalePrice = NULL
training.price = cbind(SalePrice,training)
names(training.price)
temp = rbind(training,test.out.2)

# Here are shortcuts so you don't have to run the ML models over and over again
results$var$var=read.csv("results_var_var.csv")
colnames(results$var$var) = "var"
forst$importance=read.csv("forst_importance.csv")

##########
# Step 2 #
##########
# Feature selection

# Remove all of the categorical variables
temp = temp[!sapply(temp, is.factor)]
training = training[!sapply(training,is.factor)]
training.price = training.price[!sapply(training.price,is.factor)]
test.out.2 = test.out.2[!sapply(test.out.2,is.factor)]

# I removed all of the PCA related things. It's not relevant to the model.

# Again we use the same feature selection techniques as before.
# Refer to the explore.R for more annotations.
set.seed(4.699)
clst=makeCluster(detectCores(),type="PSOCK")
registerDoParallel(clst)

control=rfeControl(functions=rfFuncs, method="cv", number=5) # Method: Cross Validation, Number: Number of Folds
size=c(2**(1:6))
results=rfe(training,SalePrice,sizes=size,metric="RMSE",maximize=FALSE,rfeControl=control) # Sizes: Number of features to retain.
print(results)
predictors(results)
plot(results,type=c("g","o"))

# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (5 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables   RMSE Rsquared     MAE  RMSESD RsquaredSD    MAESD Selected
#           2 0.1669   0.8286 0.12177 0.01625    0.02159 0.008279         
#           4 0.1574   0.8499 0.11228 0.01814    0.02464 0.009103         
#           8 0.1362   0.8883 0.09373 0.01326    0.01628 0.007920         
#          16 0.1338   0.8919 0.09152 0.01267    0.01328 0.005598         
#          32 0.1330   0.8936 0.09037 0.01302    0.01245 0.005442         
#          64 0.1322   0.8949 0.09016 0.01393    0.01326 0.006042        *
#         331 0.1327   0.8948 0.09009 0.01415    0.01143 0.006009         
# 
# The top 5 variables (out of 64):
# TotalSF, OverallQual, BsmtFinSF1, GrLivArea, OverallCond
# 
# [1] "TotalSF"             "OverallQual"         "BsmtFinSF1"          "GrLivArea"           "OverallCond"        
# [6] "YearBuilt"           "LotArea"             "YearRemodAdd"        "X1stFlrSF"           "TotalBsmtSF"        
# [11] "Fireplaces"          "X2ndFlrSF"           "GarageArea"          "BsmtUnfSF"           "LotFrontage"  

# Calculating variable importance
fordata = cbind(SalePrice,training)
attach(fordata)
forst = randomForest(SalePrice~.,data=fordata,importance=TRUE,ntree=5000)
detach(fordata)
import=forst$importance
mse = import[order(-import[,1]),][,1]
plot(1:dim(import)[1],mse)
head(mse)

# Write as csv - for future use!
write.csv(results$var$var, file="results_var_var.csv", row.names=FALSE)
write.csv(forst$importance, file="forst_importance.csv")

##########
# Step 3 #
##########
# Functions for KNN

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
  # cov: covariance matrix of A
  
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

knn = function(k,train,test,func,param) {
  # Computes the distances for k neighbors using a training set and a testing set
  #
  # k: number of neighbors
  # train: training data set, make sure SalePrice is the first column!
  # test: testing data set
  # func: Which function to use, minkowki or mahalanobis
  # param: Dimension parameter for minkowski. Ignored for mahalanobis
  
  tr.X = train[,-1]
  tr.Y = train[,1]
  
  if (func=="minkowski") {
    out=apply(test, 1, function(x) avg_neighbor(minkowski_list(x, tr.X, param), k, tr.Y))
  } else if (func=="mahalanobis") {
    cov=cov(tr.X)
    out=apply(test, 1, function(x) avg_neighbor(mahal_list(x, tr.X, cov), k, tr.Y))
  } else {
    print("Unknown function")
  }
return(out)
}

make_knn = function(k=5, train, test, func, param, file) {
  # Actually runs the knn models AND prints it as a csv.
  #
  # Params, same as before
  # file: Filename for the output i.e. "filename.csv"
  
  output=knn(k,train,test,func,param)
  out=as.data.frame(output)
  out=data.frame(names=row.names(out),out)
  names(out) = c("Id","SalePrice")
  out$SalePrice = exp(out$SalePrice)
  out$Id = 1461:(1461+dim(out)[1]-1)
  setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
  write.csv(out, file=file, row.names=FALSE)
  return(out$SalePrice)
}

cv = function(train=training, j=c(0.25, 0.50, 0.75, 1.00, 2.00, 3.00, 5.00, 50.00), knn=c(1,2,4,8,16,32),k=5,func="minkowski") {
  # Runs k fold cross validation on the knn model. Returns a dataframe which includes the dimensions tried by
  # the minkowski distance as well as the number of neighbors along with the mean log mse and sd of log mse.
  #
  # j: The dimensions to iterate through for minkowski distance.
  # knn: The number of different neighbors to try
  # k: The number of folds for cross validation
  
  # Output dataframe
  df = data.frame(matrix(ncol=4,nrow=0))
  
  # Create k folds
  folds = createFolds(1:dim(train)[1], k, list=TRUE, returnTrain=FALSE)
  
  # Iterate through the dimensions
  for (dim in j) {
    for (k.num in knn) {
      total.rmsle = c()
      # Iterate through folds
      for (i in (1:k)) {
        # This is the current fold
        curr=train[folds[[i]],]
        # Remove the sale price for later
        curr.sp=exp(curr[,1])
        # This will be the validation set
        curr=curr[,-1]
        # This is everything but that fold
        dat=train[-folds[[i]],]

        # Get the predictions
        if (func=="minkowski") {
          out=make_knn(k=k.num,as.matrix(dat),as.matrix(curr),"minkowski",dim,"empty.csv")
        } else if (func=="mahalanobis") {
          cov = cov(dat)
          out=make_knn(k=k.num,as.matrix(dat),as.matrix(curr),"mahalanobis",cov,"empty.csv")
        }
        
        # Calculate RMSLE
        rmsle=sqrt(mean((log(out+1)-log(curr.sp+1))**2))
        # Make this into a running list of RMSLE
        total.rmsle = append(total.rmsle, rmsle)
        
        # Print some diagnostics
        if (func=="minkowski") {
          print("Minkowski")
          print(c("Dim:",dim))
        } else if (func=="mahalanobis") {
          print("Mahalanobis")
        }
        print(c("Neighbors:",k.num))
        print(c("RMSLE:",rmsle))
        print("")
      }
      # Calcualte mean and sd of RMSLE and put into the dataframe
      mean.rmsle=mean(total.rmsle)
      sd.rmsle=sd(total.rmsle)
      row = c(k.num,dim,mean.rmsle,sd.rmsle)
      df = rbind(row,df)
    }
  }
  # Return the dataframe
  names(df) = c("k","Dimension","Mean RMSLE","SD RMSLE")
  return(df)
}

##########
# Part 4 #
##########
# Grid search for hyperparmeters and making the models.

# DON'T RUN ALL OF THIS! THIS WILL TAKE A VERY LONG TIME.
# ONLY DO SELECT ONES

# Let's begin with the full model.

####
# 5-NN Minkowski(0.3) on All Predictors
####

# Let's run our custom grid search.
cvtest=cv(training.price)

# Make a scatter plot and also order the list to look for trends
plot_ly(x=cvtest$k,y=cvtest$Dimension,z=cvtest$`Mean RMSLE`)
cvtest[order(cvtest$k,cvtest$`Mean RMSLE`),]
cvtest[order(cvtest$Dimension,cvtest$`Mean RMSLE`),]
cvtest[order(cvtest$`Mean RMSLE`),]

# Run a more fine test
cvtest=cv(training.price,j=c(0.30), knn=c(4,5,6), k=5)
plot_ly(x=cvtest$k,y=cvtest$Dimension,z=cvtest$`Mean RMSLE`)

# Let's make this model
make_knn(5, training.price,test.out.2,"minkowski",0.3,"knn_grid_search_1.csv")
setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(tempdf, file="cv_results_minkowski_full.csv", row.names=FALSE)
# Not bad results!

####
# 9-NN Minkowski(0.125) on 6 Predictors Selected by RFE
####

index = c("SalePrice", "TotalSF", "OverallQual", "GrLivArea", "BsmtFinSF1", "LotArea")
select = training.price[index]

cvtest.sel=cv(select)
plot_ly(x=cvtest.sel$k,y=cvtest.sel$Dimension,z=cvtest.sel$`Mean RMSLE`)

cvtest.sel=cv(select,j=c(0.100, 0.105, 0.110, 0.115, 0.120, 0.125, 0.130, 0.135, 0.140, 0.145, 0.150),knn=c(9),k=5)
plot_ly(x=cvtest.sel$k,y=cvtest.sel$Dimension,z=cvtest.sel$`Mean RMSLE`)

make_knn(9, select,test.out.2,"minkowski",0.125,"knn_grid_search_2.csv")
# This underperformed compare to the other models.

####
# 6-NN Mahalanobis on 6 Predictors Selected by RFE
####
# Let's try using the mahalanobis distance now.
cv.mahal=cv(select,j=c(0),k=5,func="mahalanobis")
plot(cv.mahal$k,cv.mahal$`Mean RMSLE`)

cv.mahal=cv(select,j=c(0),knn=c(4,5,6,7,8),k=5,func="mahalanobis")
plot(cv.mahal$k,cv.mahal$`Mean RMSLE`)

make_knn(6, select, test.out.2[colnames(select)[-1]], "mahalanobis", 0, "knn_grid_search_3.csv")
# Score isn't too good.

####
# 9-NN Minkowski(0.7) on All Normalized and Scaled Predictors
####

#### IF YOU WANT TO RUN ANY OF THE NORMALIZED TESTS RUN THIS BLOCK OF CODE ####

# Let's try minkowski again but normalize the data first.
names = colnames(training)
scale.X = scale(rbind(training,test.out.2))
scale.X = scale.X %*% diag(import[,1])
colnames(scale.X) = names
scale.train = scale.X[1:dim(training)[1],]
scale.test = scale.X[(dim(training)[1]+1):dim(scale.X)[1],]
scale.training.price = cbind(SalePrice,scale.train)




# Let's try a similar range from before
cvtest.scale=cv(scale.training.price,j=c(0.20, 0.25, 0.30, 0.35, 0.40), knn=c(1,2,3,4,5),k=5)
plot_ly(x=cvtest.scale$k,y=cvtest.scale$Dimension,z=cvtest.scale$`Mean RMSLE`)

cvtest.scale=cv(scale.training.price,j=c(0.68,0.69,0.70,0.71,0.72), knn=c(7,8,9,10,11),k=5)
plot_ly(x=cvtest.scale$k,y=cvtest.scale$Dimension,z=cvtest.scale$`Mean RMSLE`)

make_knn(9, scale.training.price,scale.test,"minkowski",0.7,"knn_grid_search_scale_1.csv")
# Very good score. Almost as good as linear regression!

####
# 7-NN Minkowski(0.7) on top 16 Predictors Selected by RFE Normalized and Scaled Predictors
####

# Finally the last one will be only select variables in the model
index = results$var$var[1:16,]
scale.select = as.data.frame(scale.training.price)[c("SalePrice",index)]
scale.cvtest.sel=cv(scale.select)
plot_ly(x=scale.cvtest.sel$k,y=scale.cvtest.sel$Dimension,z=scale.cvtest.sel$`Mean RMSLE`)

scale.cvtest.sel=cv(scale.select,j=c(0.65, 0.70, 0.75, 0.80),knn=c(6,7,8,9))
plot_ly(x=scale.cvtest.sel$k,y=scale.cvtest.sel$Dimension,z=scale.cvtest.sel$`Mean RMSLE`)

make_knn(7, scale.select, (as.data.frame(scale.test))[colnames(scale.select)[-1]],"minkowski",0.7,"knn_grid_search_scale_2.csv")
# Slightly worse results than from before

####
# Mahalanobis doesn't work well with too many variables since we get a singular correlation matrix
####

colSums(cor(scale.select)>0.8)

index = results$var$var[1:14]
scale.select = as.data.frame(scale.training.price)[c("SalePrice",index)]
colSums(cor(scale.select)>0.8)

scale.cvtest.sel=cv(scale.select, j=c(0), func="mahalanobis")
plot_ly(x=scale.cvtest.sel$k,y=scale.cvtest.sel$Dimension,z=scale.cvtest.sel$`Mean RMSLE`)

scale.cvtest.sel=cv(scale.select, knn=c(3,4,5,6),j=c(0), func="mahalanobis",file="knn_grid_search_mahalanobis.csv")
plot_ly(x=scale.cvtest.sel$k,y=scale.cvtest.sel$Dimension,z=scale.cvtest.sel$`Mean RMSLE`)

make_knn(4, scale.select, (as.data.frame(scale.test))[index], "mahalanobis", 0, "knn_grid_search_mahal.csv")

##############
# WARNING: EXPERIMENTAL 
##############

####
# What if we tune a scaling function?
####

# # Relative importance of different variables
# plot(import[,1]/colSums(import)[1])
# 
# # Increasing the power generates a more parsimonious model
# plot(import[,1]**2/colSums(import)[1])
# 
# # Reducing generates a more flexible model but can't do if importance is negative
# plot(import[,1]**(1/2)/colSums(import)[1])
# 
# # Let's call this parameter alpha
# 
# # This is testing with alpha = 2
# names = colnames(training)
# scale.X = scale(rbind(training,test.out.2))
# scale.X = scale.X %*% diag(import[,1]**2) ### THIS IS DIFFERENT
# colnames(scale.X) = names
# scale.train = scale.X[1:dim(training)[1],]
# scale.test = scale.X[(dim(training)[1]+1):dim(scale.X)[1],]
# scale.training.price = cbind(SalePrice,scale.train)
# 
# # Run the same test as before to see what we get
# cvtest.scale=cv(scale.training.price,j=c(0.28,0.3,0.32), knn=c(7,8,9),k=5)
# plot_ly(x=cvtest.scale$k,y=cvtest.scale$Dimension,z=cvtest.scale$`Mean RMSLE`)
# # About 0.3, 8 results in a score of about 0.1445
# make_knn(8, scale.training.price, (as.data.frame(scale.test))[colnames(scale.select)[-1]],"minkowski",0.3,"knn_grid_search_scale_5.csv")
# 
# # This is testing with alpha = 4
# names = colnames(training)
# scale.X = scale(rbind(training,test.out.2))
# scale.X = scale.X %*% diag(import[,1]**4) ### THIS IS DIFFERENT
# colnames(scale.X) = names
# scale.train = scale.X[1:dim(training)[1],]
# scale.test = scale.X[(dim(training)[1]+1):dim(scale.X)[1],]
# scale.training.price = cbind(SalePrice,scale.train)
# 
# # Run the same test as before to see what we get
# cvtest.scale=cv(scale.training.price,j=c(0.11,0.12,0.13,0.14,0.15), knn=c(8),k=5)
# plot_ly(x=cvtest.scale$k,y=cvtest.scale$Dimension,z=cvtest.scale$`Mean RMSLE`)
# # About 0.155, 8 results in a score of about 0.148