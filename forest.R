install.packages("randomForest")
library(randomForest)

install.packages("mlbench")
library(mlbench)

install.packages("caret")
library(caret)

wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))

##########
# Part 1 #
##########
# Recall the data from explore_new.R
dim(train.out)
dim(train.Y)
dim(test.out)

##########
# Part 2 #
##########
# Feature selection

# Recursive Feature Elimination
# Run Random Forest Models over and over to determine which variables are
# the best to use for the actual modeling process.
set.seed(4.699)

# https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/rfeControl
control=rfeControl(functions=rfFuncs, method="cv", number=10) # Method: Cross Validation, Number: Number of Folds
size=c(1:32)
# https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/rfe
results=rfe(train.out,train.Y[[1]],sizes=size,rfeControl=control) # Sizes: Number of features to retain.
print(results)
predictors(results)
plot(results,type=c("g","o"))

# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables    RMSE Rsquared     MAE   RMSESD RsquaredSD    MAESD Selected
#           1 0.09719   0.6712 0.07184 0.007468    0.05207 0.004848         
#           2 0.07265   0.8147 0.05209 0.007171    0.02580 0.004105         
#           3 0.06470   0.8558 0.04565 0.005907    0.02227 0.003150         
#           4 0.06327   0.8625 0.04406 0.005552    0.01775 0.002611         
#           5 0.06187   0.8694 0.04290 0.006174    0.01847 0.003077         
#           6 0.05948   0.8776 0.04134 0.006294    0.01864 0.002898         
#           7 0.05724   0.8869 0.03999 0.006326    0.01928 0.003277         
#           8 0.05628   0.8916 0.03877 0.005896    0.01753 0.003121         
#           9 0.05614   0.8916 0.03887 0.005664    0.01667 0.002671         
#          10 0.05592   0.8929 0.03860 0.005623    0.01685 0.002631         
#          11 0.05585   0.8935 0.03871 0.005785    0.01848 0.003069         
#          12 0.05548   0.8941 0.03833 0.006001    0.01885 0.002874         
#          13 0.05538   0.8951 0.03810 0.005703    0.01685 0.002809         
#          14 0.05532   0.8953 0.03799 0.006231    0.01853 0.003007         
#          15 0.05515   0.8958 0.03781 0.005937    0.01723 0.002640         
#          16 0.05467   0.8979 0.03733 0.005938    0.01729 0.002828         
#          17 0.05449   0.8987 0.03730 0.005938    0.01759 0.002492         
#          18 0.05431   0.8990 0.03697 0.005957    0.01796 0.002461         
#          19 0.05402   0.9002 0.03676 0.005750    0.01704 0.002150         
#          20 0.05419   0.8998 0.03696 0.006112    0.01787 0.002496         
#          21 0.05403   0.9001 0.03680 0.006058    0.01784 0.002599         
#          22 0.05398   0.9006 0.03677 0.005964    0.01769 0.002503        *
#          23 0.05424   0.8998 0.03688 0.006122    0.01791 0.002612         
#          24 0.05408   0.9004 0.03679 0.005802    0.01735 0.002454         
#          25 0.05410   0.9005 0.03676 0.005953    0.01719 0.002433         
#          26 0.05422   0.9004 0.03671 0.006120    0.01746 0.002454         
#          27 0.05426   0.9000 0.03681 0.006153    0.01745 0.002589         
#          28 0.05434   0.8998 0.03681 0.006509    0.01854 0.002726         
#          29 0.05455   0.8993 0.03683 0.006002    0.01722 0.002419         
#          30 0.05445   0.8994 0.03686 0.006329    0.01808 0.002655         
#          31 0.05450   0.8994 0.03693 0.006122    0.01789 0.002390         
#          32 0.05430   0.9003 0.03664 0.006011    0.01717 0.002386         
#          80 0.05420   0.9008 0.03653 0.006537    0.01879 0.002661         
# 
# The top 5 variables (out of 22):
#   TotalSF, Neighborhood, OverallQual, MSSubClass, GrLivArea
# 
# [1] "TotalSF"      "Neighborhood" "OverallQual"  "MSSubClass"   "GrLivArea"    "BsmtFinSF1"   "OverallCond"  "TotalBsmtSF" 
# [9] "LotArea"      "X1stFlrSF"    "YearRemodAdd" "X2ndFlrSF"    "GarageArea"   "BsmtFinType1" "YearBuilt"    "ExterQual"   
# [17] "KitchenQual"  "GarageCars"   "FireplaceQu"  "GarageType"   "BsmtUnfSF"    "TotRmsAbvGrd"

# A 'light' model and a 'heavy' model. Let's try both!
index.1=predictors(results)[1:7]
index.2=predictors(results)[1:19]

##########
# Part 3 #
##########

# Modeling

# Make a function
make_forest = function(model, file) {
  out=predict(model,test.out)
  out=as.data.frame(out)
  out=data.frame(names=row.names(out),out)
  names(out) = c("Id","SalePrice")
  out$SalePrice = ((out$SalePrice*lam)+1)**(1/lam)
  setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
  write.csv(out, file=file, row.names=FALSE)
  setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))
}

light.rf = randomForest(SalePrice~.,data=cbind(train.out,train.Y)[c(index.1,"SalePrice")],ntree=10000)
light.rf
make_forest(light.rf,"random_forest_1.csv")
# Call:
# randomForest(formula = SalePrice ~ ., data = cbind(train.out,      train.Y)[c(index.1, "SalePrice")], ntree = 10000) 
# Type of random forest: regression
# Number of trees: 10000
# No. of variables tried at each split: 2
# 
# Mean of squared residuals: 0.003148098
# % Var explained: 88.72

heavy.rf = randomForest(SalePrice~.,data=cbind(train.out,train.Y)[c(index.2,"SalePrice")],ntree=10000)
heavy.rf
make_forest(heavy.rf,"random_forest_2.csv")
# Call:
# randomForest(formula = SalePrice ~ ., data = cbind(train.out,      train.Y)[c(index.2, "SalePrice")], ntree = 10000) 
# Type of random forest: regression
# Number of trees: 10000
# No. of variables tried at each split: 6
# 
# Mean of squared residuals: 0.002920576
# % Var explained: 89.53

heaviest.rf = randomForest(SalePrice~.,data=cbind(train.out,train.Y),ntree=10000)
heaviest.rf
make_forest(heaviest.rf,"random_forest_3.csv")
# Call:
# randomForest(formula = SalePrice ~ ., data = cbind(train.out,      train.Y), ntree = 10000) 
# Type of random forest: regression
# Number of trees: 10000
# No. of variables tried at each split: 26
# 
# Mean of squared residuals: 0.002943068
# % Var explained: 89.45

##########
# Part 4 #
##########
# Cross Validation

# There is no real need to use additional CV since RF inherently uses 1/3 of
# the input data as a validation set. Since several trees were used we can expect
# the accuracy to converge to a stable value.