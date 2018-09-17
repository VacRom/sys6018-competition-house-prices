# Exploratory analysis on the data and data cleaning

##########
# Step 1 #
##########
# Start reading in the data.

#####################################################################
# If you don't have these packages, uncomment and run these lines.  #
#####################################################################
install.packages(c("dummies","e1071","randomForest","mlbench","caret","mvoutlier","dummies","doParallel"))
library(e1071)
library(randomForest)
library(mlbench)
library(caret)
library(dummies)
library(doParallel)

##############################
# Set your working directory #
##############################
wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/original_files",sep=""))

# This is the descriptions of the data in a text format.
# It doesn't look clean in R, just open it up in MS Word.
desc = read.table("data_description.txt", sep="\n")

# It looks like there are a number of variables (mostly categorical)
train = read.csv("train.csv")
head(train)
test = read.csv("test.csv")
head(test)

##########
# Step 2 #
##########
# Basic data cleaning

# Separate the SalesPrice from the rest of the data. It's not a predictor.
train.Y = train$SalePrice
train$SalePrice = NULL

# Combine the test and train data by rows so we can perform operations
# on the entire data set at the same time.
data = rbind(train, test)
# The cutoff index between train and test
cutoff = dim(train)[1]

# Apply the temporary function across the data.
# This function takes each column and checks to see if there are any missing
# values in the column. Then it counts the number of indexes where there
# were missing values found. In other words, it outputs the number of missing values
# in each of the columns.
miss = sapply(data, function(x) length(which(is.na(x))))

# Outputs a table which shows how many values are missing for each column.
miss.df = data.frame(miss[miss>0])
names(miss.df) = "number_of_missing_values"
miss.df

# Total number of entries
n = dim(data)[1]

# Check as a percentage of missing values
miss.df.p = 100*miss.df/n
miss.df.p

# number_of_missing_values
# MSZoning                   0.13703323
# LotFrontage               16.64953751
# Alley                     93.21685509
# Utilities                  0.06851662
# Exterior1st                0.03425831
# Exterior2nd                0.03425831
# MasVnrType                 0.82219938
# MasVnrArea                 0.78794108
# BsmtQual                   2.77492292
# BsmtCond                   2.80918123
# BsmtExposure               2.80918123
# BsmtFinType1               2.70640630
# BsmtFinSF1                 0.03425831
# BsmtFinType2               2.74066461
# BsmtFinSF2                 0.03425831
# BsmtUnfSF                  0.03425831
# TotalBsmtSF                0.03425831
# Electrical                 0.03425831
# BsmtFullBath               0.06851662
# BsmtHalfBath               0.06851662
# KitchenQual                0.03425831
# Functional                 0.06851662
# FireplaceQu               48.64679685
# GarageType                 5.37855430
# GarageYrBlt                5.44707091
# GarageFinish               5.44707091
# GarageCars                 0.03425831
# GarageArea                 0.03425831
# GarageQual                 5.44707091
# GarageCond                 5.44707091
# PoolQC                    99.65741692
# Fence                     80.43850634
# MiscFeature               96.40287770
# SaleType                   0.03425831

# ID isn't a predictor
data$Id = NULL
dim(data)
# [1] 2919 79

##########
# Step 3 #
##########
# Outliers

# Make some general plots
par(mfrow=c(2,2))
for (name in names(train)) {plot(train[[name]],train.Y,main=name)}

# Just based on initial observation we see that the most significant outlier
# is two points away from the large cluster in GrLivArea.
# Thehre is also one point in LotFrontage
outlier.1=train[(train$GrLivArea[1:cutoff]>4000) & (train.Y<300000),]
outlier.2=train[(train$LotFrontage[1:cutoff]>300),]
outlier.1=rownames(outlier.1)
outlier.2=rownames(outlier.2[!is.na(outlier.2$LotFrontage),]) # Since there are NA values we need to clean it up
outlier=c(outlier.1,outlier.2)
outlier=unique(outlier)

# Let's remove these points and see what happens
temp=cbind(train,train.Y)
temp=temp[which(!rownames(temp) %in% outlier),]
cutoff=dim(temp)[1]

# Replot!
par(mfrow=c(2,2))
for (name in names(temp)) {plot(temp[[name]],temp$train.Y,main=name)}

# Merge testing and training predictors again so we can perform imputation
# Save the values of Y as train.Y
train.Y=temp$train.Y
temp$train.Y = NULL
all.X = rbind(temp,test)
rownames(all.X) = NULL

##########
# Step 4 #
##########
# Filling in missing values and adding factor levels.

# Let's begin by exploring these variables and seeing what we can do with them.
# There are some NA values---but these NA values aren't actually missing.
# In fact they do represent some useful information.
# For each individual case, determine what NA actually means.

# Regarding variable types, there are three general ones to keep in mind.
# 1. Numerical. These are simple numerical values
# 2. Factor (Ordered). These are categorical variables with some order to them
# (i.e. Rating from 1-10). We can turn these into numerical variables
# BUT BE WARNED THERE ARE A FEW ASSUMPTIONS THAT COME WITH THIS.
# 3. Factor (Unordered). These are categorical variables without order.
# (i.e. Red, Green, Blue). For these we can use one-hot encoding by using
# dummy variables to represent each of the different levels.

# For lot frontage... There is a strong linear relationship between
# LotArea and LotFrontage. We can impute using LotArea
par(mfrow=c(2,2))
for (name in names(all.X)) {plot(all.X[[name]],all.X$LotFrontage,main=name)}

### Experimenetal imputation ###

# Regress but remove noisy points to make it more linear
plot(all.X$LotArea,all.X$LotFrontage)
temp.X = all.X[(all.X$LotArea<30000) & (all.X$LotFrontage<150),]
plot(temp.X$LotArea,temp.X$LotFrontage)

regress = lm(LotFrontage~LotArea, data=temp.X)
all.X$LotFrontage[is.na(all.X$LotFrontage)]=regress$coefficients[1]+all.X$LotArea[is.na(all.X$LotFrontage)]*regress$coefficients[2]
hist(all.X$LotFrontage)
# Does this look reasonable? Close, could be a little better.
plot(all.X$LotFrontage,all.X$LotArea)
plot(all.X$LotFrontage,all.X$train.Y)

# Impute by other means

# First, I noticed that there's a year for "2207". This is probably "2007".
# For the data where homes have no garage, set to 0.
all.X$GarageYrBlt[all.X$GarageYrBlt==2207] = 2007
all.X$GarageYrBlt[is.na(all.X$GarageYrBlt)] = "None"

# NA -> Mode Value
list.mode=c("MSZoning","Utilities","Electrical","Functional","KitchenQual",
            "Exterior1st","Exterior2nd")
for (name in list.mode) {
  sum = summary(all.X[[name]])
  mode = names(which.max(sum))
  all.X[[name]][is.na(all.X[[name]])] = mode
}

# NA -> None
list.none = c("Alley",
              "MasVnrType","BsmtQual","BsmtExposure","BsmtCond","BsmtFinType1",
              "BsmtFinType2","FireplaceQu","GarageType","GarageFinish",
              "GarageQual","GarageCond","PoolQC","Fence","MiscFeature")
for (name in list.none) {
  levels(all.X[[name]]) = c(levels(all.X[[name]]), "None")
  all.X[[name]][is.na(all.X[[name]])] = "None"
}

# NA -> 0
list.zero = c("BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath",
              "BsmtHalfBath","GarageCars","GarageArea","MasVnrArea")
for (name in list.zero) {
  all.X[[name]][is.na(all.X[[name]])] = 0
}

# NA -> special case
all.X$SaleType[is.na(all.X$SaleType)] = "Oth"

# Check the number of missing elements in each column
miss.2 = sapply(all.X, function(x) length(which(is.na(x))))
miss.2[miss.2>0]

# Now we check for levels, because some entries may not have been assumed
# to be factors. For example the first case with MSSubClass the diferent
# numbers actually correspond to different classes. We have to manually set
# these as levels instead.
numeric = sapply(all.X, function(x) length(which(is.numeric(x))))
numeric[numeric>0]

# Now which of these values are actually factors?
list.fact = c("MSSubClass","GarageYrBlt","YrSold","MoSold","GarageYrBlt")
for (name in list.fact) {
  all.X[[name]] = factor(all.X[[name]])
  # Apply to test data
  test[[name]] = factor(test[[name]])
}

##########
# Step 5 #
##########
# Organize and order the factors in a logical way.

# Also we should try to make factors into ordered (ordinal) categorical variables.
# they make sense. (i.e. Low, Medium, High become 1, 2, and 3).
# TO COMPLETELY AUTOMATE THIS WE COULD SORT BY MEDIAN BUT THERE ARE CIRCUMSTANCES
# WHERE DUE TO INCOMPLETE DATA THAT WE COULD ACCIDENTALLY ORDER IT IN THE WRONG WAY

# (1) Special cases, where the factor levels are unique.

all.X$LotShape=ordered(all.X$LotShape,levels=c("Reg","IR1","IR2","IR3"))
all.X$BsmtExposure=ordered(all.X$BsmtExposure,levels=c("None","No","Mn","Av","Gd"))
all.X$Electrical=ordered(all.X$Electrical,levels=c("Mix","FuseP","FuseF","FuseA","SBrkr"))
all.X$Functional=ordered(all.X$Functional,levels=c("None","Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"))
all.X$GarageFinish=ordered(all.X$GarageFinish,levels=c("None","Unf","RFn","Fin"))
all.X$Fence=ordered(all.X$Fence,levels=c("None","MnWw","GdWo","MnPrv","GdPrv"))

# (2) Naming schemes, where the factor levels use a common naming scheme

ranks.1 = c("None","Po","Fa","TA","Gd","Ex")
ord.1 = c("ExterQual","ExterCond","BsmtQual","BsmtCond","HeatingQC",
          "KitchenQual", "FireplaceQu","GarageQual","GarageCond",
          "PoolQC")
set.1 = list(ranks.1,ord.1)

ranks.2 = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ")
ord.2 = c("BsmtFinType1","BsmtFinType2")
set.2 = list(ranks.2,ord.2)

# (3) Remaining, where the factor levels are already ordered

ranks.3 = "ordered"
ord.3 = c("Utilities","LandSlope","CentralAir","PavedDrive","OverallCond","OverallQual")
set.3 = list(ranks.3,ord.3)

toorder = list(set.1,set.2,set.3)

# Iterate through the above sets. If the order isn't predetermined then
# use the else statement to specify the order.
for (set in toorder) {
  if (set[[1]][1] == "ordered") {
    for (ord in set[[2]]) {
      all.X[[ord]] = ordered(all.X[[ord]])
      # Apply to test data
      test[[ord]] = ordered(test[[ord]])
    }
  } else {
    for (ord in set[[2]]) {
      all.X[[ord]] = ordered(all.X[[ord]], levels=set[[1]])
      # Apply to test data
      test[[ord]] = ordered(test[[ord]], levels=set[[1]])
    }
  }
}

##########
# Step 6 #
##########
# General analysis.

# Reconfigure the data
data=all.X

# Make lots of plots for each of the variables vs SalePrice
par(mfrow=c(2,2))
for (i in names(data)) {plot(data[[i]][1:cutoff],train.Y,xlab=i)}

# Comments:
# For MSZoning, there are several outliers for the factor level RL.
# For Pave, there are several outlier for the factor level Pave.
# For Alley, there are several outliers for the factor level None.
# Etc. In fact we see that there are several outliers at high SalesPrice values
# This is indicitive that a transformation has to be applied to the
# raw data before analysis! Let's try a logscale on the SalesPrice
par(mfrow=c(2,1))
hist(train.Y)
hist(log(train.Y))

# Skew is 1.88->0.12
skewness(train.Y)
skewness(log(train.Y))
train.Y=log(train.Y)

# Rerun plots
par(mfrow=c(4,4))
for (i in names(data)) {plot(data[[i]][1:cutoff],train.Y,xlab=i)}
par(mfrow=c(1,1))
# The look a lot neater and centered.

# Let's look at the skew of other predictors
# Get the numerical columns
names=names(Filter(is.numeric, data))
skews=apply(data[names],2,skewness)
skew.df=as.data.frame(skews)[order(-skews),,drop=FALSE]
skew.df[abs(skew.df)>1,,drop=FALSE]

#               skews
# MiscVal       21.92
# PoolArea      17.68
# LotArea       13.13
# LowQualFinSF  12.08
# X3SsnPorch    11.36
# LotFrontage    7.24
# KitchenAbvGr   4.30
# BsmtFinSF2     4.14
# EnclosedPorch  4.00
# ScreenPorch    3.94
# BsmtHalfBath   3.93
# MasVnrArea     2.62
# OpenPorchSF    2.53
# WoodDeckSF     1.84
# X1stFlrSF      1.26
# GrLivArea      1.07

# Since there are many values of 0 in the data we can do a log(x+1) transform on
# these variables where skew >0.5

# Make a temporary data frame for the log transformed variables
data.log = data
# Let's transform only a select number of predictors.
# I don't want to transform the others because (1) It doesnt make sense for
# counting variables or (2) I can add SF values together later and would need
# to transform all other SF values as well if one were changed
names = c("LotArea","GrLivArea","LotFrontage")
for (name in names) {
  print(name)
  data.log[[name]] = log(1+data[[name]])
  # Repeat on the test data
  test[[name]] = log(1+test[[name]])
}
par(mfrow=c(4,4))
for (i in names(data.log)) {plot(data.log[[i]][1:cutoff],train.Y,xlab=i)}
par(mfrow=c(1,1))

##########
# Step 7 #
##########
# Generating relevant interacting variables
data.int = data.log

# Some variables can be combined with one another like TOTAL area of the house (excluding porches and things of that nature)

# Total Area of House
data.int$TotalSF=data.int$TotalBsmtSF+data.int$X1stFlrSF+data.int$X2ndFlrSF+data.int$GarageArea
# Repeat for the test data.
test$TotalSF=test$TotalBsmtSF+test$X1stFlrSF+test$X2ndFlrSF+test$GarageArea

# Total Area of Porch+Deck+Pool
data.int$TotalSFMisc=data.int$WoodDeckSF+data.int$OpenPorchSF+data.int$EnclosedPorch+data.int$X3SsnPorch+data.int$ScreenPorch+data.int$PoolArea
test$TotalSFMisc=test$WoodDeckSF+test$OpenPorchSF+test$EnclosedPorch+test$X3SsnPorch+test$ScreenPorch+test$PoolArea

##########
# Step 8 #
##########
# Dummy (one-hot) encoding

# Clean-up
# (1) Training predictors called, "train_X.csv"
# (2) Training results called, "train_Y.csv"
# (3) Testing predictors called, "test_X.csv"

cutoff = dim(temp)[1]
train.Y=as.data.frame(train.Y)
names(train.Y) = "SalePrice"

train.out = data.int
# Drop ID
train.out$Id = NULL
test.out = test
test.out$Id = NULL

# This is a big distinction
# Should we use ORDINAL categorical variables or ONE HOT ENCODING?
# There is no clear answer but OHE is usually more robust
# USE THIS FOR NOW BUT WE CAN REMOVE IT LATER IF ORDINAL IS BETTER

names = names(Filter(is.factor, train.out))

dummy = dummy.data.frame(train.out, names=names)
dim(dummy)

# Complete the dataset
train.X = dummy[1:cutoff,]
test.X = dummy[(cutoff+1):dim(dummy)[1],]
training = cbind(train.X,train.Y)
dim(training)
dim(test.X)

############
# Step 8.5 #
############
# Make a secondary data-set where categorical variables aren't one-hot encoded
names.1 = names(Filter(is.factor, data.int))
names.2 = names(Filter(is.ordered, data.int))
# These are the unordered categorical factors
names   = names.1[!names.1 %in% names.2]
names
dummy.2 = dummy.data.frame(data.int, names=names)

##########
# Step 9 #
##########
# Print data.

setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

training$SalePrice = NULL
train.out = training
test.out = test.X

write.csv(train.out, file="train_X.csv", row.names=FALSE)
write.csv(train.Y, file="train_Y.csv", row.names=FALSE)
write.csv(test.out, file="test_X.csv", row.names=FALSE)

############
# Step 9.5 #
############
# Reapeat for dummy.2
train.out.2=dummy.2[1:cutoff,]
test.out.2=dummy.2[(cutoff+1):2916,]
write.csv(train.out.2, file="train_X_2.csv", row.names=FALSE)
write.csv(test.out.2, file="test_X_2.csv", row.names=FALSE)

############
#
# For the purposes of running the other R files you can stop here
#
############

###########
# Step 10 #
###########
# Feature selection

# Recursive Feature Elimination
# Run Random Forest Models over and over to determine which variables are
# the best to use for the actual modeling process.
set.seed(4.699)

#####################################
# WARNING THIS WILL EAT UP YOUR CPU #
#####################################

# Parallel computing
clst=makeCluster(detectCores(),type="PSOCK")
registerDoParallel(clst)

#####

# https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/rfeControl
control=rfeControl(functions=rfFuncs, method="cv", number=5) # Method: Cross Validation, Number: Number of Folds
size=c(2**(1:8))
# https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/rfe
results=rfe(train.out,train.Y[[1]],sizes=size,rfeControl=control) # Sizes: Number of features to retain.
print(results)
predictors(results)
plot(results,type=c("g","o"))

# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (5 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables  RMSE Rsquared    MAE  RMSESD RsquaredSD   MAESD Selected
#           2 0.204    0.740 0.1473 0.01263    0.03462 0.01280         
#           4 0.167    0.828 0.1159 0.00627    0.01095 0.00241         
#           8 0.156    0.851 0.1078 0.00541    0.00829 0.00365         
#          16 0.151    0.858 0.1047 0.00759    0.01295 0.00325         
#          32 0.145    0.870 0.0992 0.00774    0.01307 0.00384         
#          64 0.139    0.882 0.0947 0.00750    0.01214 0.00363         
#         128 0.139    0.882 0.0941 0.00700    0.01123 0.00351         
#         256 0.138    0.884 0.0939 0.00647    0.01001 0.00364        *
#         452 0.139    0.884 0.0940 0.00638    0.00993 0.00349 
# 
# The top 5 variables (out of 256):
#   TotalSF, GrLivArea, YearBuilt, LotArea, BsmtFinSF1
# 
# [1] "TotalSF"              "GrLivArea"            "YearBuilt"            "LotArea"             
# [5] "BsmtFinSF1"           "YearRemodAdd"         "ExterQualTA"          "X2ndFlrSF"           
# [9] "TotalBsmtSF"          "GarageArea"           "X1stFlrSF"            "TotalSFMisc"         
# [13] "LotFrontage"          "GarageCars"           "BsmtUnfSF"            "Fireplaces"          

#############
# Step 10.5 #
#############
# Repeat for dummy.2
set.seed(4.699)

clst=makeCluster(detectCores(),type="PSOCK")
registerDoParallel(clst)

control=rfeControl(functions=rfFuncs, method="cv", number=5) # Method: Cross Validation, Number: Number of Folds
size=c(2**(1:8))

results=rfe(train.out.2,train.Y[[1]],sizes=size,rfeControl=control)
print(results)
predictors(results)
plot(results,type=c("g","o"))

# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (5 fold) 
# 
# Resampling performance over subset size:
#   
#   Variables  RMSE Rsquared    MAE  RMSESD RsquaredSD   MAESD Selected
#           2 0.168    0.824 0.1222 0.01161     0.0231 0.00669         
#           4 0.160    0.841 0.1144 0.01125     0.0224 0.00385         
#           8 0.139    0.882 0.0952 0.01094     0.0188 0.00464         
#          16 0.136    0.886 0.0928 0.00985     0.0154 0.00288         
#          32 0.131    0.895 0.0893 0.00841     0.0128 0.00273         
#          64 0.131    0.896 0.0886 0.00876     0.0130 0.00367         
#         128 0.130    0.897 0.0880 0.00759     0.0110 0.00277         
#         256 0.130    0.897 0.0883 0.00834     0.0122 0.00269        *
#         354 0.131    0.896 0.0890 0.00823     0.0120 0.00276         
# 
# The top 5 variables (out of 256):
#   TotalSF, OverallQual, GrLivArea, LotArea, BsmtFinSF1
# 
# [1] "TotalSF"              "OverallQual"          "GrLivArea"            "LotArea"             
# [5] "BsmtFinSF1"           "OverallCond"          "YearBuilt"            "TotalBsmtSF"         
# [9] "X2ndFlrSF"            "YearRemodAdd"         "X1stFlrSF"            "ExterQual"           
# [13] "GarageArea"           "BsmtFinType1"         "KitchenQual"          "LotFrontage" 