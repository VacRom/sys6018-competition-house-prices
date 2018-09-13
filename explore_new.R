# Exploratory analysis on the data and data cleaning

##########
# Step 1 #
##########
# Start reading in the data.

#####################################################################
# If you don't have these packages, uncomment and run these lines.  #
#####################################################################
# install.packages("mvoutlier")
# install.packages("dummies")

library(mvoutlier)
library(dummies)

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

# NA -> Mode Value
list.mode=c("MSZoning","Utilities","Electrical")
for (name in list.mode) {
  sum = summary(data[[name]])
  mode = names(which.max(sum))
  data[[name]][is.na(data[[name]])] = mode
}

# NA -> None
list.none = c("Exterior1st","Exterior2nd","KitchenQual","Functional","Alley",
              "MasVnrType","BsmtQual","BsmtExposure","BsmtCond","BsmtFinType1",
              "BsmtFinType2","FireplaceQu","GarageType","GarageFinish",
              "GarageQual","GarageCond","PoolQC","Fence","MiscFeature")
for (name in list.none) {
  levels(data[[name]]) = c(levels(data[[name]]), "None")
  data[[name]][is.na(data[[name]])] = "None"
}

# NA -> 0
list.zero = c("BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtFullBath",
              "BsmtHalfBath","GarageCars","GarageArea","MasVnrArea")
for (name in list.zero) {
  data[[name]][is.na(data[[name]])] = 0
}

# NA -> special case
data$SaleType[is.na(data$SaleType)] = "Oth"

# NA -> median value
data$LotFrontage[is.na(data$LotFrontage)] = median(data$LotFrontage[!is.na(data$LotFrontage)])

# Also, I noticed that there's a year for "2207". This is probably "2007".
# For the data where homes have no garage, set to 0.
data$GarageYrBlt[data$GarageYrBlt==2207] = 2007
data$GarageYrBlt[is.na(data$GarageYrBlt)] = 0

# Check the number of missing elements in each column
miss.2 = sapply(data, function(x) length(which(is.na(x))))
miss.2[miss.2>0]

# Now we check for levels, because some entries may not have been assumed
# to be factors. For example the first case with MSSubClass the diferent
# numbers actually correspond to different classes. We have to manually set
# these as levels instead.
numeric = sapply(data, function(x) length(which(is.numeric(x))))
numeric[numeric>0]

# Now which of these values are actually factors?
list.fact = c("MSSubClass","OverallCond","OverallQual")
for (name in list.fact) {
  data[[name]] = factor(data[[name]])
}

##########
# Step 4 #
##########
# Organize and order the factors in a logical way.

# Also we should try to make factors into ordered (ordinal) categorical variables.
# they make sense. (i.e. Low, Medium, High become 1, 2, and 3).
# TO COMPLETELY AUTOMATE THIS WE COULD SORT BY MEDIAN BUT THERE ARE CIRCUMSTANCES
# WHERE DUE TO INCOMPLETE DATA THAT WE COULD ACCIDENTALLY ORDER IT IN THE WRONG WAY

# (1) Special cases, where the factor levels are unique.

data$LotShape=ordered(data$LotShape,levels=c("Reg","IR1","IR2","IR3"))
data$BsmtExposure=ordered(data$BsmtExposure,levels=c("None","No","Mn","Av","Gd"))
data$Electrical=ordered(data$Electrical,levels=c("Mix","FuseP","FuseF","FuseA","SBrkr"))
data$Functional=ordered(data$Functional,levels=c("None","Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"))
data$GarageFinish=ordered(data$GarageFinish,levels=c("None","Unf","RFn","Fin"))
data$Fence=ordered(data$Fence,levels=c("None","MnWw","GdWo","MnPrv","GdPrv"))

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
      data[[ord]] = ordered(data[[ord]])
    }
  } else {
    for (ord in set[[2]]) {
      data[[ord]] = ordered(data[[ord]], levels=set[[1]])
    }
  }
}

##########
# Step 5 #
##########
# General analysis.

# Cutoff for training/testing sets
cutoff = 1460

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

# For rigorous calculations use this pacakge for skewness
install.packages("e1071")
library(e1071)

skewness(train.Y)
# Skew is 1.88

# Let's try the Box-Cox transform until we get about 0.
lam = -0.0719
skewness((train.Y**lam-1)/lam)
train.Y = (train.Y**lam-1)/lam

par(mfrow=c(1,1))
hist(train.Y,breaks=30)
skewness(train.Y)

# Rerun plots
par(mfrow=c(2,2))
for (i in names(data)) {plot(data[[i]][1:1460],train.Y,xlab=i)}
par(mfrow=c(1,1))
# The look a lot neater and centered.

# Let's look at the skew of other predictors
# Get the numerical columns
names=names(Filter(is.numeric, data))
skews=apply(data[names],2,skewness)
skew.df=as.data.frame(skews)[order(-skews),,drop=FALSE]
skew.df

#                    skews
# MiscVal       21.9359177 ***
# PoolArea      16.8896450 ***
# LotArea       12.8158428 ***
# LowQualFinSF  12.0825494 ***
# X3SsnPorch    11.3702193 ***
# KitchenAbvGr   4.3000437 **
# BsmtFinSF2     4.1440129 **
# EnclosedPorch  4.0018339 **
# ScreenPorch    3.9446658 **
# BsmtHalfBath   3.9295737 **
# MasVnrArea     2.6122492 **
# OpenPorchSF    2.5338111 **
# WoodDeckSF     1.8414861 **
# LotFrontage    1.6739910 **
# X1stFlrSF      1.4688493 **
# BsmtFinSF1     1.4244979 **
# GrLivArea      1.2687055 **
# TotalBsmtSF    1.1562997 **
# BsmtUnfSF      0.9188668 *
# X2ndFlrSF      0.8612320 *
# TotRmsAbvGrd   0.7579772 *
# Fireplaces     0.7331177 *
# HalfBath       0.6942096 *
# BsmtFullBath   0.6245111 *
# BedroomAbvGr   0.3261567 
# GarageArea     0.2391340 
# MoSold         0.1957833 
# FullBath       0.1675196 
# YrSold         0.1323308 
# GarageCars    -0.2194681 
# YearRemodAdd  -0.4507886 
# YearBuilt     -0.5994973 *
# GarageYrBlt   -3.9048146 **

# There is no assumption of normality for predictors so we can
# just leave it for now. Maybe come back later.

##########
# Step 6 #
##########
# Outliers

# This is a temporary dataset which emulates the data in the previous plots
temp.data = cbind(data[1:cutoff,],train.Y)

# Just based on initial observation we see these are potential outliers:

# Two points all the way out in the distance from the other points.
a=rownames(temp.data[temp.data$MiscVal>5000,])

# One VERY cheap home compared to how large the OpenPorchSF value is.
b=rownames(temp.data[(temp.data$OpenPorchSF>500) & (temp.data$train.Y<7.4),])

# Two points away from the large cluster.
c=rownames(temp.data[(temp.data$GrLivArea>4000) & (temp.data$train.Y<8.2),])

# Another lone point
d=rownames(temp.data[(temp.data$X1stFlrSF>4000),])
e=rownames(temp.data[(temp.data$TotalBsmtSF>6000),])
f=rownames(temp.data[(temp.data$BsmtFinSF1>5000),])
g=rownames(temp.data[(temp.data$BsmtFinSF2>1200),])

# Two lone points
h=rownames(temp.data[(temp.data$LotFrontage>250),])

# Lots of outliers in the distance
i=rownames(temp.data[temp.data$LotArea>100000,])

# Bring them together and see what's common about these points
cand=c(a,b,c,d,e,f,g,h,i)
table(cand)

# House 1299 alone accounts for 5 of these outliers
temp.data[1299,]

# Let's remove these points and see what happens
rem=unique(cand)
removed.data = temp.data[which(!rownames(temp.data) %in% rem),]

# Replot!
par(mfrow=c(2,2))
for (i in names(removed.data)) {plot(removed.data[[i]],removed.data$train.Y,xlab=i)}
par(mfrow=c(1,1))

##########
# Step 7 #
##########
# Generating relevant interacting variables

# Some variables can be combined with one another like TOTAL area of the house (excluding porches and things of that nature)
removed.data$TotalSF=removed.data$TotalBsmtSF+removed.data$X1stFlrSF+removed.data$X2ndFlrSF+removed.data$GarageArea
par(mfrow=c(1,1))
plot(removed.data$TotalSF,removed.data$train.Y)

# Repeat for the test data.
data$TotalSF=data$TotalBsmtSF+data$X1stFlrSF+data$X2ndFlrSF+data$GarageArea

##########
# Step 8 #
##########
# Print data.

# The data is now cleaned up and ready to be used in analysis
# Let's split the results into
# (1) Training predictors called, "train_X.csv"
# (2) Training results called, "train_Y.csv"
# (3) Testing predictors called, "test_X.csv"

cutoff = dim(removed.data)[1]
train.Y=as.data.frame(removed.data$train.Y)
names(train.Y) = "SalePrice"

train.out = removed.data[1:cutoff,]
train.out$train.Y = NULL

test.out = tail(data, 1459)

setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

write.csv(train.out, file="train_X.csv", row.names=FALSE)
write.csv(train.Y, file="train_Y.csv", row.names=FALSE)
write.csv(test.out, file="test_X.csv", row.names=FALSE)