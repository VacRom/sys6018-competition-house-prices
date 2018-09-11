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
wd = "F:/2018 Fall/SYS 6018/assignments/kaggle/02_Housing/"
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

# Separate the SalesPrice from the rest of the data. It's not a predictor
train.Y = train$SalePrice
train$SalePrice = NULL

# Apply the temporary function across the data.
# This function takes each column and checks to see if there are any missing
# values in the column. Then it counts the number of indexes where there
# were missing values found. In other words, it outputs the number of missing values
# in each of the columns.
miss = sapply(train, function(x) length(which(is.na(x))))

# Outputs a table which shows how many values are missing for each column.
miss.df = data.frame(miss[miss>0])
names(miss.df) = "number_of_missing_values"
miss.df

# Total number of entries
n = dim(train)[1]

# Check as a percentage of missing values
miss.df.p = 100*miss.df/n
miss.df.p

#              number_of_missing_values
# LotFrontage               17.73972603
# Alley                     93.76712329
# MasVnrType                 0.54794521
# MasVnrArea                 0.54794521
# BsmtQual                   2.53424658
# BsmtCond                   2.53424658
# BsmtExposure               2.60273973
# BsmtFinType1               2.53424658
# BsmtFinType2               2.60273973
# Electrical                 0.06849315
# FireplaceQu               47.26027397
# GarageType                 5.54794521
# GarageYrBlt                5.54794521
# GarageFinish               5.54794521
# GarageQual                 5.54794521
# GarageCond                 5.54794521
# PoolQC                    99.52054795
# Fence                     80.75342466
# MiscFeature               96.30136986

# Repeat for the test data.
miss.t = sapply(test, function(x) length(which(is.na(x))))
data.frame(miss.t[miss.t>0])/dim(test)[1]

# MSZoning            0.002741604
# LotFrontage         0.155586018
# Alley               0.926662097
# Utilities           0.001370802
# Exterior1st         0.000685401
# Exterior2nd         0.000685401
# MasVnrType          0.010966415
# MasVnrArea          0.010281014
# BsmtQual            0.030157642
# BsmtCond            0.030843043
# BsmtExposure        0.030157642
# BsmtFinType1        0.028786840
# BsmtFinSF1          0.000685401
# BsmtFinType2        0.028786840
# BsmtFinSF2          0.000685401
# BsmtUnfSF           0.000685401
# TotalBsmtSF         0.000685401
# BsmtFullBath        0.001370802
# BsmtHalfBath        0.001370802
# KitchenQual         0.000685401
# Functional          0.001370802
# FireplaceQu         0.500342700
# GarageType          0.052090473
# GarageYrBlt         0.053461275
# GarageFinish        0.053461275
# GarageCars          0.000685401
# GarageArea          0.000685401
# GarageQual          0.053461275
# GarageCond          0.053461275
# PoolQC              0.997943797
# Fence               0.801233722
# MiscFeature         0.965044551
# SaleType            0.000685401

# There are different kinds of missing data for each of the data sets.
# This will make it very difficult to interpolate or fix missing data points.
# Instead let's try combining the data and fixing the missing data points as
# a whole instead, then reseparating them by ID (?)

data = rbind(train, test)
data$Id = NULL
head(data)

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

# MSZoning: Identifies the general zoning classification of the sale.
# Insert mode
summary(data$MSZoning)
data$MSZoning[is.na(data$MSZoning)] = "RL"

# Utilities: Type of utilities available
# Insert mode
summary(data$Utilities)
data$Utilities[is.na(data$Utilities)] = "AllPub"

# Exterior1st: Exterior covering on house
levels(data$Exterior1st) = c(levels(data$Exterior1st), "None")
data$Exterior1st[is.na(data$Exterior1st)] = "None"

# Exterior2nd: Exterior covering on house (if more than one material)
levels(data$Exterior2nd) = c(levels(data$Exterior2nd), "None")
data$Exterior2nd[is.na(data$Exterior2nd)] = "None"

# BsmtFinSF1: Type 1 finished square feet
data$BsmtFinSF1[is.na(data$BsmtFinSF1)] = 0

# BsmtFinSF2: Type 2 finished square feet
data$BsmtFinSF2[is.na(data$BsmtFinSF2)] = 0

# BsmtUnfSF: Unfinished square feet of basement area
data$BsmtUnfSF[is.na(data$BsmtUnfSF)] = 0

# TotalBsmtSF: Total square feet of basement area
data$TotalBsmtSF[is.na(data$TotalBsmtSF)] = 0

# BsmtFullBath: Basement full bathrooms
data$BsmtFullBath[is.na(data$BsmtFullBath)] = 0

# BsmtHalfBath: Basement half bathrooms
data$BsmtHalfBath[is.na(data$BsmtHalfBath)] = 0

# KitchenQual: Kitchen quality
levels(data$KitchenQual) = c(levels(data$KitchenQual), "None")
data$KitchenQual[is.na(data$KitchenQual)] = "None"

# Functional: Home functionality (Assume typical unless deductions are warranted)
levels(data$Functional) = c(levels(data$Functional), "None")
data$Functional[is.na(data$Functional)] = "None"

# GarageCars: Size of garage in car capacity
data$GarageCars[is.na(data$GarageCars)] = 0

# GarageArea: Size of garage in square feet
data$GarageArea[is.na(data$GarageArea)] = 0

# SaleType: Type of sale
data$SaleType[is.na(data$SaleType)] = "Oth"

# LotFrontage: Linear feet of street connected to property
# Using NA=0 will probably skew the data points to the left too much.
data$LotFrontage[is.na(data$LotFrontage)] = median(data$LotFrontage[!is.na(data$LotFrontage)])

# Alley: Type of alley access to property
# Factor NA as a third variable, it's not actually missing data
# Set this as "None"
levels(data$Alley) = c(levels(data$Alley), "None")
data$Alley[is.na(data$Alley)] = "None"

# MasVnrType: Masonry veneer type
# Again NA probably means none.
data$MasVnrType[is.na(data$MasVnrType)] = "None"

# MasVnrArea: Masonry veneer area in square feet
data$MasVnrArea[is.na(data$MasVnrArea)] = 0

# BsmtQual: Evaluates the height of the basement
levels(data$BsmtQual) = c(levels(data$BsmtQual), "None")
data$BsmtQual[is.na(data$BsmtQual)] = "None"

# BsmtExposure: Refers to walkout or garden level walls
levels(data$BsmtExposure) = c(levels(data$BsmtExposure), "None")
data$BsmtExposure[is.na(data$BsmtExposure)] = "None"

# BsmtCond: Evaluates the general condition of the basement
levels(data$BsmtCond) = c(levels(data$BsmtCond), "None")
data$BsmtCond[is.na(data$BsmtCond)] = "None"

# BsmtFinType1: Rating of basement finished area
levels(data$BsmtFinType1) = c(levels(data$BsmtFinType1), "None")
data$BsmtFinType1[is.na(data$BsmtFinType1)] = "None"

# BsmtFinType2: Rating of basement finished area (if multiple types)
levels(data$BsmtFinType2) = c(levels(data$BsmtFinType2), "None")
data$BsmtFinType2[is.na(data$BsmtFinType2)] = "None"

# Electrical: Electrical system
# Replace with the mode since we can't calculate an 'average'
summary(data$Electrical[!is.na(data$Electrical)])
data$Electrical[is.na(data$Electrical)] = "SBrkr"

# FireplaceQu: Fireplace quality
levels(data$FireplaceQu) = c(levels(data$FireplaceQu), "None")
data$FireplaceQu[is.na(data$FireplaceQu)] = "None"

# GarageType: Garage location
levels(data$GarageType) = c(levels(data$GarageType), "None")
data$GarageType[is.na(data$GarageType)] = "None"

# GarageYrBlt: Year garage was built
# Either set arbitrarily to 0 or set as a factor.
# For now set to 0.

# Also, I noticed that there's a year for "2207". This is probably "2007".
data$GarageYrBlt[data$GarageYrBlt==2207] = 2007
data$GarageYrBlt[is.na(data$GarageYrBlt)] = 0

# GarageFinish: Interior finish of the garage
levels(data$GarageFinish) = c(levels(data$GarageFinish), "None")
data$GarageFinish[is.na(data$GarageFinish)] = "None"

# GarageQual: Garage quality
levels(data$GarageQual) = c(levels(data$GarageQual), "None")
data$GarageQual[is.na(data$GarageQual)] = "None"

# GarageCond: Garage condition
levels(data$GarageCond) = c(levels(data$GarageCond), "None")
data$GarageCond[is.na(data$GarageCond)] = "None"

# PoolQC: Pool quality
levels(data$PoolQC) = c(levels(data$PoolQC), "None")
data$PoolQC[is.na(data$PoolQC)] = "None"

# Fence: Fence quality
levels(data$Fence) = c(levels(data$Fence), "None")
data$Fence[is.na(data$Fence)] = "None"

# MiscFeature: Miscellaneous feature not covered in other categories
levels(data$MiscFeature) = c(levels(data$MiscFeature), "None")
data$MiscFeature[is.na(data$MiscFeature)] = "None"

miss.2 = sapply(data, function(x) length(which(is.na(x))))
miss.2[miss.2>0]

# Now we check for levels, because some entries may not have been assumed
# to be factors. For example the first case with MSSubClass the diferent
# numbers actually correspond to different classes. We have to manually set
# these as levels instead.
lv = sapply(data, function(x) levels(x))
data$MSSubClass = factor(data$MSSubClass)

##########
# Step 4 #
##########
# Organize and order the factors in a logical way.

# Also we should try to make factors into ordered categorical variables.
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
ord.3 = c("Utilities","LandSlope","CentralAir","PavedDrive")
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

# Now let's check which values are missing
miss.2 = sapply(data, function(x) length(which(is.na(x))))
miss.2[miss.2>0]

# Make lots of plots
par(mfrow=c(2,2))
for (i in names(data)) {plot(data[[i]][1:1460],train.Y,xlab=i)}
par(mfrow=c(1,1))

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
par(mfrow=c(1,1))
# Much more evenly distributed!
train.Y = log(train.Y)

# Rerun plots
par(mfrow=c(2,2))
for (i in names(data)) {plot(data[[i]][1:1460],train.Y,xlab=i)}
par(mfrow=c(1,1))

# Finally let's see how many categorical variables we have
cat.count = 0
total.count = 0
for (i in names(data)){
  total.count = total.count + 1
  if (is.factor(data[[i]])){
    cat.count = cat.count + 1
    print(i)
  }
}
cat.count   # 45
total.count # 79

##########
# Step 6 #
##########
# Outliers

# Outliers will be handled differently depending on each model used.

##########
# Step 7 #
##########
# Print data.

# The data is now cleaned up and ready to be used in analysis
# Let's split the results into
# (1) Training predictors called, "train_X.csv"
# (2) Training results called, "train_Y.csv"
# (3) Testing predictors called, "test_X.csv"

train.out = data[1:1460,]
test.out = data[1461:2919,]
train.Y=as.data.frame(train.Y)
names(train.Y) = "SalePrice"

setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

write.csv(train.out, file="train_X.csv", row.names=FALSE)
write.csv(train.Y, file="train_Y.csv", row.names=FALSE)
write.csv(test.out, file="test_X.csv", row.names=FALSE)

# Remember the sale price is log transformed!