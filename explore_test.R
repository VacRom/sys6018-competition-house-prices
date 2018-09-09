#Explore Test Data
# Exploratory analysis on the data

# If you don't have this package, uncomment and run this next line

# install.packages("mvoutlier")
library(mvoutlier)

# install.packages("dummies")
library(dummies)

# Set your working directory
wd = "F:/2018 Fall/SYS 6018/assignments/kaggle/02_Housing/data"
setwd(wd)

# It doesn't look clean in R, just open it up in MS Word
desc = read.table("data_description.txt", sep="\n")

# It looks like there are a number of variables (mostly categorical)

test = read.csv("test.csv")
head(test)

# Apply the temporary function across the data
# This function takes each column and checks to see if there are any missing
# values. Then it puts their indexes into a list and outputs the length of
# this list
miss = sapply(test, function(x) length(which(is.na(x))))

# Output the data frame given that there are some missing values
miss.df = data.frame(miss[miss>0])
names(miss.df) = "number_of_missing_values"
miss.df

# Total number of entries
n = dim(test)[1]

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

# Let's explore these variables:

# LotFrontage: Linear feet of street connected to property
#
# With about 17% loss we should be able to interpolate this, unless it's
# supposed to mean 0. Check to see the distribution
hist(test$LotFrontage)
# These many are missing
sum(is.na(test$LotFrontage))
# It would nottest be ridiculous to assume that some houses have 0 lot frontage
summary(test$LotFrontage)
# The minimum value is 21, so maybe it is feasible that 0 is the valid answer.
# Maybe try plotting type of lot vs. frontage?
plot(test$LotConfig,test$LotFrontage)
# Maybe try checking which types of lots have the most missing values?
table(test$LotConfig[is.na(test$LotFrontage)])
# It looks like houses on the inside have the most missing values. This is
# because a house with no access to the sidewalk would have no or little
# access to the street. We are then justified with setting the value of NA to 0
test$LotFrontage[is.na(test$LotFrontage)] = 0

# Alley: Type of alley access to property
# Grvl	Gravel
# Pave	Paved
# NA 	No alley access
#
# Factor NA as a third variable, it's not actually missing data
# Set this as "None"
levels(test$Alley) = c(levels(test$Alley), "None")
test$Alley[is.na(test$Alley)] = "None"

# MasVnrType: Masonry veneer type
# BrkCmn	Brick Common
# BrkFace	Brick Face
# CBlock	Cinder Block
# None	None
# Stone	Stone
#
# This is actually missing. With such a low percentage missing rows
# can be dropped with minimal consequence. Maybe we can interpolate.

# MasVnrArea: Masonry veneer area in square feet
#
# Same missing value percentage as the previous variable. Maybe they are linked.
row.names(test[is.na(test$MasVnrArea),])==row.names(test[is.na(test$MasVnrArea),])
# Yes they are the same rows. Maybe there's a systematic error?
# We will look into interpolation later

# BsmtQual: Evaluates the height of the basement
# 
# Ex	Excellent (100+ inches)	
# Gd	Good (90-99 inches)
# TA	Typical (80-89 inches)
# Fa	Fair (70-79 inches)
# Po	Poor (<70 inches
# NA	No Basement
#
# This is not missing. Set new level as None.
levels(test$BsmtQual) = c(levels(test$BsmtQual), "None")
test$BsmtQual[is.na(test$BsmtQual)] = "None"

# Just repeat for these following variables
# BsmtExposure: Refers to walkout or garden level walls
# 
# Gd	Good Exposure
# Av	Average Exposure (split levels or foyers typically score average or above)	
# Mn	Mimimum Exposure
# No	No Exposure
# NA	No Basement
levels(test$BsmtExposure) = c(levels(test$BsmtExposure), "None")
test$BsmtExposure[is.na(test$BsmtExposure)] = "None"

# BsmtCond: Evaluates the general condition of the basement
# 
# Ex	Excellent
# Gd	Good
# TA	Typical - slight dampness allowed
# Fa	Fair - dampness or some cracking or settling
# Po	Poor - Severe cracking, settling, or wetness
# NA	No Basement
levels(test$BsmtCond) = c(levels(test$BsmtCond), "None")
test$BsmtCond[is.na(test$BsmtCond)] = "None"

# BsmtFinType1: Rating of basement finished area
# 
# GLQ	Good Living Quarters
# ALQ	Average Living Quarters
# BLQ	Below Average Living Quarters	
# Rec	Average Rec Room
# LwQ	Low Quality
# Unf	Unfinshed
# NA	No Basement
# 
levels(test$BsmtFinType1) = c(levels(test$BsmtFinType1), "None")
test$BsmtFinType1[is.na(test$BsmtFinType1)] = "None"

# BsmtFinType2: Rating of basement finished area (if multiple types)
# 
# GLQ	Good Living Quarters
# ALQ	Average Living Quarters
# BLQ	Below Average Living Quarters	
# Rec	Average Rec Room
# LwQ	Low Quality
# Unf	Unfinshed
# NA	No Basement
levels(test$BsmtFinType2) = c(levels(test$BsmtFinType2), "None")
test$BsmtFinType2[is.na(test$BsmtFinType2)] = "None"

# Electrical: Electrical system
# 
# SBrkr	Standard Circuit Breakers & Romex
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
# FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix	  Mixed
#
# Only one data point is missing. This is probably an error and is droppable.
test[is.na(test$Electrical),]

# FireplaceQu: Fireplace quality
# 
# Ex	Excellent - Exceptional Masonry Fireplace
# Gd	Good - Masonry Fireplace in main level
# TA	Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
# Fa	Fair - Prefabricated Fireplace in basement
# Po	Poor - Ben Franklin Stove
# NA	No Fireplace
levels(test$FireplaceQu) = c(levels(test$FireplaceQu), "None")
test$FireplaceQu[is.na(test$FireplaceQu)] = "None"

# GarageType: Garage location
# 
# 2Types	More than one type of garage
# Attchd	Attached to home
# Basment	Basement Garage
# BuiltIn	Built-In (Garage part of house - typically has room above garage)
# CarPort	Car Port
# Detchd	Detached from home
# NA	No Garage
levels(test$GarageType) = c(levels(test$GarageType), "None")
test$GarageType[is.na(test$GarageType)] = "None"

# GarageYrBlt: Year garage was built
# 
# This correlates with the "No" basements section. There are a number of ways to
# tackle this problem. It will be set to 0, temporarily
test$GarageYrBlt[is.na(test$GarageYrBlt)] = 0 

# GarageFinish: Interior finish of the garage
# 
# Fin	Finished
# RFn	Rough Finished	
# Unf	Unfinished
# NA	No Garage
levels(test$GarageFinish) = c(levels(test$GarageFinish), "None")
test$GarageFinish[is.na(test$GarageFinish)] = "None"

# GarageQual: Garage quality
# 
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage
# 
levels(test$GarageQual) = c(levels(test$GarageQual), "None")
test$GarageQual[is.na(test$GarageQual)] = "None"

# GarageCond: Garage condition
# 
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage
levels(test$GarageCond) = c(levels(test$GarageCond), "None")
test$GarageCond[is.na(test$GarageCond)] = "None"

# PoolQC: Pool quality
# 
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# NA	No Pool
levels(test$PoolQC) = c(levels(test$PoolQC), "None")
test$PoolQC[is.na(test$PoolQC)] = "None"

# Fence: Fence quality
# 
# GdPrv	Good Privacy
# MnPrv	Minimum Privacy
# GdWo	Good Wood
# MnWw	Minimum Wood/Wire
# NA	No Fence
levels(test$Fence) = c(levels(test$Fence), "None")
test$Fence[is.na(test$Fence)] = "None"

# MiscFeature: Miscellaneous feature not covered in other categories
# 
# Elev	Elevator
# Gar2	2nd Garage (if not described in garage section)
# Othr	Other
# Shed	Shed (over 100 SF)
# TenC	Tennis Court
# NA	None
levels(test$MiscFeature) = c(levels(test$MiscFeature), "None")
test$MiscFeature[is.na(test$MiscFeature)] = "None"

levels(test$ExterCond) = c(levels(test$ExterCond), "None")
test$ExterCond[is.na(test$ExterCond)] = "None"
# Now we check for levels, because some entries may not have been assumed
# to be factors. For example the first case with MSSubClass the diferent
# numbers actually correspond to different classes. We have to manually set
# these as levels instead.
lv = sapply(test, function(x) levels(x))
test$MSSubClass = factor(test$MSSubClass)

# Also we should try to make factors into numerical values whenever
# they make sense. (i.e. Low, Medium, High become 1, 2, and 3).
test$LotShape = as.character(test$LotShape)
test$LotShape[test$LotShape=="Reg"] = 0
test$LotShape[test$LotShape=="IR1"] = 1
test$LotShape[test$LotShape=="IR2"] = 2
test$LotShape[test$LotShape=="IR3"] = 3
test$LotShape = as.numeric(test$LotShape)

test$Utilities = as.character(test$Utilities)
test$Utilities[test$Utilities=="ELO"] = 0
test$Utilities[test$Utilities=="NoSeWa"] = 1
test$Utilities[test$Utilities=="NoSewr"] = 2
test$Utilities[test$Utilities=="AllPub"] = 3
test$Utilities = as.numeric(test$Utilities)

train$LandSlope = as.character(train$LandSlope)
train$LandSlope[train$LandSlope=="Gtl"] = 0
train$LandSlope[train$LandSlope=="Mod"] = 1
train$LandSlope[train$LandSlope=="Sev"] = 2
train$LandSlope = as.numeric(train$LandSlope)

test$ExterQual = as.character(test$ExterQual)
test$ExterQual[test$ExterQual=="Ex"] = 4
test$ExterQual[test$ExterQual=="Gd"] = 3
test$ExterQual[test$ExterQual=="TA"] = 2
test$ExterQual[test$ExterQual=="Fa"] = 1
test$ExterQual[test$ExterQual=="Po"] = 0
test$ExterQual = as.numeric(test$ExterQual)

test$ExterCond = as.character(train$ExterCond)
test$ExterCond[train$ExterCond=="Ex"] = 4
test$ExterCond[test$ExterCond=="Gd"] = 3
test$ExterCond[test$ExterCond=="TA"] = 2
test$ExterCond[test$ExterCond=="Fa"] = 1
test$ExterCond[test$ExterCond=="Po"] = 0
test$ExterCond = as.numeric(test$ExterCond)

test$BsmtQual = as.character(test$BsmtQual)
test$BsmtQual[test$BsmtQual=="Ex"] = 5
test$BsmtQual[test$BsmtQual=="Gd"] = 4
test$BsmtQual[test$BsmtQual=="TA"] = 3
test$BsmtQual[test$BsmtQual=="Fa"] = 2
test$BsmtQual[test$BsmtQual=="Po"] = 1
test$BsmtQual[test$BsmtQual=="None"] = 0
test$BsmtQual = as.numeric(test$BsmtQual)

test$BsmtCond = as.character(train$BsmtCond)
test$BsmtCond[test$BsmtCond=="Ex"] = 5
test$BsmtCond[test$BsmtCond=="Gd"] = 4
test$BsmtCond[test$BsmtCond=="TA"] = 3
test$BsmtCond[test$BsmtCond=="Fa"] = 2
test$BsmtCond[test$BsmtCond=="Po"] = 1
test$BsmtCond[test$BsmtCond=="None"] = 0
test$BsmtCond = as.numeric(test$BsmtCond)

test$BsmtExposure = as.character(test$BsmtExposure)
test$BsmtExposure[test$BsmtExposure=="Gd"] = 4
test$BsmtExposure[test$BsmtExposure=="Av"] = 3
test$BsmtExposure[test$BsmtExposure=="Mn"] = 2
test$BsmtExposure[test$BsmtExposure=="No"] = 1
test$BsmtExposure[test$BsmtExposure=="None"] = 0
test$BsmtExposure = as.numeric(test$BsmtExposure)

test$BsmtFinType1 = as.character(test$BsmtFinType1)
test$BsmtFinType1[test$BsmtFinType1=="GLQ"] = 6
test$BsmtFinType1[test$BsmtFinType1=="ALQ"] = 5
test$BsmtFinType1[test$BsmtFinType1=="BLQ"] = 4
test$BsmtFinType1[test$BsmtFinType1=="Rec"] = 3
test$BsmtFinType1[test$BsmtFinType1=="LwQ"] = 2
test$BsmtFinType1[test$BsmtFinType1=="Unf"] = 1
test$BsmtFinType1[test$BsmtFinType1=="None"] = 0
test$BsmtFinType1 = as.numeric(test$BsmtFinType1)

test$BsmtFinType2 = as.character(test$BsmtFinType2)
test$BsmtFinType2[test$BsmtFinType2=="GLQ"] = 6
test$BsmtFinType2[test$BsmtFinType2=="ALQ"] = 5
test$BsmtFinType2[test$BsmtFinType2=="BLQ"] = 4
test$BsmtFinType2[test$BsmtFinType2=="Rec"] = 3
test$BsmtFinType2[test$BsmtFinType2=="LwQ"] = 2
test$BsmtFinType2[test$BsmtFinType2=="Unf"] = 1
test$BsmtFinType2[test$BsmtFinType2=="None"] = 0
test$BsmtFinType2 = as.numeric(test$BsmtFinType2)

test$HeatingQC = as.character(test$HeatingQC)
test$HeatingQC[test$HeatingQC=="Ex"] = 5
test$HeatingQC[test$HeatingQC=="Gd"] = 4
test$HeatingQC[test$HeatingQC=="TA"] = 3
test$HeatingQC[test$HeatingQC=="Fa"] = 2
test$HeatingQC[test$HeatingQC=="Po"] = 1
test$HeatingQC[test$HeatingQC=="None"] = 0
test$HeatingQC = as.numeric(test$HeatingQC)

test$CentralAir = as.character(test$CentralAir)
test$CentralAir[test$CentralAir=="N"] = 0
test$CentralAir[test$CentralAir=="Y"] = 1
test$CentralAir = as.numeric(test$CentralAir)

test$Electrical = as.character(test$Electrical)
test$Electrical[test$Electrical=="SBrkr"] = 4
test$Electrical[test$Electrical=="FuseA"] = 3
test$Electrical[test$Electrical=="FuseF"] = 2
test$Electrical[test$Electrical=="FuseP"] = 1
test$Electrical[test$Electrical=="Mix"] = 0
test$Electrical = as.numeric(test$Electrical)

test$KitchenQual = as.character(test$KitchenQual)
test$KitchenQual[test$KitchenQual=="Ex"] = 5
test$KitchenQual[test$KitchenQual=="Gd"] = 4
test$KitchenQual[test$KitchenQual=="TA"] = 3
test$KitchenQual[test$KitchenQual=="Fa"] = 2
test$KitchenQual[test$KitchenQual=="Po"] = 1
test$KitchenQual[test$KitchenQual=="None"] = 0
test$KitchenQual = as.numeric(test$KitchenQual)

test$Functional = as.character(test$Functional)
test$Functional[test$Functional=="Typ"] = 7
test$Functional[test$Functional=="Min1"] = 6
test$Functional[test$Functional=="Min2"] = 5
test$Functional[test$Functional=="Mod"] = 4
test$Functional[test$Functional=="Maj1"] = 3
test$Functional[test$Functional=="Maj2"] = 2
test$Functional[test$Functional=="Sev"] = 1
test$Functional[test$Functional=="Sal"] = 0
test$Functional = as.numeric(test$Functional)

test$FireplaceQu = as.character(test$FireplaceQu)
test$FireplaceQu[test$FireplaceQu=="Ex"] = 5
test$FireplaceQu[test$FireplaceQu=="Gd"] = 4
test$FireplaceQu[test$FireplaceQu=="TA"] = 3
test$FireplaceQu[test$FireplaceQu=="Fa"] = 2
test$FireplaceQu[test$FireplaceQu=="Po"] = 1
test$FireplaceQu[test$FireplaceQu=="None"] = 0
test$FireplaceQu = as.numeric(test$FireplaceQu)

test$GarageFinish = as.character(test$GarageFinish)
test$GarageFinish[test$GarageFinish=="Fin"] = 3
test$GarageFinish[test$GarageFinish=="RFn"] = 2
test$GarageFinish[test$GarageFinish=="Unf"] = 1
test$GarageFinish[test$GarageFinish=="None"] = 0
test$GarageFinish = as.numeric(test$GarageFinish)

test$GarageQual = as.character(test$GarageQual)
test$GarageQual[test$GarageQual=="Ex"] = 5
test$GarageQual[test$GarageQual=="Gd"] = 4
test$GarageQual[test$GarageQual=="TA"] = 3
test$GarageQual[test$GarageQual=="Fa"] = 2
test$GarageQual[test$GarageQual=="Po"] = 1
test$GarageQual[test$GarageQual=="None"] = 0
test$GarageQual = as.numeric(test$GarageQual)

test$GarageCond = as.character(test$GarageCond)
test$GarageCond[test$GarageCond=="Ex"] = 5
test$GarageCond[test$GarageCond=="Gd"] = 4
test$GarageCond[test$GarageCond=="TA"] = 3
test$GarageCond[test$GarageCond=="Fa"] = 2
test$GarageCond[test$GarageCond=="Po"] = 1
test$GarageCond[test$GarageCond=="None"] = 0
test$GarageCond = as.numeric(test$GarageCond)

test$PavedDrive = as.character(test$PavedDrive)
test$PavedDrive[test$PavedDrive=="Y"] = 2
test$PavedDrive[test$PavedDrive=="P"] = 1
test$PavedDrive[test$PavedDrive=="N"] = 0
test$PavedDrive = as.numeric(test$PavedDrive)

test$PoolQC = as.character(test$PoolQC)
test$PoolQC[test$PoolQC=="Ex"] = 5
test$PoolQC[test$PoolQC=="Gd"] = 4
test$PoolQC[test$PoolQC=="TA"] = 3
test$PoolQC[test$PoolQC=="Fa"] = 2
test$PoolQC[test$PoolQC=="Po"] = 1
test$PoolQC[test$PoolQC=="None"] = 0
test$PoolQC = as.numeric(test$PoolQC)

test$Fence = as.character(test$Fence)
test$Fence[test$Fence=="GdPrv"] = 4
test$Fence[test$Fence=="MnPrv"] = 3
test$Fence[test$Fence=="GdWo"] = 2
test$Fence[test$Fence=="MnWw"] = 1
test$Fence[test$Fence=="None"] = 0
test$Fence = as.numeric(test$Fence)

# Now let's check which values are missing
miss.2 = sapply(test, function(x) length(which(is.na(x))))
miss.2[miss.2>0]

# Let's drop the rows where there are missing values.
# The alternative would be to use interpolation but the overall loss would
# be minimal
test.new = na.omit(test)
# Double check for missing values
miss.new = sapply(test.new, function(x) length(which(is.na(x))))
miss.new

# Issues: The one issue still remaining is setting the year for garage built to 0
# This may pose problem for linear models. It may be idea to use the mean instead
# for those models as it won't skew the data. For a robust dataset (one that
# can work for most models) we would need to just drop those rows where
# there are missing values---however, this also could bias the data as then
# we are only looking at homes with garages.
# Another problem is that grades given in categories like OverallQual are
# assumed to be linear improvements (score from 4 to 5 is the same as 8 to 9).
# This may not be the case as people can be biased in their subjective
# assessments. However, if this is an aggregate score from a survey this bias
# can be mitigated. This is also the case with number of bathrooms and bedrooms.
hist(test.new$OverallQual)

# Make lots of plots
# for (i in names(test.new)){plot(test.new[[i]], test.new$SalePrice, xlab=i)}
# 
# # Comments:
# # For MSZoning, there are several outliers for the factor level RL.
# # For Pave, there are several outlier for the factor level Pave.
# # For Alley, there are several outliers for the factor level None.
# # Etc. In fact we see that there are several outliers at high SalesPrice values
# # This is indicitive that a transformation has to be applied to the
# # raw data before analysis! Let's try a logscale on the SalesPrice
# par(mfrow=c(2,1))
# hist(test.new$SalePrice)
# hist(log(test.new$SalePrice))
# par(mfrow=c(1,1))
# # Much more evenly distributed!
# test.new$SalePrice = log(test.new$SalePrice)
# # Rerun plots
# for (i in names(test.new)){plot(test.new[[i]], test.new$SalePrice, xlab=i)}
# 
# # # Drop ID variable since it's just for notation/organization not a predictor
# test.new$Id = NULL
# 
# # Finally let's see how many categorical variables we have
# cat.count = 0
# total.count = 0
# for (i in names(test.new)){
#   total.count = total.count + 1
#   if (is.factor(test.new[[i]])){
#     cat.count = cat.count + 1
#     print(i)
#     plot(test.new[[i]],test.new$SalePrice)
#   }
# }
# cat.count
# total.count

# Outliers
# Outliers will be handled differently depending on each model used.

# The data is now cleaned up and ready to be used in analysis
write.csv(test, file="test_cleaned.csv", row.names=FALSE)

# Try dummy variables for columns where this is no particular order
# for the categorical variables

# Go through all of the variables and remove the ones where we already
# refactored them
name.1 = names(Filter(is.factor, test.new))
dum.1 = dummy.data.frame(test.new, names=name.1)
write.csv(test, file="test_cleaned_dummy.csv", row.names=FALSE)
