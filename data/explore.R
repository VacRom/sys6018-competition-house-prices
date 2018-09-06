# Exploratory analysis on the data

# Set your working directory
wd = "F:/2018 Fall/SYS 6018/assignments/kaggle/02_Housing/data"
setwd(wd)

# It doesn't look clean in R, just open it up in MS Word
desc = read.table("data_description.txt", sep="\n")

# It looks like there are a number of variables (mostly categorical)

train = read.csv("train.csv")
head(train)

# Apply the temporary function across the data
# This function takes each column and checks to see if there are any missing
# values. Then it puts their indexes into a list and outputs the length of
# this list
miss = sapply(train, function(x) length(which(is.na(x))))

# Output the data frame given that there are some missing values
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

# Let's explore these variables:

# LotFrontage: Linear feet of street connected to property
#
# With about 17% loss we should be able to interpolate this, unless it's
# supposed to mean 0. Check to see the distribution
hist(train$LotFrontage)
# These many are missing
sum(is.na(train$LotFrontage))
# It would not be ridiculous to assume that some houses have 0 lot frontage
summary(train$LotFrontage)
# The minimum value is 21, so maybe it is feasible that 0 is the valid answer.
# Maybe try plotting type of lot vs. frontage?
plot(train$LotConfig,train$LotFrontage)
# Maybe try checking which types of lots have the most missing values?
table(train$LotConfig[is.na(train$LotFrontage)])
# It looks like houses on the inside have the most missing values. This is
# because a house with no access to the sidewalk would have no or little
# access to the street. We are then justified with setting the value of NA to 0
train$LotFrontage[is.na(train$LotFrontage)] = 0

# Alley: Type of alley access to property
# Grvl	Gravel
# Pave	Paved
# NA 	No alley access
#
# Factor NA as a third variable, it's not actually missing data
# Set this as "None"
levels(train$Alley) = c(levels(train$Alley), "None")
train$Alley[is.na(train$Alley)] = "None"

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
row.names(train[is.na(train$MasVnrArea),])==row.names(train[is.na(train$MasVnrArea),])
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
levels(train$BsmtQual) = c(levels(train$BsmtQual), "None")
train$BsmtQual[is.na(train$BsmtQual)] = "None"

# Just repeat for these following variables
# BsmtExposure: Refers to walkout or garden level walls
# 
# Gd	Good Exposure
# Av	Average Exposure (split levels or foyers typically score average or above)	
# Mn	Mimimum Exposure
# No	No Exposure
# NA	No Basement
levels(train$BsmtExposure) = c(levels(train$BsmtExposure), "None")
train$BsmtExposure[is.na(train$BsmtExposure)] = "None"

# BsmtCond: Evaluates the general condition of the basement
# 
# Ex	Excellent
# Gd	Good
# TA	Typical - slight dampness allowed
# Fa	Fair - dampness or some cracking or settling
# Po	Poor - Severe cracking, settling, or wetness
# NA	No Basement
levels(train$BsmtCond) = c(levels(train$BsmtCond), "None")
train$BsmtCond[is.na(train$BsmtCond)] = "None"

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
levels(train$BsmtFinType1) = c(levels(train$BsmtFinType1), "None")
train$BsmtFinType1[is.na(train$BsmtFinType1)] = "None"

# BsmtFinType2: Rating of basement finished area (if multiple types)
# 
# GLQ	Good Living Quarters
# ALQ	Average Living Quarters
# BLQ	Below Average Living Quarters	
# Rec	Average Rec Room
# LwQ	Low Quality
# Unf	Unfinshed
# NA	No Basement
levels(train$BsmtFinType2) = c(levels(train$BsmtFinType2), "None")
train$BsmtFinType2[is.na(train$BsmtFinType2)] = "None"

# Electrical: Electrical system
# 
# SBrkr	Standard Circuit Breakers & Romex
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average)	
# FuseF	60 AMP Fuse Box and mostly Romex wiring (Fair)
# FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor)
# Mix	  Mixed
#
# Only one data point is missing. This is probably an error and is droppable.
train[is.na(train$Electrical),]

# FireplaceQu: Fireplace quality
# 
# Ex	Excellent - Exceptional Masonry Fireplace
# Gd	Good - Masonry Fireplace in main level
# TA	Average - Prefabricated Fireplace in main living area or Masonry Fireplace in basement
# Fa	Fair - Prefabricated Fireplace in basement
# Po	Poor - Ben Franklin Stove
# NA	No Fireplace
levels(train$FireplaceQu) = c(levels(train$FireplaceQu), "None")
train$FireplaceQu[is.na(train$FireplaceQu)] = "None"

# GarageType: Garage location
# 
# 2Types	More than one type of garage
# Attchd	Attached to home
# Basment	Basement Garage
# BuiltIn	Built-In (Garage part of house - typically has room above garage)
# CarPort	Car Port
# Detchd	Detached from home
# NA	No Garage
levels(train$GarageType) = c(levels(train$GarageType), "None")
train$GarageType[is.na(train$GarageType)] = "None"


# GarageYrBlt: Year garage was built
# 
# This correlates with the "No" basements section. There are a number of ways to
# tackle this problem. It will be set to 0, temporarily
train$GarageYrBlt[is.na(train$GarageYrBlt)] = 0 

# GarageFinish: Interior finish of the garage
# 
# Fin	Finished
# RFn	Rough Finished	
# Unf	Unfinished
# NA	No Garage
levels(train$GarageFinish) = c(levels(train$GarageFinish), "None")
train$GarageFinish[is.na(train$GarageFinish)] = "None"

# GarageQual: Garage quality
# 
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage
# 
levels(train$GarageQual) = c(levels(train$GarageQual), "None")
train$GarageQual[is.na(train$GarageQual)] = "None"

# GarageCond: Garage condition
# 
# Ex	Excellent
# Gd	Good
# TA	Typical/Average
# Fa	Fair
# Po	Poor
# NA	No Garage
levels(train$GarageCond) = c(levels(train$GarageCond), "None")
train$GarageCond[is.na(train$GarageCond)] = "None"

# PoolQC: Pool quality
# 
# Ex	Excellent
# Gd	Good
# TA	Average/Typical
# Fa	Fair
# NA	No Pool
levels(train$PoolQC) = c(levels(train$PoolQC), "None")
train$PoolQC[is.na(train$PoolQC)] = "None"

# Fence: Fence quality
# 
# GdPrv	Good Privacy
# MnPrv	Minimum Privacy
# GdWo	Good Wood
# MnWw	Minimum Wood/Wire
# NA	No Fence
levels(train$Fence) = c(levels(train$Fence), "None")
train$Fence[is.na(train$Fence)] = "None"

# MiscFeature: Miscellaneous feature not covered in other categories
# 
# Elev	Elevator
# Gar2	2nd Garage (if not described in garage section)
# Othr	Other
# Shed	Shed (over 100 SF)
# TenC	Tennis Court
# NA	None
levels(train$MiscFeature) = c(levels(train$MiscFeature), "None")
train$MiscFeature[is.na(train$MiscFeature)] = "None"

# Now let's check which values are missing
miss.2 = sapply(train, function(x) length(which(is.na(x))))
miss.2[miss.2>0]

# Now we check for levels, because some entries may not have been assumed
# to be factors. For example the first case with MSSubClass the diferent
# numbers actually correspond to different classes. We have to manually set
# these as levels instead.
lv = sapply(train, function(x) levels(x))
train$MsSubClass = factor(train$MsSubClass)

# Let's drop the rows where there are missing values.
# The alternative would be to use interpolation but the overall loss would
# be minimal
train.new = na.omit(train)
# Double check for missing values
miss.new = sapply(train.new, function(x) length(which(is.na(x))))
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
hist(train$OverallQual)
