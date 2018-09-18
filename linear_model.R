# Linear Regression

##########
#
# For the purposes of running the other R files run Step 1 an Step 2.2
#
##########

##########
# Step 1 #
##########
# Run explore.R

install.packages(c("caret","fmsb","DAAG"))
library(caret)
library(fmsb)
library(DAAG)

wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

training = cbind(train.out,train.Y)

##########
# Step 2 #
##########
# Predictor selection (full one hot encoded data)
# We could use the variables selected through random forests or...

# Fit the model
full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)

# Variable selection using BIC
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(cutoff))
bo$anova

#                      Step Df     Deviance Resid. Df Resid. Dev       AIC
# 1                         NA           NA      1456  232.64943 -2665.731
# 2               + TotalSF -1 170.25433271      1455   62.39509 -4575.926
# 3             + YearBuilt -1  15.43292515      1454   46.96217 -4982.640
# 4          + YearRemodAdd -1   4.85359196      1453   42.10858 -5134.301
# 5            + Fireplaces -1   3.91807207      1452   38.19051 -5269.315

# Variable selection using AIC
bo.2 = step(null, scope=list(lower=null, upper=full), direction="both")
bo.2$anova

#                       Step Df     Deviance Resid. Df Resid. Dev       AIC
# 1                          NA           NA      1456  232.64943 -2671.015
# 2                + TotalSF -1 1.702543e+02      1455   62.39509 -4586.494
# 3              + YearBuilt -1 1.543293e+01      1454   46.96217 -4998.493
# 4           + YearRemodAdd -1 4.853592e+00      1453   42.10858 -5155.438
# 5             + Fireplaces -1 3.918072e+00      1452   38.19051 -5295.735


# This generates the solution based on a model
make_model = function(model, filename) {
  test.model = model
  print("The VIF is:")
  VIF(test.model)
  
  cv = cv.lm(data=training, form.lm=formula(test.model), m=10)

  pred = predict(test.model,test.out)
  pred.trans = exp(pred)
  
  ids = ((1461):2919)
  
  pl = data.frame(ids, pred.trans)
  colnames(pl) = c("Id", "SalePrice")
  
  setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
  write.csv(pl, file=filename, row.names=FALSE)
  setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))
}

# MLR with all variables as included by BIC
make_model(bo, "multiplelinearreg_01.csv")
VIF(bo)

# MLR with all variables as included by AIC
make_model(bo, "multiplelinearreg_02.csv")
VIF(bo.2)

# All
mod=lm(SalePrice~.,data=training)
make_model(mod, "multiplelinearreg_03.csv")
VIF(mod)

# All of the above are seriously overfit. Let's try doing some analysis with only a few variables
mod.2=lm(SalePrice~TotalSF+GrLivArea+YearBuilt+LotArea+BsmtFinSF1+
           YearRemodAdd+ExterQualTA+X2ndFlrSF,data=training)
VIF(mod.2)
make_model(mod, "multiplelinearreg_04.csv")

############
# Step 2.2 #
############
# Predictor selection (for partially one hot encoded data)

training = cbind(train.out.2,train.Y)
# I ran into a random error I can't solve where OverallQual has odd factors
# temporarily I will change it to numeric, but it shouldn't be too much of
# a problem since it is still strongly linearly related
training$OverallQual = as.numeric(training$OverallQual)
test.out.2$OverallQual = as.numeric(test.out.2$OverallQual)
training$OverallCond = as.numeric(training$OverallCond)
test.out.2$OverallCond = as.numeric(test.out.2$OverallCond)
training$Functional = NULL
test.out.2$Functional = NULL
training$Id = NULL
test.out.2$Id = NULL

############
# Step 2.5 #
############

full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(cutoff))
bo$anova

#                     Step Df Deviance Resid. Df Resid. Dev   AIC
# 1                        NA       NA      1456      232.6 -2666
# 2              + TotalSF -1 170.2543      1455       62.4 -4576
# 3          + OverallQual -1  21.8852      1454       40.5 -5198
# 4         + YearRemodAdd -1   4.5177      1453       36.0 -5363
# 5            + BsmtUnfSF -1   4.0894      1452       31.9 -5531
# 6              + LotArea -1   2.5591      1451       29.3 -5646

make_model_2 = function(model, filename) {
  test.model = model
  cv = cv.lm(data=training, form.lm=formula(test.model), m=10)
  pred = predict(test.model,test.out.2)
  pred.trans = exp(pred)
  ids = ((1461):2919)
  pl = data.frame(ids, pred.trans)
  colnames(pl) = c("Id", "SalePrice")
  setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
  write.csv(pl, file=filename, row.names=FALSE)
  setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))
}

# MLR with all variables as included by BIC
make_model_2(bo, "multiplelinearreg_05.csv")

# This is the best score so far with a kaggle metric of 0.12564

# Try AIC?
bo.2 = step(null, scope=list(lower=null, upper=full), direction="both")
bo.2$anova

#                       Step Df Deviance Resid. Df Resid. Dev   AIC
# 1                          NA       NA      1456      232.6 -2671
# 2                + TotalSF -1 1.70e+02      1455       62.4 -4586
# 3            + OverallQual -1 2.19e+01      1454       40.5 -5214
# 4           + YearRemodAdd -1 4.52e+00      1453       36.0 -5384
# 5              + BsmtUnfSF -1 4.09e+00      1452       31.9 -5558
# 6                + LotArea -1 2.56e+00      1451       29.3 -5678

make_model_2(bo, "multiplelinearreg_06.csv")
# Same score as before. I'm satisfied with this for now.