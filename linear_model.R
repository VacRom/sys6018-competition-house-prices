wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

# install.packages(c("caret","fmsb","DAAG"))
library(caret)
library(fmsb)
library(DAAG)

##########
# Step 1 #
##########
# Run explore.R

##########
# Step 2 #
##########
# Predictor selection
# We could use the variables selected through random forests or...

training = cbind(train.out,train.Y)

# Fit the model
full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)

# Variable selection using BIC
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(cutoff))
bo$anova

#                      Step Df Deviance Resid. Df Resid. Dev   AIC
# 1                         NA       NA      1456      232.6 -2666
# 2               + TotalSF -1 170.2543      1455       62.4 -4576
# 3             + YearBuilt -1  15.4329      1454       47.0 -4983
# 4          + YearRemodAdd -1   4.8536      1453       42.1 -5134
# 5            + Fireplaces -1   3.9181      1452       38.2 -5269
# 6             + BsmtUnfSF -1   2.2300      1451       36.0 -5350
# 7     + `MSZoningC (all)` -1   2.0047      1450       34.0 -5426
# 8         + FunctionalTyp -1   1.9883      1449       32.0 -5507
# 9          + KitchenAbvGr -1   1.3870      1448       30.6 -5564
# 10            + GrLivArea -1   1.5145      1447       29.1 -5631
# 11              + LotArea -1   1.3966      1446       27.7 -5695
# ...
# 54 predictors.

# Variable selection using AIC
bo.2 = step(null, scope=list(lower=null, upper=full), direction="both")
bo.2$anova

#                       Step Df Deviance Resid. Df Resid. Dev   AIC
# 1                          NA       NA      1456      232.6 -2671
# 2                + TotalSF -1 1.70e+02      1455       62.4 -4586
# 3              + YearBuilt -1 1.54e+01      1454       47.0 -4998
# 4           + YearRemodAdd -1 4.85e+00      1453       42.1 -5155
# 5             + Fireplaces -1 3.92e+00      1452       38.2 -5296
# 6              + BsmtUnfSF -1 2.23e+00      1451       36.0 -5381
# 7      + `MSZoningC (all)` -1 2.00e+00      1450       34.0 -5463
# 8          + FunctionalTyp -1 1.99e+00      1449       32.0 -5549
# 9           + KitchenAbvGr -1 1.39e+00      1448       30.6 -5612
# 10             + GrLivArea -1 1.51e+00      1447       29.1 -5684
# 11               + LotArea -1 1.40e+00      1446       27.7 -5753
# ...
# 138 predictors

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
names=predictors(results)[1:20]
mod.2=lm(SalePrice~TotalSF+GrLivArea+YearBuilt+LotArea+BsmtFinSF1+
           YearRemodAdd+ExterQualTA+X2ndFlrSF,data=training)
VIF(mod.2)
make_model(mod, "multiplelinearreg_04.csv")