wd = "F:/2018 Fall/SYS 6018/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

# install.packages("caret")
library(caret)

##########
# Step 1 #
##########
# Run explore.R

##########
# Step 2 #
##########
# Some data transformations

# Get all X values from both datasets
all.X = rbind(train.out,test.out)

# Find all the categorical variables which are not ordered
names.1 = names(Filter(is.ordered, all.X))
names.2 = names(Filter(is.factor, all.X))
names=names.2[!(names.2 %in% names.1)]

dummy = dummy.data.frame(all.X, names=names)
dim(dummy)

# Complete the dataset
train.X = dummy[1:1460,]
test.X = dummy[1461:2919,]
training = cbind(train.X,train.Y)

##########
# Step 3 #
##########
# Parameter selection

# Fit the model
full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)

# Variable selection using BIC
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(1460))
bo$anova

# First 20 variables with dummy encoding

#                     Step Df     Deviance Resid. Df Resid. Dev       AIC
# 1                        NA           NA      1459  232.80066 -2673.287
# 2          + OverallQual -1 155.46203888      1458   77.33862 -4274.905
# 3            + GrLivArea -1  16.80406338      1457   60.53456 -4625.288
# 4            + YearBuilt -1   9.95756590      1456   50.57699 -4880.390
# 5          + OverallCond -1   4.89830356      1455   45.67869 -5021.826
# 6           + GarageCars -1   4.15828976      1454   41.52040 -5153.893
# 7          + TotalBsmtSF -1   3.46306030      1453   38.05734 -5273.759
# 8      + RoofMatlClyTile -1   5.84876070      1452   32.20858 -5510.091
# 9            + BsmtUnfSF -1   2.19499050      1451   30.01359 -5605.855
# 10      + Condition2PosN -1   1.74877622      1450   28.26481 -5686.216
# 11          + MSZoningRM -1   1.38651331      1449   26.87830 -5752.366
# 12   + `MSZoningC (all)` -1   1.24080380      1448   25.63749 -5814.084
# 13         + SaleTypeNew -1   0.89708395      1447   24.74041 -5858.800
# 14          + Fireplaces -1   0.81442860      1446   23.92598 -5900.384
# 15     + FoundationPConc -1   0.79749015      1445   23.12849 -5942.592
# 16             + LotArea -1   0.65660320      1444   22.47189 -5977.354
# 17 + SaleConditionNormal -1   0.56596983      1443   21.90592 -6007.310
# 18 + NeighborhoodCrawfor -1   0.50107386      1442   21.40484 -6033.807
# 19       + MSSubClass160 -1   0.40438079      1441   21.00046 -6054.367
# 20         + KitchenQual -3   0.57777595      1438   20.42269 -6073.240
# 21          + CentralAir -1   0.30472787      1437   20.11796 -6087.903

##
# Test model 3: Linear regression using 6 variables
##

test.model = lm(SalePrice~OverallQual+GrLivArea+YearBuilt+OverallCond+TotalBsmtSF+GarageCars+RoofMatlClyTile, data=training)
formula(test.model)
summary(test.model)

pred = predict(test.model, newdata=test.X, type = "response")
predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(pl, file="third_linear_reg.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

#
# How about not using dummy variables?
#

training = cbind(all.X[1:1460,],train.Y)
full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)

# Variable selection using BIC
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(1460))
bo$anova

# First 20 variables without dummy encoding (from previous calculations)

#               Step  Df    Deviance Resid. Df Resid. Dev       AIC
# 1                   NA          NA      1459  232.80066 -2673.287
# 2    + OverallQual  -1 155.4620389      1458   77.33862 -4274.905
# 3      + GrLivArea  -1  16.8040634      1457   60.53456 -4625.288
# 4   + Neighborhood -24  18.6731706      1433   41.86139 -4988.942
# 5   + BsmtFinType1  -6   4.0332712      1427   37.82811 -5093.139
# 6     + GarageCars  -4   3.4776504      1423   34.35046 -5204.792
# 7    + OverallCond  -1   2.6568250      1422   31.69364 -5315.035
# 8      + YearBuilt  -1   1.5792554      1421   30.11438 -5382.374
# 9       + RoofMatl  -7   2.6808299      1414   27.43355 -5467.495
# 10   + TotalBsmtSF  -1   2.4057646      1413   25.02779 -5594.208
# 11    + Fireplaces  -1   0.8760294      1412   24.15176 -5638.941
# 12      + MSZoning  -4   1.0980843      1408   23.05368 -5677.733
# 13    + Condition2  -7   1.2996454      1401   21.75403 -5711.448
# 14     + BsmtUnfSF  -1   0.7204402      1400   21.03359 -5753.332
# 15  - BsmtFinType1   6   0.1755356      1406   21.20913 -5784.915
# 16 + SaleCondition  -5   0.9323749      1401   20.27675 -5814.121
# 17       + LotArea  -1   0.3605403      1400   19.91621 -5833.029
# 18    + CentralAir  -1   0.3461655      1399   19.57005 -5851.342
# 19    + Functional  -6   0.7876166      1393   18.78243 -5867.599
# 20  + YearRemodAdd  -1   0.3400120      1392   18.44242 -5886.985
# 21   + ScreenPorch  -1   0.2010194      1391   18.24140 -5895.700

###
# Test model 4: Linear regression using 8 terms
###
test.X = all.X[1461:2919,]

test.model = lm(SalePrice~OverallQual+GrLivArea+Neighborhood+BsmtFinType1+GarageCars+OverallCond+YearBuilt+RoofMatl+TotalBsmtSF, data=training)
formula(test.model)
summary(test.model)

pred = predict(test.model, newdata=test.X, type = "response")
predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(pl, file="fourth_linear_reg.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

###
# Test model 5: Linear regression using almost all terms given by stepwise selection
###
# note this is probably a bad idea

test.model = bo
test.model = update(test.model,.~.-Functional)
test.model = update(test.model,.~.-KitchenQual)

formula(test.model)
summary(test.model)

pred = predict(test.model, newdata=test.X, type = "response")
predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(pl, file="fifth_linear_reg.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

# This is actually the best model made so far. This is surprising.

###
# K-Fold Cross Validation (6th model based on the 4th)
###

data_ctrl <- trainControl(method = "cv", number = 10)

test.model = train(
  SalePrice~OverallQual+GrLivArea+Neighborhood+BsmtFinType1+GarageCars+OverallCond+YearBuilt+RoofMatl+TotalBsmtSF,
  data=training,
  trControl = data_ctrl,
  method = "lm",
  na.action = na.pass
)

test.model
test.model$finalModel
# Std deviation for each resampling
sd(test.model$resample$Rsquared)

pred = predict(test.model, newdata=test.X)
predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(pl, file="sixth_linear_reg.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

##
# K-fold Cross Validation (7th model based on the 5th)
##

test.model = train(
  formula(bo),
  data=training,
  trControl = data_ctrl,
  method = "lm",
  na.action = na.pass
)

pred = predict(test.model, newdata=test.X)
predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")

setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
write.csv(pl, file="seventh_linear_reg.csv", row.names=FALSE)
setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

# As expected this model is WAY too overfitted and gave a terrible score!