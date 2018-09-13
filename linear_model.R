wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/02_Housing/"
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
train.X = dummy[1:cutoff,]
test.X = dummy[(cutoff+1):dim(dummy)[1],]
training = cbind(train.X,train.Y)

##########
# Step 3 #
##########
# Predictor selection

# Fit the model
full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)

# Variable selection using BIC
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(cutoff))
bo$anova

# First 10 variables with dummy encoding

#                      Step Df     Deviance Resid. Df Resid. Dev       AIC
# 1                         NA           NA      1448  40.422658 -5179.038
# 2               + TotalSF -1 29.355784881      1447  11.066873 -7048.843
# 3           + OverallQual -9  3.986275193      1438   7.080598 -7630.456
# 4             + YearBuilt -1  0.814606106      1437   6.265992 -7800.276
# 5           + OverallCond -8  1.080451430      1429   5.185540 -8016.289
# 6             + BsmtUnfSF -1  0.470622770      1428   4.714918 -8146.872
# 7               + LotArea -1  0.293541439      1427   4.421376 -8232.736
# 8   + NeighborhoodCrawfor -1  0.216809218      1426   4.204567 -8298.313
# 9     + `MSZoningC (all)` -1  0.139841041      1425   4.064726 -8340.046
# 10      + FoundationPConc -1  0.130498591      1424   3.934227 -8380.051
# 11           + MSZoningRM -1  0.146644989      1423   3.787582 -8427.815



# This generates a pdf
make_model = function(model, filename) {
  test.model = model
  pred = predict(test.model, test.X, typer="response")
  pred.trans = ((pred*lam)+1)**(1/lam)
  ids = 1461:2919
  pl = data.frame(ids, pred.trans)
  colnames(pl) = c("Id", "SalePrice")
  setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))
  write.csv(pl, file=filename, row.names=FALSE)
  setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))
}

###
# Test model, includes the top 4 explanatory variables
###

mod = lm(SalePrice~TotalSF+OverallQual+YearBuilt+OverallCond, data=training)
file = "new_linear_reg_1.csv"
make_model(mod,file)

###
# Includes the 20~ recommended variables
###

mod = bo
temp=update(bo,.~.-Functional)
file = "new_linear_reg_2.csv"
make_model(temp,file)

##
# How about not using dummy variables? w/ all variables
##

training = cbind(all.X[1:cutoff,],train.Y)
full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)

# Variable selection using BIC
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(cutoff))
bo$anova

mod = bo
temp = update(bo,.~.-SaleCondition)
temp = update(temp,.~.-MSZoning)
temp = update(temp,.~.-Foundation)
temp = update(temp,.~.-Functional)

file = "new_linear_reg_3.csv"
make_model(temp,file)

###
# K-Fold Cross Validation on the model new_linear_reg_2.csv
###

data_ctrl = trainControl(method = "cv", number = 10)

# Perform the same steps
training = cbind(train.X,train.Y)
full = lm(SalePrice~., data=training)
null = lm(SalePrice~1, data=training)
bo = step(null, scope=list(lower=null, upper=full), direction="both", k=log(cutoff))
temp = update(bo,.~.-MSZoning)
temp = update(temp,.~.-Functional)

test.model = train(
  formula(temp),
  data=training,
  trControl = data_ctrl,
  method = "lm",
  na.action = na.pass
)

test.model$finalModel
# Std deviation for each resampling
# This is a pretty low SD which means our models are consistent (low variance)
# The problem is that it might be biased!
sd(test.model$resample$Rsquared)

make_model(test.model, "new_linear_reg_2_kfold.csv")

##########
# Step 4 #
##########
# Model analysis. What went right? What went wrong?
# Check for linear relationships
names = test.model$coefnames
colnames = names(train.X) %in% names
sub=train.X[colnames]
sub=cbind(sub,train.Y)
par(mfrow=c(2,2))
for (name in names(sub)) {
  plot(sub[[name]],sub$SalePrice, xlab=name)
}

# All of the data which is not simply 0 or 1 (dummies) actually do look like
# they can be linear relationships.

# We want the covariances between predictors to be low.
cov.mat = abs(cov(sub))<0.70
colSums(cov.mat)
# This full model does not achieve this goal.

install.packages("fmsb")
library(fmsb)

# There is huge multicollinearity! We need to reduce the number of features
# which are related to one another. This value should be well <10
VIF(temp)