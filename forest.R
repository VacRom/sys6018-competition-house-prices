wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))

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