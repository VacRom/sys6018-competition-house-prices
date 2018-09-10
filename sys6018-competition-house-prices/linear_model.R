train <- read.csv('train_cleaned_dummy.csv')

# Fit the model
full = lm(SalePrice~., data=train)
null = lm(SalePrice~1, data=train)

# Variable selection
fo = step(null, scope=list(lower=null, upper=full), direction="forward", k=log(dim(train)[1]))
fo$rank

# Other dataset
train.2 = read.csv("train_cleaned.csv")
full = lm(SalePrice~., data=train.2)
null = lm(SalePrice~1, data=train.2)
bo = step(null, scope=list(upper=full), data=train.2, direction="both")
bo.2 = step(null, scope=list(upper=full), data=train.2, direction="both", k=log(1451))

test.model = lm(SalePrice~OverallQual+Neighborhood+GrLivArea, data=train.2)
formula(test.model)
summary(test.model)

test = read.csv("test_cleaned.csv",header=TRUE)

pred = predict(test.model, newdata=test, type = "response")
predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")
write.csv(pl, file="first_linear_reg.csv", row.names=FALSE)

####### Attempt 2
# Change overall quality to factor variable

# Other dataset
train.2 = read.csv("train_cleaned.csv")
train.2$OverallQual = factor(train.2$OverallQual)
full = lm(SalePrice~., data=train.2)
null = lm(SalePrice~1, data=train.2)

test.model = lm(SalePrice~OverallQual+Neighborhood+GrLivArea, data=train.2)
formula(test.model)
summary(test.model)

test = read.csv("test_cleaned.csv",header=TRUE)
test$OverallQual = factor(test$OverallQual)

pred = predict(test.model, newdata=test, type = "response")
predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")
write.csv(pl, file="second_linear_reg.csv", row.names=FALSE)

##### Attempt 3
# ALL OF THEM
# This is a bad idea
test$LandSlope = as.integer(test$LandSlope)
test$ExterCond = as.integer(test$ExterCond)
test$BsmtCond = as.integer(test$BsmtCond)
pred = predict(full, newdata=test, type="response")

predlog = exp(pred)
ids = 1461:2919
pl = data.frame(ids, predlog)
colnames(pl) = c("Id", "SalePrice")
write.csv(pl, file="third_linear_reg.csv", row.names=FALSE)
