train <- read.csv('train_cleaned.csv')
train[sapply(train, is.integer)] <- lapply(train[sapply(train, is.integer)],as.factor)
train$LotArea <- as.numeric(train$LotArea)
train$LotFrontage <- as.numeric(train$LotFrontage)
train$GrLivArea <- as.numeric(train$GrLivArea)
train$X1stFlrSF <- as.numeric(train$X1stFlrSF)
lmMod <- lm(SalePrice ~ LotArea + SaleCondition+ OverallQual + GrLivArea + Neighborhood + X1stFlrSF, data = train)

summary(lmMod)
