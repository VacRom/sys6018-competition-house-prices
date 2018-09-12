wd = "F:/2018 Fall/SYS 6018/assignments/kaggle/02_Housing/"
setwd(paste(wd,"sys6018-competition-house-prices/final_submission_results",sep=""))

reg.1=read.csv("first_linear_reg.csv")
reg.4=read.csv("fourth_linear_reg.csv")
reg.5=read.csv("fifth_linear_reg.csv")

hist(reg.1$SalePrice,xlim=c(0,600000),breaks=20)
hist(reg.4$SalePrice,xlim=c(0,600000),breaks=20)
hist(reg.5$SalePrice,xlim=c(0,600000),breaks=20)

knn.1=read.csv("first_knn.csv")
knn.2=read.csv("second_knn.csv")
knn.3=read.csv("third_knn.csv")

hist(knn.1$SalePrice,xlim=c(0,600000),breaks=20)
hist(knn.3$SalePrice,xlim=c(0,600000),breaks=20)
hist(knn.2$SalePrice,xlim=c(0,600000),breaks=20)


distances = function(train,func,param) {
  if (func=="minkowski") {
    out=apply(train, 1, function(x) median(minkowski_list(x, train, param)))
  } else if (func=="mahalanobis") {
    out=apply(train, 1, function(x) mahal_list(x, train, param))
  } else {
    print("Unknown function")
  }
  return(out)
}

# Euclidean
median.dist=distances(X.standard,"minkowski",2)
plot(median.dist[1:1460], Y)
plot(median.dist[1:1460], exp(Y))

median.dist.5=distances(X.standard,"minkowski",5)
plot(median.dist.5[1:1460], Y)
plot(median.dist.5[1:1460], exp(Y))

median.dist.2=distances(X.standard,"minkowski",10)
plot(median.dist.2[1:1460], Y)
plot(median.dist.2[1:1460], exp(Y))

# Manhattan
median.dist.3=distances(X.standard,"minkowski",1)
plot(median.dist.3[1:1460], Y)
plot(median.dist.3[1:1460], exp(Y))

median.dist.4=distances(X.standard,"minkowski",.5)
plot(median.dist.4[1:1460], Y)
plot(median.dist.4[1:1460], exp(Y))

setwd(paste(wd,"sys6018-competition-house-prices/cleaned_data",sep=""))

index=names(Filter(is.factor,data))
?dummyVars
dummyVars(.~.,data=class)
