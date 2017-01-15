install.packages("e1071")
library(e1071)
svmfit <- svm(y~., data=simdat, cost=10, kernel="linear")

#This shows the cutting line and which regions are assigned to Y=1 or Y=0.
#It also shows the data points and the "support vectors" (the x's) which are those data points close to the dividing line 
#that affect its position (the ones far from the line don't affect its position,
#because the dividing line algorithm only cares about maximizing the distance to the closest points).
#The red x's are those that have been mis-classified (either 1's in the 0 zone, or 0's in the 1 zone.)


costvalues <- 10^seq(-3,2,1)
tuned.svm <- tune(svm, y~., data=simdat, ranges=list(cost=costvalues), kernel="linear")
summary(tuned.svm)

y <- ifelse(Hitters$Salary > 500,1,0)
summary(y)


y <- ifelse(Hitters$Salary > 500,1,0)
summary(y)



dat <- data.frame(x=x, y=as.factor(y))
tuned.svm <- tune(svm,y~., data=dat, ranges=list(cost=costvalues), kernel="linear")
summary(tuned.svm)


