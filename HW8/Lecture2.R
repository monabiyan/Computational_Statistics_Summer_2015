anes_2008tr <- read.table("C:/Users/monabiyan/SkyDrive/Summer 2015/Statistics/HW9/anes_2008tr.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
mr2 <- lm(ideology_con ~ age + gender_male + race_white + 
            education + income,data=anes_2008tr)
summary(mr2)

xmat <- as.matrix(cbind(anes_2008tr$age,anes_2008tr$gender_male,anes_2008tr$race_white,anes_2008tr$education,anes_2008tr$income))
xmat <- cbind(1,xmat) # add the columns of 1's
head(xmat)


#now we solve for Beta in one step: (X'X)^-1 X'Y :
solve( t(xmat) %*% xmat )   %*%   t(xmat) %*% anes_2008tr$ideology_con



ypred <- predict(mr2)
# and the rest of it is done as we have done before:
y <- anes_2008tr$ideology_con
tss <- sum((y - mean(y))^2)
sse <- sum((y-ypred)^2)
r2 <- (tss-sse)/tss
r2



n <- length(y)
k <- ncol(xmat)-1
dft <- n - 1
dfe <- n - k - 1
(tss/dft - sse/dfe)/ (tss/dft)


f <- (r2/k) / ((1-r2)/(n-k-1))
f

pf(f,k,(n-k-1),lower.tail=F)
