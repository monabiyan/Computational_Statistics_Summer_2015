
############################################################

calc_mean<-function(v)
{
    temp=0;
    for (i in 1:length(v))
    {
      temp=temp+v[i];
    }
    ans=temp/length(v);
    return(ans)
    print(ans)
}
calc_mean(c(1,2,3,4))


###############################################################
check_vector<-function(v)
{
  if(length(v)!=4) stop(' The vector must have 4 elements!');
  first_half=v[1]+v[2];
  second_half=v[3]+v[4];
  if (first_half>second_half) 
    { 
      return(v);
      print(v);
    }
  else 
    {
      return(0);
      print(0);
    }
}
check_vector(c(7,2,3,4))

#################################################################

Fibonacci<-function(n)
{
  if (n==1 | n==2) 
    {
      return(1);
    }
  else  
    {
      return(Fibonacci(n-1)+Fibonacci(n-2));
    }
}
Fibonacci_write<-function(n)
{
  fibvec <- numeric(n)
  for (i in 1:n) 
    {
      fibvec[i]<-Fibonacci(i);
    }
  return(fibvec);
  print(fibvec)
}
Fibonacci_write(10)
#################################################
MAT = matrix(1:16,nrow=4,ncol=4);
mean_MAT_row=apply(B,1,calc_mean);
print(mean_MAT_row)
###############################################
ds1<-aggregate(cbind(Wind,Ozone) ~ Month, data=airquality,mean)
########################################################################
authors <- data.frame(surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"), nationality = c("US", "Australia", "US", "UK", "Australia"),stringsAsFactors=FALSE);
books <- data.frame(name = c("Tukey", "Venables", "Tierney","Ripley", "Ripley", "McNeil", "R Core"),title = c("Exploratory Data Analysis","Modern Applied Statistics ...","LISP-STAT","Spatial Statistics", "Stochastic Simulation","Interactive Data Analysis","An Introduction to R"),stringsAsFactors=FALSE);
authors
books
bookmerge <- merge(authors, books, by.x="surname",by.y="name", all.x=TRUE, all.y=TRUE )
###################################################################
gout1 <- gsub("to","2","To be, or not to be -- that is the question: Whether 'tis nobler in the mind to suffer
The slings and arrows of outrageous fortune, Or to take arms against a sea of troubles, 
And by opposing end them. To die -- to sleep -- No more...");

gout2<-gsub("To","2",gout1)
gout2

################################################################

#####################################################
setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Statistics/HW2")
real_state<-read.table("realstate_CA.csv",header=TRUE,sep=",")

stat_class_grades=c(100,80,83,63,65,85,83,85,75,76,76,75,70,80,77,67,90,69,92,78,89,80,89,98,85,86,89,84,56,78,75,86,93,85,86,95,84,77,74,75,73);

hist(stat_class_grades,main="Final Grades of students in Statistics Class 2014",xlab="Grade")
hist(real_state$price,main="RealState price in California",xlab="Price")
plot(real_state$sq__ft,real_state$price,xlab="sq_ft",ylab="Price",main="Price for the sq area of residential area in CA")
boxplot(real_state$price,main="Real State price in California",ylab="Price")


########################  simple ggplot #####################
library(ggplot2)
ggplot(data=real_state,aes(x=price)) + geom_histogram()
ggplot(data=real_state,aes(x=1,y=price)) + geom_boxplot()
ggplot(data=real_state,aes(x=sq__ft,y=price),fill=cond) + geom_point()
ggplot(data=real_state,aes(x=as.factor(beds),y=price)) + geom_boxplot()
ggplot(data=real_state,aes(x=sq__ft,y=price)) +geom_point() + geom_line() + xlab("Area(sq ft)") + ylab("Price")

#########################   Beautiful ggplots  ############################
ggplot(data=real_state,aes(x=price)) + geom_histogram(fill="#FF9999", colour="black")





