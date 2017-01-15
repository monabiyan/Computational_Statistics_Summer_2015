v1=c(2:6)
v2=c(5:9)
m1=matrix(1:25,nrow=5)
getwd()
setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Statistics/HW1")
getwd()
df1<-data.frame(c("2015-05-19","2015-05-19","2015-05-19","2015-05-19","2015-05-19"),c("Mason","Kati","Sara","David","Jack"),c(80,90,95,100,50))
colnames(df1)<-c("Submission_Date","Name","Grade")
df1$Submission_Date<-as.Date(df1$Submission_Date)
df1$Name<-as.character(df1$Name)
df1$Grade=as.numeric(df1$Grade)
str(df1)
write.table(df1,file="df1.csv",row.names=FALSE,sep=",")
df1 <- read.table(file="df1.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
str(df1)
df2=df1[c(1,3,5),c(1,2)]
df2
df1[df1[1:5,3]%%2==0,3]=0
df1
list1=list(v1,v2,m1,df1)
names(list1) <- c("vector1","vector2","matrix1","data_frame")
str(list1)
list1$matrix1[2,1]
