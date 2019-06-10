#Reshape data
ID<-c("ID1","ID2","ID3")
PER1<-c(10,20,30)
PER2<-c(8,15,18)
PER3<-c(7,4,16)
PER4<-c(6,2,3)
dat<-data.frame(ID,PER1,PER2,PER3,PER4)

ID<-c((rep("ID1",4)),rep("ID2",4),rep("ID3",4))
TRIAL<-rep(1:4,3)
TIME<-c(10,8,7,6,20,15,4,2,30,18,16,3)

dat.expected<-data.frame(ID,TRIAL,TIME)
#We have this data frame
dat
#And we want to transform in this data frame
dat.expected

library(reshape2)

#We just use melt
dat.transformed<-melt(dat)

#And then we order it to get the shape we want
dat.transformed<-dat.transformed[order(dat.transformed$ID),]
colnames(dat.transformed)<-c("ID","TRIAL","TIME")
dat.transformed
#We replace values with numbers
a<-replace(dat.transformed, c("PER1","PER2","PER3","PER4"), c(1,2,3,4))
a$PER1

dat.transformed$TRIAL<-a$PER1
str(dat.transformed)

dat.transformed



#And now we can do some models and graphs!
plot(subset(dat.transformed,subset = (dat.transformed$ID == "ID1"))$TRIAL,subset(dat.transformed,subset = (dat.transformed$ID == "ID1"))$TIME)


ID1<-lm(TRIAL ~ TIME, data = (subset(dat.transformed,subset = (dat.transformed$ID == "ID1"))))
summary(ID1)
lm(TRIAL ~ TIME, data = (subset(dat.transformed,subset = (dat.transformed$ID == "ID2"))))
lm(TRIAL ~ TIME, data = (subset(dat.transformed,subset = (dat.transformed$ID == "ID3"))))

   