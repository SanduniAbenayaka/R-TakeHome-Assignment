#1
library(MASS)
attach(birthwt)
View(birthwt)

#3
pie(table(race),main = "Pie Chart for race")
pie(table(smoke),main = "Pie Chart for smoke")
pie(table(low),main = "Pie Chart for low")

#4
hist(lwt,prob=T)
curve(dnorm(x, mean=mean(birthwt$lwt), sd=sd(birthwt$lwt)), col="red", add=TRUE)

hist(age,prob=T)
curve(dnorm(x, mean=mean(birthwt$age), sd=sd(birthwt$age)), col="red", add=TRUE) 
 
hist(bwt,prob=T)
curve(dnorm(x, mean=mean(birthwt$bwt), sd=sd(birthwt$bwt)), col="red", add=TRUE) 

hist(ptl,prob=T)
curve(dnorm(x, mean=mean(birthwt$ptl), sd=sd(birthwt$ptl)), col="red", add=TRUE) 

hist(ftv,prob=T)
curve(dnorm(x, mean=mean(birthwt$ftv), sd=sd(birthwt$ftv)), col="red", add=TRUE) 

#5
summary(age)
summary(lwt)
summary(bwt)

#6
plot(age~bwt)

#7
set.seed(17001031)

#8
birthwt_df<-data.frame(birthwt)
smoking<-subset.data.frame(birthwt_df,smoke==1)
no_smoking<-subset.data.frame(birthwt_df,smoke==0)
r1<- smoking[sample(nrow(smoking), 25),]
r2<- no_smoking[sample(nrow(no_smoking), 25),]
new<-rbind.data.frame(r1,r2)

#9
new_m1<-mean(smoking$bwt)
new_sd1<-sd(smoking$bwt)
new_m1
new_sd1

new_m2<-mean(no_smoking$bwt)
new_sd2<-sd(no_smoking$bwt)
new_m2
new_sd2

#10
n1<-nrow(smoking)
n2<-nrow(no_smoking)
sp<-sqrt(((n1-1)*(new_sd1)**2)+((n2-1)*(new_sd2)**2)/(n1+n2-2))
t<-(new_m1-new_m2)/(sp*sqrt((1/n1)+(1/n2)))
pval <- pt(t,df=n1+n2-2)
pval

#using function
s1<-bwt[smoke==0]
s2<-bwt[smoke==1]
t.test(s1,s2,var.equal = T)

#11
fit<- lm(bwt~lwt)
fit
summary(fit)
