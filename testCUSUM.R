library(readxl)
library(IQCC)
newdata <- read_excel("TestData.xlsx", col_types = c("skip", 
         "numeric", "numeric", "numeric", "numeric", 
         "numeric"))


MaxCUSUM<-function(mydata,k=0.396,h=4.051) {
n<-ncol(mydata)
t<-nrow(mydata)
mydata$Xbar<-apply(mydata[,1:n],1,mean)
mydata$S<-apply(mydata[,1:n],1,sd)

Mu<-mean(mydata$Xbar)
Sgm<-mean(mydata$S)/c4(n)
mydata$Z<-sqrt(n)*(mydata$Xbar-Mu)/Sgm
mydata$Y<-qnorm(pchisq((n-1)*mydata$S*mydata$S/(Sgm*Sgm),(n-1)))
mydata$Cp<-0
mydata$Cm<-0
mydata$Sp<-0
mydata$Sm<-0

mydata$Cp[1]=max(0,mydata$Z[1]-k) 
mydata$Cm[1]=max(0,-mydata$Z[1]-k) 
mydata$Sp[1]=max(0,mydata$Y[1]-k) 
mydata$Sm[1]=max(0,-mydata$Y[1]-k)
for(i in 2:t) {
  mydata$Cp[i]=max(0,mydata$Z[i]-k+mydata$Cp[i-1]) 
  mydata$Cm[i]=max(0,-mydata$Z[i]-k+mydata$Cm[i-1]) 
  mydata$Sp[i]=max(0,mydata$Y[i]-k+mydata$Sp[i-1]) 
  mydata$Sm[i]=max(0,-mydata$Y[i]-k+mydata$Sm[i-1]) 
}
mydata$M<-apply(mydata[,10:13],1,max)
plot(mydata$M,type="l",main="Max-CUSUM контрольная карта",xlab="Номер выборки",ylab="Max-CUSUM")
abline(h=h,lty="dotdash")
for(i in 1:t) {
  if (mydata$Cp[i]>h && mydata$Sp[i]>h) text(i,mydata$M[i],"B++") else
  if (mydata$Cp[i]>h && mydata$Sm[i]>h) text(i,mydata$M[i],"B+-") else 
  if (mydata$Cm[i]>h && mydata$Sm[i]>h) text(i,mydata$M[i],"B--") else
  if (mydata$Cm[i]>h && mydata$Sp[i]>h) text(i,mydata$M[i],"B-+") else
  if (mydata$Cm[i]>h) text(i,mydata$M[i],"C-") else
  if (mydata$Cp[i]>h) text(i,mydata$M[i],"C+") else 
  if (mydata$Sp[i]>h) text(i,mydata$M[i],"S+") else
  if (mydata$Sm[i]>h) text(i,mydata$M[i],"S-")
}  
}
MaxCUSUM(newdata)
RealData <- read_excel("RealData.xlsx", col_names = FALSE, col_types = c("numeric"))
RealData <-as.data.frame(matrix(log(unlist(RealData[,1])),20,5))
svg('piccusum.svg', width = 11, height = 7,  pointsize = 14)
MaxCUSUM(RealData)
dev.off()
pdf('piccusum.pdf', width = 11, height = 7,  pointsize = 14)
MaxCUSUM(RealData)
dev.off()