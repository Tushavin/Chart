# Рисунки к статье о контрольных картах
require(grDevices)
library(qcc)
library(readr)
require(lubridate)
data <- read_csv("data.csv", col_types = cols(actualstart = col_datetime(format = "%Y-%m-%d %H:%M:%S"), actualfinish = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
data<-data[order(data$actualstart),]
data$badtime<-data$actualfinish-data$actualstart
data$year<-year(data$actualstart)
data$month<-month(data$actualstart)
data$diff<-0
data$diff[-1]<- diff(data$actualstart)
uchart<-aggregate(badtime~month+year,data=data,sum)
uchart<-uchart[1:13,]
uchart<-rbind(uchart,data.frame(month=c(1,5,8,11,12),year=2018,badtime=0))
uchart<-rbind(uchart,data.frame(month=c(1:5,12),year=2019,badtime=0))
uchart$total<-days_in_month(uchart$month)*24*60
uchart<-uchart[order(uchart$year,uchart$month),]
svg('pic1.svg', width = 11, height = 7,  pointsize = 14)
old <- qcc.options() 
qcc.options(bg.margin="white")
qcc(uchart$badtime,sizes=uchart$total,type="u",title="U-chart")
dev.off()
svg('pic2.svg', width = 11, height = 7,  pointsize = 14)
qcc(data$diff[25:37], type="g",title="G-chart 2019")
dev.off()
#svg('pic3.svg', width = 11, height = 7,  pointsize = 14)
#qcc(data2[32:40,1], type="g",title="G-chart 201",limits= c(16.58457, 2416.421))
#dev.off()
qcc.options(old)
library(ADGofTest)
ad.test(data$diff[25:37],pgeom,prob=1/mean(data$diff[25:37]))
