#載入所需的packages
library(lubridate)
library(ggplot2)
library(GGally)
require(reshape2)
require(scales)
library(coefplot)
#建立整理初始資料的function
makeData1<-function(originaldata1,time){
  originaldata1<-as.data.frame(lapply(originaldata1, as.numeric)) #將空白改為NA
  originaldata1<-originaldata1[-1,]
  originaldata1$觀測時間.hour.<-time#加入時間欄位，以便data1、data2組合
  #擷取需要的資料並重新命名
  originaldata1<-originaldata1[,c(1,2,4,6:8,11)]
  colnames(originaldata1)<-c("時間","氣壓","氣溫","相對溼度","風速","風向","降水量")
  return(originaldata1)
}
makeData2<-function(originaldata2,time,item){
  originaldata2<-originaldata2[originaldata2$測項!='CH4                 ',]#去除2022新增的CH4資料
  originaldata2<-originaldata2[originaldata2$測項!='THC                 ',]#去除2022新增的THC資料
  originaldata2<-originaldata2[originaldata2$測項!='NMHC                ',]#去除2022新增的NMHC資料
  originaldata2<-originaldata2[-1]
  i<-0
  cname<-originaldata2[1:item,2]
  #將原始資料儲存方式改為每小時一個row，裡面存放各col的資料
  while(i<nrow(originaldata2)){
    if(i==0){
      middle<-originaldata2[(i+1):(i+item),-2]
      middle<-as.data.frame(t(middle),stringAsFactors=FALSE)
      middle<-middle[-1,]
      colnames(middle)<-cname
      data2<-middle
    }else{
      middle<-originaldata2[(i+1):(i+item),-2]
      middle<-as.data.frame(t(middle),stringAsFactors=FALSE)
      middle<-middle[-1,]
      colnames(middle)<-cname
      data2<-rbind(data2,middle)
    }
    i<-i+item;
  }
  data2col<-trimws(colnames(data2))#去除colnames頭尾空白並紀錄
  data2<-as.data.frame(lapply(data2, as.numeric)) #將空白改為NA
  data2$時間<-time#加入時間欄位，以便data1、data2組合
  colnames(data2)<-c(data2col,"時間")
  return(data2)
}
finishdata<-function(original_data1,original_data2,item,time){
  air<-makeData2(original_data2,time,item)#item變數數量
  weather<-makeData1(original_data1,time)
  finish<-merge(weather,air,by="時間")#利用時間欄位將兩資料彙整
  finish<-na.omit(finish)#只要有NA值，就移除該列資料不採用
  finish<-finish[,c(1:2,4,7:22)]#氣溫、風向、風速有兩個相同變數，移除data1的
  return(finish)
}
#建立原始資料
#橋頭2020
original_2020data1<-read.table("C:/Users/user/Desktop/大二下進階R資料分析與應用/期中/2020weather.csv",fileEncoding="UTF-8",header=TRUE,sep = ",", stringsAsFactors = FALSE)
original_2020data2<-read.table("C:/Users/user/Desktop/大二下進階R資料分析與應用/期中/2020air.csv",fileEncoding="BIG-5",header=TRUE,fill=TRUE ,sep = ",", stringsAsFactors = FALSE)
original_2020data2<-original_2020data2[-1,] #第一筆資料為無效資訊
time2020<-seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-12-31 23:00:00"),by="hour")#時間範圍2020
finish2020<-finishdata(original_2020data1,original_2020data2,15,time2020)
#橋頭2021
original_2021data1<-read.table("C:/Users/user/Desktop/大二下進階R資料分析與應用/期中/2021weather.csv",fileEncoding="UTF-8",header=TRUE,sep = ",", stringsAsFactors = FALSE)
original_2021data2<-read.table("C:/Users/user/Desktop/大二下進階R資料分析與應用/期中/2021air.csv",fileEncoding="BIG-5",header=TRUE,fill=TRUE ,sep = ",", stringsAsFactors = FALSE)
original_2021data2<-original_2021data2[-1,]#第一筆資料為無效資訊
time2021<-seq(as.POSIXct("2021-01-01 00:00:00"), as.POSIXct("2021-12-31 23:00:00"),by="hour")#時間範圍2021
finish2021<-finishdata(original_2021data1,original_2021data2,15,time2021)
#橋頭2022
original_2022data1<-read.table("C:/Users/user/Desktop/大二下進階R資料分析與應用/期中/2022weather.csv",fileEncoding="UTF-8",header=TRUE,sep = ",", stringsAsFactors = FALSE)
original_2022data2<-read.table("C:/Users/user/Desktop/大二下進階R資料分析與應用/期中/2022air.csv",fileEncoding="BIG-5",header=TRUE,fill=TRUE ,sep = ",", stringsAsFactors = FALSE)
time2022<-seq(as.POSIXct("2022-01-01 00:00:00"), as.POSIXct("2022-12-31 23:00:00"),by="hour")#時間範圍2022
finish2022<-finishdata(original_2022data1,original_2022data2,15,time2022)
#初始資料處理完畢，橋頭2020~2022
#基礎統計
#從資料中抓出O3與PM2.5兩筆資料出來用cor做出兩者的相關係數
statistic<-finish2022
cor(statistic$O3, statistic$PM2.5)
#用cor找出氣壓、相對濕度、風速、風向間的相關係數
cor(statistic[, c(10, 15:17)])
#用GGally畫出相關圖
ggpairs(statistic[, c(10, 12:17)], wrap = list(labelSize = 8))
#顯示格式化的資料
finCor <- cor(statistic[, c(6:12)])
finMelt <- melt(finCor, varnames=c("x", "y"),value.name="Correlation")
finMelt <- finMelt[order(finMelt$Correlation), ]
finMelt
#用ggplot作圖
ggplot(finMelt, aes(x=x, y=y)) +geom_tile(aes(fill=Correlation)) +scale_fill_gradient2(low=muted("red"), mid="white",high="steelblue",guide=guide_colorbar(ticks=FALSE, barheight=10),limits=c(-1, 1)) +theme_minimal() +labs(x=NULL, y=NULL)
#問題1:針對PM2.5含量進行建模預測
question1data<-finish2022#取資料
cor(question1data[,c(2:19)])
#點狀圖
ggplot(question1data, aes(x =PM2.5, y = AMB_TEMP)) + geom_point()+geom_smooth(method="lm")
ggplot(question1data, aes(x =PM2.5, y = 氣壓)) + geom_point()+geom_smooth(method="lm")
ggplot(question1data, aes(x =PM2.5, y = CO)) + geom_point()+geom_smooth(method="lm")
ggplot(question1data, aes(x =PM2.5, y = NO2)) + geom_point()+geom_smooth(method="lm")
ggplot(question1data, aes(x =PM2.5, y = NOx)) + geom_point()+geom_smooth(method="lm")
#去除極端值數據
question1data<-question1data[question1data$NOx<50,]
question1data<-question1data[question1data$CO<0.8,]
question1data<-question1data[question1data$NO2<35,]
#CO、氣溫、NOx、氣壓、NO2對PM2.5可能有影響，以此建模
question1_1 <- lm(PM2.5 ~ 氣壓, data = question1data)
question1_2 <- lm(PM2.5 ~ NO2*NOx+氣壓, data = question1data)
question1_3 <- lm(PM2.5 ~ CO+NOx+NO2, data = question1data)
question1_4 <- lm(PM2.5 ~ AMB_TEMP+CO+氣壓, data = question1data)
question1_5 <- lm(PM2.5 ~ AMB_TEMP+CO+氣壓+NO2*NOx, data = question1data)
#逐步挑選法建模
nullmodelQ1<-lm(PM2.5 ~ 1, data = question1data)
fullmodelQ1<-lm(PM2.5 ~ 氣壓*NO2*NOx*CO*AMB_TEMP, data = question1data)
question1_6 <- step(nullmodelQ1,scope=list(lower=nullmodelQ1, upper=fullmodelQ1),direction="both")
#模型診斷(6雖然最好但變數太多了不適合，僅供參考)
anova(question1_1,question1_2,question1_3,question1_4,question1_5,question1_6)#5最好
AIC(question1_1,question1_2,question1_3,question1_4,question1_5,question1_6)#5最好
#5號模型QQ圖
ggplot(question1_5, aes(sample = .stdresid)) + stat_qq() + geom_abline()
#用2021年資料來測試預測力
Q1Predict <- predict(question1_5, newdata =finish2021, se.fit = TRUE, interval = "prediction", level = .95)
cor(Q1Predict$fit,finish2021$PM2.5)
#畫圖
Q1_draw2021<-as.data.frame(cbind("PM2.5"=finish2021$PM2.5,Q1Predict$fit))
Q1_gp2021<-ggplot(Q1_draw2021,aes(x=1:nrow(Q1_draw2021),y=1:50))+geom_line(aes(x=1:nrow(Q1_draw2021),y=fit,color="預測PM2.5"))+geom_line(aes(x=1:nrow(Q1_draw2021),y=PM2.5,color="實際PM2.5"))
Q1_gp2021<-Q1_gp2021+scale_color_discrete(name="項目")+labs(title="預測2021年PM2.5結果比較圖", x="測資",y="數值")
Q1_gp2021
#用2020年資料來測試預測力
Q1Predict <- predict(question1_5, newdata =finish2020, se.fit = TRUE, interval = "prediction", level = .95)
cor(Q1Predict$fit,finish2020$PM2.5)
#畫圖
Q1_draw2020<-as.data.frame(cbind("PM2.5"=finish2020$PM2.5,Q1Predict$fit))
Q1_gp2020<-ggplot(Q1_draw2020,aes(x=1:nrow(Q1_draw2020),y=1:50))+geom_line(aes(x=1:nrow(Q1_draw2020),y=fit,color="預測PM2.5"))+geom_line(aes(x=1:nrow(Q1_draw2020),y=PM2.5,color="實際PM2.5"))
Q1_gp2020<-Q1_gp2020+scale_color_discrete(name="項目")+labs(title="預測2020年PM2.5結果比較圖", x="測資",y="數值")
Q1_gp2020
#問題2:影響溫度高低的因素
question2data<-finish2022#取資料
#點狀圖
ggplot(question2data, aes(x =AMB_TEMP, y = CO)) + geom_point()+geom_smooth(method="lm")
ggplot(question2data, aes(x =AMB_TEMP, y = NO)) + geom_point()+geom_smooth(method="lm")
ggplot(question2data, aes(x =AMB_TEMP, y = 氣壓)) + geom_point()+geom_smooth(method="lm")
ggplot(question2data, aes(x =AMB_TEMP, y = NOx)) + geom_point()+geom_smooth(method="lm")
#去除極端值數據
question2data<-question2data[question2data$CO<0.7,]
question2data<-question2data[question2data$NO<20,]
question2data<-question2data[question2data$NOx<40,]
#CO、NO、NOx、氣壓對氣溫可能有影響，以此建模
question2_1 <- lm(AMB_TEMP ~ 氣壓, data = question2data)
question2_2 <- lm(AMB_TEMP ~ NO+NOx, data = question2data)
question2_3 <- lm(AMB_TEMP ~ CO*NO*NOx, data = question2data)
question2_4 <- lm(AMB_TEMP ~ CO*NOx+氣壓, data = question2data)
question2_5 <- lm(AMB_TEMP ~ NO*NOx+氣壓, data = question2data)
#逐步挑選法建模
nullmodelQ2<-lm(AMB_TEMP ~ 1, data = question2data)
fullmodelQ2<-lm(AMB_TEMP ~ 氣壓*NO*NOx*CO, data = question2data)
question2_6 <- step(nullmodelQ2,scope=list(lower=nullmodelQ2, upper=fullmodelQ2),direction="both")
#模型診斷(6雖然最好但變數太多了不適合，僅供參考)
anova(question2_1,question2_2,question2_3,question2_4,question2_5,question2_6)#5最好
AIC(question2_1,question2_2,question2_3,question2_4,question2_5,question2_6)#5最好
#5號模型QQ圖
ggplot(question2_5, aes(sample = .stdresid)) + stat_qq() + geom_abline()
#用2021年資料來測試預測力
Q2Predict <- predict(question2_5, newdata =finish2021, se.fit = TRUE, interval = "prediction", level = .95)
cor(Q2Predict$fit,finish2021$AMB_TEMP)
#畫圖
Q2_draw2021<-as.data.frame(cbind("temp"=finish2021$AMB_TEMP,Q2Predict$fit))
Q2_draw2021<-Q2_draw2021[finish2021$NO<20&finish2021$NOx<40,]#NO、NOx異常高會導致預測失準，先移除不採用
Q2_gp2021<-ggplot(Q2_draw2021,aes(x=1:nrow(Q2_draw2021),y=1:50))+geom_line(aes(x=1:nrow(Q2_draw2021),y=fit,color="預測溫度"))+geom_line(aes(x=1:nrow(Q2_draw2021),y=temp,color="實際溫度"))
Q2_gp2021<-Q2_gp2021+scale_color_discrete(name="項目")+labs(title="預測2021年氣溫結果比較圖", x="測資",y="攝氏")
Q2_gp2021
#用2020年資料來測試預測力
Q2Predict <- predict(question2_5, newdata =finish2020, se.fit = TRUE, interval = "prediction", level = .95)
cor(Q2Predict$fit,finish2020$AMB_TEMP)
#畫圖
Q2_draw2020<-as.data.frame(cbind("temp"=finish2020$AMB_TEMP,Q2Predict$fit))
Q2_draw2020<-Q2_draw2020[finish2020$NO<20&finish2020$NOx<40,]#NO、NOx異常高會導致預測失準，先移除不採用
Q2_gp2020<-ggplot(Q2_draw2020,aes(x=1:nrow(Q2_draw2020),y=1:50))+geom_line(aes(x=1:nrow(Q2_draw2020),y=fit,color="預測溫度"))+geom_line(aes(x=1:nrow(Q2_draw2020),y=temp,color="實際溫度"))
Q2_gp2020<-Q2_gp2020+scale_color_discrete(name="項目")+labs(title="預測2020年氣溫結果比較圖", x="測資",y="攝氏")
Q2_gp2020
#問題3:是否有降雨的影響因素
question3data<-finish2021#取資料
question3data$是否降雨 <- with(question3data, 降水量 > 0) #增加是否降雨二元變數欄位(TRUE > 0; FALSE <= 0)
question3_1 <- glm(是否降雨 ~ AMB_TEMP  + 相對溼度 + WD_HR + WS_HR + NO + 氣壓 + CO + NO + NO2 + NOx + O3 + PM10 + PM2.5 + SO2 -1, data=question3data, family=binomial(link="logit"))
summary(question3_1)
coefplot(question3_1)#繪製係數圖
invlogit <- function(x)#用羅吉反函數對係數做出轉換
{
  1/(1 + exp(-x))
}
invlogit(question3_1$coefficients)
#最高WS_HR:0.6685519，最低AMB_TEMP:0.2051367

