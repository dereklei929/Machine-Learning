library(dplyr)
library(ggplot2)
library(RCurl)
library(rmarkdown)
library(knitr)
library(stats4)
library(stats)
library(pander)
library(NbClust)
library(randomForest)
library(ROCR)
library(glmnet)
library(e1071)
library(doParallel)
library(data.table)
library(tidyr)
library(gridExtra)

rm(list=ls())
dr<-data.table(read.csv("default of credit card clients.csv",header=TRUE,sep = ",",skip=1)%>%rename(y=default.payment.next.month))
set.seed(0706)

#Data manipulation
d<-data.table(dr)

d[,ID:=NULL]

d[,SEX:=factor(SEX,levels=c("1","2"),labels=c("Male","Female"))]
d[,EDUCATION:=as.factor(EDUCATION)]
d[,MARRIAGE:=as.factor(MARRIAGE)]


ggplot(data.table(gather(d[,.(SEX,EDUCATION,MARRIAGE)],K,V)))+geom_bar(aes(x=V,fill=V))+facet_wrap(~K,scales="free")+
  xlab("")+ylab("Count")+scale_fill_brewer(palette = "Spectral",guide=FALSE)


ggplot(d[EDUCATION%in%c("1","2","3","4")&MARRIAGE%in%c("1","2","3"),mean(y),by=.(SEX,EDUCATION,MARRIAGE)],aes(x=EDUCATION,y=MARRIAGE))+geom_tile(aes(fill=V1))+facet_wrap(~SEX)+
  scale_fill_distiller(palette="Spectral","Probability")


d$PAY_0<-as.factor(d$PAY_0)
d$PAY_2<-as.factor(d$PAY_2)
d$PAY_3<-as.factor(d$PAY_3)
d$PAY_4<-as.factor(d$PAY_4)
d$PAY_5<-as.factor(d$PAY_5)
d$PAY_6<-as.factor(d$PAY_6)


ggplot(data.table(gather(d[,.(PAY_0,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6)],K,V)))+geom_bar(aes(x=V,fill=log(..count..)))+facet_wrap(~K)+
  scale_x_discrete(limits=sort(d[,.N,by=.(PAY_0)]$PAY_0))+scale_fill_distiller(palette="Spectral",guide=FALSE)


ggplot(data.table(gather(d[,.(BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,BILL_AMT6)],K,V)))+geom_histogram(aes(x=V,fill=log(..count..)))+
  facet_wrap(~K)+scale_fill_distiller(palette="Spectral",guide=FALSE)+xlab("Bill amount")+ylab("Count")

ggplot(data.table(gather(d[,.(BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,BILL_AMT6)],K,V))[V>0])+geom_histogram(aes(x=log(V),fill=log(..count..)))+
  facet_wrap(~K)+scale_fill_distiller(palette="Spectral",guide=FALSE)+xlab("Log bill amount")+ylab("Count")



ggplot(data.table(gather(d[,.(PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)],K,V)))+geom_histogram(aes(x=V,fill=log(..count..)))+
  facet_wrap(~K)+scale_fill_distiller(palette="Spectral",guide=FALSE)+xlab("Payment amount")+ylab("Count")

ggplot(data.table(gather(d[,.(PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)],K,V))[V>0])+geom_histogram(aes(x=log(V),fill=log(..count..)))+
  facet_wrap(~K)+scale_fill_distiller(palette="Spectral",guide=FALSE)+xlab("Log payment amount")+ylab("Count")


ggplot(d[(BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6>0)&(PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5>0)])+geom_hex(aes(x=log(BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6),y=log(PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5)))+
  scale_fill_distiller(palette="Spectral")+xlab("Log Bill amount from Apr to Aug")+ylab("Log Payment amount from May to Sep")+geom_abline(col="#00CC99")

ggplot(d)+
  geom_smooth(se=F,aes(x=log(BILL_AMT2)-log(PAY_AMT1),y=y,col="Aug 2005"))+
  geom_smooth(se=F,aes(x=log(BILL_AMT3)-log(PAY_AMT2),y=y,col="Jul 2005"))+
  geom_smooth(se=F,aes(x=log(BILL_AMT4)-log(PAY_AMT3),y=y,col="Jun 2005"))+
  geom_smooth(se=F,aes(x=log(BILL_AMT5)-log(PAY_AMT4),y=y,col="May 2005"))+
  geom_smooth(se=F,aes(x=log(BILL_AMT6)-log(PAY_AMT5),y=y,col="Apr 2005"))+
  xlab("Difference of log bill and log payment")+ylab("Default probability")+
  scale_color_brewer(palette="Spectral","Month")

ggplot(d)+geom_histogram(aes(x=LIMIT_BAL,fill=..count..))+scale_fill_distiller(palette="Spectral")

ggplot(d[,mean(y),by=(round(exp(round(log(d$LIMIT_BAL),2))))])+geom_smooth(aes(x=round,y=V1),se=F)+
  xlab("Amount of the given credit (NT dollar)")+ylab("Default probability")



d$LLIMIT_BAL<-log(d$LIMIT_BAL)
d$LBILL_AMT1<-ifelse(d$BILL_AMT1>0,log(d$BILL_AMT1),0)
d$LBILL_AMT2<-ifelse(d$BILL_AMT2>0,log(d$BILL_AMT2),0)
d$LBILL_AMT3<-ifelse(d$BILL_AMT3>0,log(d$BILL_AMT3),0)
d$LBILL_AMT4<-ifelse(d$BILL_AMT4>0,log(d$BILL_AMT4),0)
d$LBILL_AMT5<-ifelse(d$BILL_AMT5>0,log(d$BILL_AMT5),0)
d$LBILL_AMT6<-ifelse(d$BILL_AMT6>0,log(d$BILL_AMT6),0)
d$LPAY_AMT1<-ifelse(d$PAY_AMT1>0,log(d$PAY_AMT1),0)
d$LPAY_AMT2<-ifelse(d$PAY_AMT2>0,log(d$PAY_AMT2),0)
d$LPAY_AMT3<-ifelse(d$PAY_AMT3>0,log(d$PAY_AMT3),0)
d$LPAY_AMT4<-ifelse(d$PAY_AMT4>0,log(d$PAY_AMT4),0)
d$LPAY_AMT5<-ifelse(d$PAY_AMT5>0,log(d$PAY_AMT5),0)
d$LPAY_AMT6<-ifelse(d$PAY_AMT6>0,log(d$PAY_AMT6),0)

d$BILL_AMT1_0<-ifelse(d$BILL_AMT1==0,1,0)
d$BILL_AMT2_0<-ifelse(d$BILL_AMT2==0,1,0)
d$BILL_AMT3_0<-ifelse(d$BILL_AMT3==0,1,0)
d$BILL_AMT4_0<-ifelse(d$BILL_AMT4==0,1,0)
d$BILL_AMT5_0<-ifelse(d$BILL_AMT5==0,1,0)
d$BILL_AMT6_0<-ifelse(d$BILL_AMT6==0,1,0)

d$BILL_AMT1_N<-ifelse(d$BILL_AMT1<0,1,0)
d$BILL_AMT2_N<-ifelse(d$BILL_AMT2<0,1,0)
d$BILL_AMT3_N<-ifelse(d$BILL_AMT3<0,1,0)
d$BILL_AMT4_N<-ifelse(d$BILL_AMT4<0,1,0)
d$BILL_AMT5_N<-ifelse(d$BILL_AMT5<0,1,0)
d$BILL_AMT6_N<-ifelse(d$BILL_AMT6<0,1,0)

d$PAY_AMT1_0<-ifelse(d$PAY_AMT1==0,1,0)
d$PAY_AMT2_0<-ifelse(d$PAY_AMT2==0,1,0)
d$PAY_AMT3_0<-ifelse(d$PAY_AMT3==0,1,0)
d$PAY_AMT4_0<-ifelse(d$PAY_AMT4==0,1,0)
d$PAY_AMT5_0<-ifelse(d$PAY_AMT5==0,1,0)
d$PAY_AMT6_0<-ifelse(d$PAY_AMT6==0,1,0)

library(h2o)
h2o.init(nthreads = 6)
h2o.removeAll()


df<-d
df[,arr_delay:=as.factor(ifelse(arr_delay>15,1,0))]
set.seed(0706)
N1<-nrow(df)
vt<-sample(1:N1,0.6*N1)
d_train<-df[vt,]
d_vt<-df[-vt,]
N2<-nrow(d_vt)
t<-sample(1:N2,0.5*N2)
d_valid<-d_vt[t,]
d_test<-d_vt[-t,]

dx_train <- as.h2o(d_train)  ## uploads data to H2O
dx_valid<- as.h2o(d_valid)  ## uploads data to H2O
dx_test <- as.h2o(d_test)

#Elastic Net

h2o.rm("GLM")
GLM <- h2o.grid(
  algorithm = "glm", 
  grid_id = "GLM",
  hyper_params = list(alpha = c(0,0.5,1)),
  training_frame = dx_train,
  validation_frame = dx_valid,
  x=colnames(dx_train)[-24],
  y="y",
  seed=0706,
  nfolds = 5,
  family = "binomial"
)

GLMm<-h2o.getGrid(
  grid_id = "GLM", 
  sort_by = "AUC",
  decreasing = TRUE
)

GLMb<- h2o.getModel(GLMm@model_ids[[1]])
GLMp<-h2o.performance(GLMb)
h2o.auc(GLMp)
GLMrp<-cbind(h2o.fpr(GLMp),h2o.tpr(GLMp)$tpr)
colnames(GLMrp)[3]<-"tpr"
GLMt<-h2o.performance(GLMb,newdata = dx_test)
GLMrt<-cbind(h2o.fpr(GLMt),h2o.tpr(GLMt)$tpr)
h2o.auc(GLMt)
colnames(GLMrt)[3]<-"tpr"

GLMev<-ggplot(h2o.F1(GLMp))+geom_line(aes(x=threshold,y=f1,color=threshold),size=1)+
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5)+
  xlab("Threshold")+ylab("F1 Metric")
GLMet<-ggplot(h2o.F1(GLMt))+geom_line(aes(x=threshold,y=f1,color=threshold),size=1)+
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5)+
  xlab("Threshold")+ylab("F1 Metric")
GLMav<-ggplot(GLMrp,aes(x=fpr,y=tpr))+geom_line(aes(col=threshold),size=1)+xlab("False Positive Rate")+ylab("True Positive Rate")+
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5)
GLMat<-ggplot(GLMrt,aes(x=fpr,y=tpr))+geom_line(aes(col=threshold),size=1)+xlab("False Positive Rate")+ylab("True Positive Rate")+
  scale_color_gradient2("Threshold",low="red",high="green",mid="yellow",midpoint = 0.5)

grid.arrange(GLMev,GLMav,ncol=1)
grid.arrange(GLMet,GLMat,ncol=1)

ggplot(data.table(cbind(h2o.varimp(GLMb)$names[1:10],h2o.varimp(GLMb)$coefficients[1:10],h2o.varimp(GLMb)$sign[1:10]))[order(-V2)])+
  geom_col(aes(x=V1,y=V2,fill=V3))+coord_flip()+scale_x_discrete(limits=rev(h2o.varimp(GLMb)$names[1:10]))+
  theme(axis.text.x=element_blank(),axis.ticks=element_blank())+ylab("Magnitude")+xlab("Variable")+
  scale_fill_discrete("Sign",labels=c("Negative","Positive"))

#Random Forest
h2o.rm("RF")
?h2o.grid
RF <- h2o.grid(
  algorithm = "randomForest", 
  grid_id = "RF",
  hyper_params = list(alpha = c(0,0.5,1)),
  training_frame = dx_train,
  validation_frame = dx_valid,
  x=colnames(dx_train)[-24],
  y="y",
  seed=0706,
  nfolds = 5,
  family = "binomial"
)

GLMm<-h2o.getGrid(
  grid_id = "GLM", 
  sort_by = "AUC",
  decreasing = TRUE
)