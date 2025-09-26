setwd("C:\\Users\\it24101099\\Desktop\\IT24101099")

data<-read.table("Exercise - LaptopsWeights.txt",header=TRUE)
fix(data)
attach(data)

#Q1
popmn<-mean(Weight.kg.)
popmn

popvar<-var(Weight.kg.)
popvar

#Q2

samples<-c()
n<-c()

for(i in 1:25){
  s<-sample(Weight.kg.,6,replace=TRUE)
  samples<-cbind(samples,s)
  n<-c(n,paste('S',i))
}

samples


colnames(samples) = n
samples


s.means<-apply(samples,2,mean)
s.means

s.vars<-apply(samples,2,var)
s.vars

#Q3
samplemean<-mean(s.means)
samplevars<-var(s.means)

samplemean
samplevars

popmn
samplemean
