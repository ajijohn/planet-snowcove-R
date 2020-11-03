#Calculating F-measure with Random Forest
#gistfile1.r
#################################
##Programmer: James E. Yonamine
##Helpful edits: Peter Li
##Date: 9/2/2012
##Random Forest Alpha Cutoff Function
#################################

#https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/

##load pacakges and data
library(foreign)
library(MASS)
library(randomForest)
rm(list=ls())
set.seed(2012)

data<-read.dta("master.dta") #make sure to set your working directory
data<-na.omit(data)
data.x<-data[,3:153] #extract covariates
data.x<-as.matrix(data.x)

for (i in 1:ncol(data.x)) {
  data.x[,i] <- (data.x[,i] - mean(data.x[,i])) / sd(data.x[,i])
}

data.y<-data$onset
#data.y.0 <- data.y - mean(data.y)
#y.matrix<-as.matrix(data.y.0)
data.y<-as.matrix(data.y)
data.x<-as.matrix(data.x)

x.train<-data.x[1:1960,]
x.test<-data.x[1961:3920,]
y.train<-data.y[1:1960,]
y.test<-data.y[1961:3920,]
train<-cbind(y.train, x.train)
test<-cbind(y.test, x.test)

#####################################################
##############Random Forest F-measure#################
#####################################################

#parameters user needs to specify
#y.train = vector of binary dependent variables to trianing set
#train = matrix of x covariates that correspond to y.train
#mtry = number of covariates you want to use for each tree
#sim = number of trees you want to grow

y.train<-y.train
x.train<-train
y.test<-y.test
sim<-100  
mtry=3
####loop the random forest###
#normally, you would change the ntree value within the randomForest package.  By running ntree=1 in a loop, we save the 
#prediction of each tree for each observation into the history matrix.  This allows us to see exactly how many of the n=sim 
#trees predict a "0" or a "1" for each observation. 

history <- matrix(data=0, nrow=nrow(test), ncol=sim)

for (k in 1:sim){
  rf.fit <- randomForest(as.factor(y.train)~., data=train,  ntree=1, mtry=mtry)
  rf.predict<-predict(rf.fit, newdata=x.test)
  matrix<-as.matrix(rf.predict)
  nrow(matrix)
  history[,k]<-as.numeric(matrix[,1])	
}

sum<-as.matrix(rowSums(history))
####calculate accuracy###
#Now we have the all of the tree predictions, we want to calculate our model's predictive accuracy at all possible cutpoints 
#(i.e. if sim=100, then we will test accuracy with every integer cutpoint from 1-100). Performance is a matrix with each row 
#corresponding to to an observation in the test set.  The "rforest" column reflects how many of the nsim trees predict that
#observation to by of class "1". "y.hat" indicates the prediction based on the cutpoint.  y=the true class. 

performance<-matrix(data=0, nrow=nrow(sum), ncol=7)
colnames(performance)<-c("rforest", "y.hat", "y", "true.pos", "true.neg", "false.pos", "false.neg")
performance[,1]<-sum
performance[,3]<-y.test
#the compare matrix stores the accuracy scores for each cutpoint.  Each row reflects the accuracy of each cutpoint. 
compare<-matrix(data=0, nrow=sim, ncol=8)
colnames(compare)<-c("cutoff", "precision", "recall", "sensitivity", "specificity", "PPV", "NPV", "break")

for (j in 1:sim){
  performance[, 2] <- performance[, 1] > j  #sets the cutoff iteratively from 1-sim
  
  performance[,4] <-performance[,2] == 1 & performance[,3]==1
  performance[,5] <-performance[,2] == 0 & performance[,3]==0
  performance[,6] <-performance[,2] == 1 & performance[,3]==0
  performance[,7] <-performance[,2] == 0 & performance[,3]==1
  
  true.ones<-sum(performance[,3])
  true.zeros<-nrow(performance)-true.ones
  
  precision<-sum(performance[,4])/sum(performance[,2])
  recall<-sum(performance[,4])/sum(performance[,3])
  sensitivity<-sum(performance[,4])/true.ones
  specificity<-sum(performance[,5])/true.zeros
  pos.pred.value<-sum(performance[,4])/(sum(performance[,4])+sum(performance[,6]))
  neg.pred.value<-sum(performance[,5])/(sum(performance[,5])+sum(performance[,7]))
  compare[j,1]<-j
  compare[j,2]<-precision
  compare[j,3]<-recall
  compare[j,4]<-sensitivity
  compare[j,5]<-specificity
  compare[j,6]<-pos.pred.value
  compare[j,7]<-neg.pred.value
  compare[j,8]<-abs(precision-recall)
}
compare[is.na(compare)]<-0
sub<-subset(compare, compare[,2]>.0) #this eliminates cutpoints that predict all "1's" or all "0's".  
#If all "1's" or all "0's" gives the best prediction, then why did you run a model in the first place???
F<-which.min(sub[,8]) #the precision-recall will rarely = 0, so we want the cutpoint that gets us the closest to 0. 

###Print the results##
F  # this is the cutpoint
compare[F, 4]
compare[F, 5]
compare[F, 6]
compare[F, 7]