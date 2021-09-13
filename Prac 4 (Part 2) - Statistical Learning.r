dev.new(width=5, height=4) # set the figure size to be 5in x 4in

library("readr")
fheight<-read_csv("./PearsonFather.csv",col_names="fheight",col_types="d")
sheight<-read_csv("./PearsonSon.csv",col_names="sheight",col_types="d")
fs_height<-data.frame(fheight,sheight)
plot(fs_height$fheight,fs_height$sheight,main="Father and Son Heights", xlab="Father Heights (in)",ylab="Son Heights (in)",col=rgb(1,0.3,0.1,0.5))
summary(fs_height)

lmfit<-with(fs_height,lm(sheight~fheight))
summary(lmfit)

# [Place your Answer here]
fsCoef1 = 0.51401

# [Place your Answer here]
intCoef1 = 33.89280

lmfit.res<-resid(lmfit)
plot(fs_height$fheight,lmfit.res,
     main="Residual Son Heights from Simple Linear Regression", 
     xlab="Father Heights (in)",
     ylab="Residual Son Heights (in)",
     col=rgb(1,0.3,0.1,0.5))
summary(lmfit.res)

library("car")
qqPlot(lmfit.res,xlab="Standard Normal Quantiles",ylab="Quantiles of the Residuals")

shapiro.test(lmfit.res)

summary(lmfit)

# [Place your Answer here]
boolean1 = c(TRUE, TRUE, TRUE)

confint(lmfit)

predict(lmfit, data.frame(fheight=67))

modelFun = function(x) {
    predict = 5 ;
    return(round(20+(1.234*x), 2))
}

equa1 = modelFun(123)

predict(lmfit,data.frame(fheight=67),interval="confidence",level = 0.95,type="response")

newfheight<-seq(min(fheight$fheight)-1, max(fheight$fheight)+1, by=0.1)
lmfitci<-predict(lmfit,data.frame(fheight=newfheight),interval="confidence",level = 0.95,type="response")
plot(fs_height$fheight,fs_height$sheight,main="Father and Son Heights", xlab="Father Heights (in)",ylab="Son Heights (in)",col=rgb(1,0.3,0.1,0.5))
lines(newfheight,lmfitci[,1],col="black",lty=1)
lines(newfheight,lmfitci[,2],col="black",lty=2)
lines(newfheight,lmfitci[,3],col="black",lty=2)

predict(lmfit,data.frame(fheight=67),interval="predict",level=0.95,type="response")

# [Place your Answer here]
newfheight<-seq(58,76)


# [Place your Answer here]


N = length(fheight$fheight)
set.seed(107)
trainidx = sample(1:N, floor(0.7*N), replace=FALSE)
tr = data.frame(fheight=fheight$fheight[trainidx], sheight=sheight$sheight[trainidx])
ts = data.frame(fheight=fheight$fheight[-trainidx], sheight=sheight$sheight[-trainidx])

# [Place your Answer here].  Include the desciptive portion of the answer as an R comment.


library("readr")
HR_comma_sep <- read_csv("HR_comma_sep.csv")
HR_best <- HR_comma_sep[(HR_comma_sep$last_evaluation>=0.8)&(HR_comma_sep$time_spend_company>=4),]
head(HR_best,10)
summary(HR_best)
set.seed(8888) # Set a seed for the random number generator;
trainidx<-sample(nrow(HR_best), floor(nrow(HR_best) * 0.5))
HR_best_train<-HR_best[trainidx,]
HR_best_test<-HR_best[-trainidx,]

plot(HR_best_train$average_montly_hours,HR_best_train$left,main="Left vs Average Monthly Hours Worked (Best)", xlab="Average Monthly Hours",ylab="Left",col=rgb(0,0,1,0.5))

logfit<-glm(left~average_montly_hours,data=HR_best_train,family=binomial)
summary(logfit)
newamh<-seq(96,310)
predprobs<-predict(logfit,data.frame(average_montly_hours=newamh),type="response")
plot(HR_best_train$average_montly_hours,HR_best_train$left,main="Left vs Average Monthly Hours Worked (Best)", xlab="Average Monthly Hours",ylab="Left",col=rgb(0,0,1,0.5))
lines(newamh,predprobs,col="black",lty=1)

# [Place your Answer here]


HR_best_train$probs<-predict(logfit,data.frame(average_montly_hours=HR_best_train$average_montly_hours),type="response")
HR_best_train$predleft<-as.integer(HR_best_train$probs>=0.5)
table(HR_best_train$predleft,HR_best_train$left)

HR_best_test$probs<-predict(logfit,data.frame(average_montly_hours=HR_best_test$average_montly_hours),type="response")
HR_best_test$predleft<-as.integer(HR_best_test$probs>=0.5)
table(HR_best_test$predleft,HR_best_test$left)

# [Place your Answer here]


# [Place your Answer here]


library("ROCR")
trainROC<-performance(prediction(HR_best_train$probs,HR_best_train$left),"tpr","fpr")
plot(trainROC)
abline(a=0, b= 1,lty=2)
trainAUC<-as.double(performance(prediction(HR_best_train$probs,HR_best_train$left),"auc")@y.values)
trainAUC

# [Place your Answer here]


logfit2<-glm(left~average_montly_hours*satisfaction_level*number_project,data=HR_best_train,family=binomial)
summary(logfit2)

HR_best_train$probs2<-predict(logfit2,data.frame(average_montly_hours=HR_best_train$average_montly_hours,satisfaction_level=HR_best_train$satisfaction_level,number_project=HR_best_train$number_project),type="response")
trainROC2<-performance(prediction(HR_best_train$probs2,HR_best_train$left),"tpr","fpr")
plot(trainROC2)
abline(a=0, b= 1,lty=2)
trainAUC2<-as.double(performance(prediction(HR_best_train$probs2,HR_best_train$left),"auc")@y.values)
trainAUC2

# [Place your Answer here].  Include the desciptive portion of the answer as an R comment.


# [Place your Answer here]


library("MASS")
ldafit<-lda(left~average_montly_hours*satisfaction_level*number_project,data=HR_best_train)
temp<-predict(ldafit,data.frame(average_montly_hours=HR_best_test$average_montly_hours,satisfaction_level=HR_best_test$satisfaction_level,number_project=HR_best_test$number_project),type="response")
HR_best_test$probs3<-temp$posterior[,2]
testROC3<-performance(prediction(HR_best_test$probs3,HR_best_test$left),"tpr","fpr")
plot(testROC3)
abline(a=0, b= 1,lty=2)
testAUC3<-as.double(performance(prediction(HR_best_test$probs3,HR_best_test$left),"auc")@y.values)
testAUC3

HR_left<-HR_comma_sep[HR_comma_sep$left==1,]
head(HR_left)

library("ggplot2") # Expanded plotting functionality over "lattice" package
x<-cbind(HR_left$average_montly_hours,HR_left$satisfaction_level,HR_left$last_evaluation)
kmfit<-kmeans(x,3,nstart=25) # Find the best 3 clusters using 25 random sets of (distinct) rows in x as initial centres.
pairs(x,col=(kmfit$cluster),labels=c("Av. Mon. Hrs","Sat. Lvl","Last Eval."))

x<-cbind(HR_left$average_montly_hours,100*HR_left$satisfaction_level,100*HR_left$last_evaluation)
set.seed(55)
kmfit<-kmeans(x,3,nstart=25) # Find the best 3 clusters using 25 random restarts
pairs(x,col=(kmfit$cluster),labels=c("Av. Mon. Hrs","Sat. Lvl","Last Eval."))

# [Place your Answer here].  Include the desciptive portion of the answer as an R comment.




print("This Line gets printed if there is no error, when Kernel -> Restart & Run All")
