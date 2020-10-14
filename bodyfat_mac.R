library(leaps)
library(MASS)
setwd("/Users/yicenliu/xuexi/628/628/")
data_raw=read.csv("BodyFat.csv")[,-1]
names(data_raw)=tolower(names(data_raw))
## model 1


name_model1=(names(data_raw)[-1])[c(2,3,4)]
name_set=t(combn(name_model1,2))
name_set_paste=paste(paste(name_set[,1],name_set[,2],sep=":"),collapse = "+")
name_set_paste2=paste(name_model1,collapse = "+")
fmla=as.formula(paste("bodyfat~",name_set_paste2,"+",name_set_paste,sep=""))
lm_election=lm(fmla,data=data_raw)


X <- model.matrix(lm_election)[,-1]
election.leaps <- leaps(X, data_1$y, nbest=1, method='adjr2')

best.model.adjr2 <- election.leaps$which[which.max(election.leaps$adjr2),]
a=colnames(X)[best.model.adjr2]

plot(election.leaps$size, election.leaps$adjr2, pch=23, bg='orange', cex=2, 
     xlab="Number of Parameters", ylab="Adjusted R2 of the Best Model",
     main="Adjusted R^2 for Each Possible Model in Our Election Data")

fmla_model1=as.formula(paste("bodyfat~",paste(a,collapse = "+"),sep=""))
model1=lm(fmla_model1,data_raw)
model1_coefficients=data.frame(vairable=c(names(model1$coefficients)),coefficients=model1$coefficients)
row.names(model1_coefficients)=c()
write.csv(model1_coefficients,"mode1.csv",header=FALSE)
## model2

name_model2=(names(data_raw)[-1])[c(2,3,4,7,9,15)]
name_set=t(combn(name_model2,2))
name_set_paste=paste(paste(name_set[,1],name_set[,2],sep=":"),collapse = "+")
name_set_paste2=paste(name_model2,collapse = "+")
fmla=as.formula(paste("bodyfat~",name_set_paste2,"+",name_set_paste,sep=""))
lm_election=lm(fmla,data=data_raw)



X <- model.matrix(lm_election)[,-1]
election.leaps <- leaps(X, data_1$y, nbest=1, method='adjr2')

best.model.adjr2 <- election.leaps$which[which.max(election.leaps$adjr2),]
a=colnames(X)[best.model.adjr2]

plot(election.leaps$size, election.leaps$adjr2, pch=23, bg='orange', cex=2, 
     xlab="Number of Parameters", ylab="Adjusted R2 of the Best Model",
     main="Adjusted R^2 for Each Possible Model in Our Election Data")

fmla_model2=as.formula(paste("bodyfat~",paste(a,collapse = "+"),sep=""))


model2=lm(fmla_model2,data_raw)
model2_coefficients=data.frame(vairable=c(names(model2$coefficients)),coefficients=model2$coefficients)
row.names(model2_coefficients)=c()
write.csv(model2_coefficients,"mode2.csv")
## model3
name_model3=(names(data_raw)[-1])[2:15]
name_set=t(combn(name_model3,2))
name_set_paste=paste(paste(name_set[,1],name_set[,2],sep=":"),collapse = "+")
name_set_paste2=paste(name_model3,collapse = "+")
fmla=as.formula(paste("bodyfat~",name_set_paste2,"+",name_set_paste,sep=""))

election.step.backward <- stepAIC(lm(fmla, data_raw), direction='backward',steps=5000)

model3_coefficients=data.frame(vairable=c(names(election.step.backward$coefficients)),coefficients=election.step.backward$coefficients)
row.names(model3_coefficients)=c()
write.csv(model3_coefficients,"mode3.csv")
plot()

#election.step.backward <- step(lm(y~1, data_1),list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15), direction='forward', k=2)
#election.step.backward <- step(lm(y~., data_1), direction='backward')
#election.step.both <- step(lm(y~., data_1), direction='both')

#lm_model=lm(y~x1+x2+x3+x7+x12+x13,data_1)
