library(leaps)
setwd("/Users/yicenliu/xuexi/628/628/")
data_raw=read.csv("BodyFat.csv")[,-1]

## model 1
data_1=data_raw
names(data_1)=c("y",paste("x",1:15,sep = ""))


lm_election=lm(y~x2+x3+x4+x2:x3+x2:x4+x3:x4,data=data_1)

X <- model.matrix(lm_election)[,-1]
election.leaps <- leaps(X, data_1$y, nbest=1, method='adjr2')

best.model.adjr2 <- election.leaps$which[which.max(election.leaps$adjr2),]
colnames(X)[best.model.adjr2]

plot(election.leaps$size, election.leaps$adjr2, pch=23, bg='orange', cex=2, 
     xlab="Number of Parameters", ylab="Adjusted R2 of the Best Model",
     main="Adjusted R^2 for Each Possible Model in Our Election Data")


## model2

name_model2=c("x2","x3","x4","x7","x9","x15")
name_set=t(combn(name_model2,2))
name_set_paste=paste(paste(name_set[,1],name_set[,2],sep=":"),collapse = "+")
fmla=as.formula(paste("y~x2+x3+x4+x7+x9+x15+",name_set_paste,sep=""))
lm_election=lm(fmla,data=data_1)



X <- model.matrix(lm_election)[,-1]
election.leaps <- leaps(X, data_1$y, nbest=1, method='adjr2')

best.model.adjr2 <- election.leaps$which[which.max(election.leaps$adjr2),]
colnames(X)[best.model.adjr2]

plot(election.leaps$size, election.leaps$adjr2, pch=23, bg='orange', cex=2, 
     xlab="Number of Parameters", ylab="Adjusted R2 of the Best Model",
     main="Adjusted R^2 for Each Possible Model in Our Election Data")

## model3

name_model3=names(data_1)[-c(1,2)]
name_set=t(combn(name_model3,2))
name_set_paste=paste(paste(name_set[,1],name_set[,2],sep=":"),collapse = "+")
fmla=as.formula(paste("y~x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+",name_set_paste,sep=""))
election.step.backward <- step(lm(fmla, data_1), direction='backward')




#election.step.backward <- step(lm(y~1, data_1),list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15), direction='forward', k=2)
#election.step.backward <- step(lm(y~., data_1), direction='backward')
#election.step.both <- step(lm(y~., data_1), direction='both')

#lm_model=lm(y~x1+x2+x3+x7+x12+x13,data_1)
