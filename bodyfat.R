library(leaps)
setwd("C:/Users/liuyi/OneDrive/xuexi2/628/bodyfat/")
data_raw=read.csv("BodyFat.csv")[,-1]
data_1=data_raw
names(data_1)=c("y",paste("x",1:15,sep = ""))


lm_election=lm(y~.,data=data_1)

X <- model.matrix(lm_election)[,-1]
election.leaps <- leaps(X, data_1$y, nbest=1, method='adjr2')

best.model.adjr2 <- election.leaps$which[which.max(election.leaps$adjr2),]
colnames(X)[best.model.adjr2]

plot(election.leaps$size, election.leaps$adjr2, pch=23, bg='orange', cex=2, 
     xlab="Number of Parameters", ylab="Adjusted R2 of the Best Model",
     main="Adjusted R^2 for Each Possible Model in Our Election Data")


election.step.backward <- step(lm(y~1, data_1),list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15), direction='forward', k=2)
election.step.backward <- step(lm(y~., data_1), direction='backward')
election.step.both <- step(lm(y~., data_1), direction='both')

lm_model=lm(y~x1+x2+x3+x7+x12+x13,data_1)
lm_model$coefficients
