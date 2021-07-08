rm(list=ls())
#d1=read.table("Desktop/SEM-1/student/student-mat.csv",sep=";",header=TRUE)
d1=read.csv("student-mat.csv")
View(d1)
dim(d1)
data = subset(d1, select = -c(school,address,reason,traveltime) )
dim(data)
attach(data)
glm.fit <- glm(G3 ~., data = data)
summary(glm.fit)

glmg1 <- glm(G3 ~ G1, data=data)
summary(glmg1)
glmg2 <- glm(G3 ~ G2, data=data)
summary(glmg2)


glm1 <- glm(G3 ~ sex, data=data)
summary(glm1)#yes
glm2 <- glm(G3 ~ age, data=data)
summary(glm2)#yes
glm3 <- glm(G3 ~ famsize, data=data)
summary(glm3)
glm4 <- glm(G3 ~ Pstatus, data=data)
summary(glm4)
glm5 <- glm(G3 ~ Medu, data=data)
summary(glm5)#yes
glm6 <- glm(G3 ~ Fedu, data=data)
summary(glm6)#yes
glm7 <- glm(G3 ~ Mjob, data=data)
summary(glm7)#yes
glm8 <- glm(G3 ~ Fjob, data=data)
summary(glm8)#0.33
glm9 <- glm(G3 ~ guardian, data=data)
summary(glm9)
glm10 <- glm(G3 ~ studytime, data=data)
summary(glm10)
glm11 <- glm(G3 ~ failures, data=data)
summary(glm11)
glm12 <- glm(G3 ~ schoolsup, data=data)
summary(glm12)
glm13 <- glm(G3 ~ famsup, data=data)
summary(glm13)
glm14 <- glm(G3 ~ paid, data=data)
summary(glm14)
glm15 <- glm(G3 ~ activities, data=data)
summary(glm15)
glm16 <- glm(G3 ~ nursery, data=data)
summary(glm16)
glm17 <- glm(G3 ~ higher, data=data)
summary(glm17)
glm18 <- glm(G3 ~ internet, data=data)
summary(glm18)
glm19 <- glm(G3 ~ romantic, data=data)
summary(glm19)
glm20 <- glm(G3 ~ famrel, data=data)
summary(glm20)
glm21 <- glm(G3 ~ freetime, data=data)
summary(glm21)
glm22 <- glm(G3 ~ goout, data=data)
summary(glm22)
glm23 <- glm(G3 ~ Dalc, data=data)
summary(glm23)
glm24 <- glm(G3 ~ Walc, data=data)
summary(glm24)
glm25 <- glm(G3 ~ health, data=data)
summary(glm25)
glm26 <- glm(G3 ~ absences, data=data)
summary(glm26)


glmfinal <- glm(G3 ~., data=data)
summary(glmfinal)



##install.packages("caret")
##install.packages("e1071")



library(boot)
library(caret)

#cverror
##cross-validation
mycost <- function(r, pi = 0) {
  mean(abs(r-pi) > 0.5)}
cv.error.10 <- cv.glm(data, glmfinal, cost=mycost, K=10)
cv.error.10$delta






# Implementing random forests to data
#RandomForests
library(randomForest)
set.seed(42)
N<- nrow(data)
test <- sample(N, N/3)
train <- seq(1:N)[-test]
test

P<-ncol(data)-1

rf.updt1 <- randomForest(G3~., data=data, subset=train, ry=P/3, importance=TRUE)
rf.updt1
importance(rf.updt1)



set.seed(4543)
data.rf <- randomForest(G3 ~ ., data=data, ntree=1000, keep.forest=FALSE,importance = TRUE)
varImpPlot(data.rf)


