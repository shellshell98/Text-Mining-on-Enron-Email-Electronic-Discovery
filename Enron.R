rm(list=ls())
setwd("/Users/shelleyshi/Desktop/Anderson/19 Spring/MGMTMSA408 - MISIC Operations Analytics/HW2")

email <- read.csv("energy_bids.csv")
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(email$email))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation) 
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm.sparse = removeSparseTerms(dtm, 0.97)

email.df = as.data.frame(as.matrix(dtm.sparse) )
names(email.df) = paste("w_", names(email.df),  sep ='')
email.df$responsive = email$responsive

## Part 1
str(dtm.sparse)

sort(colSums(email.df[,1:ncol(email.df)-1]),decreasing = TRUE)[1:5] 

sum(email$responsive) / nrow(email)

## Part 2
set.seed(144)
spl = sample.split(email.df$responsive, 0.7)
email.train = subset(email.df, spl == TRUE)
email.test = subset(email.df, spl == FALSE)

email.rpart = rpart( responsive ~ ., data = email.train, method = "class")
prp(email.rpart)
email.predict = predict(email.rpart, newdata = email.test, method = "class")

email.predict = email.predict[,2]
confMat = table(email.test$responsive, email.predict > 0.5)
confMat
accuracy_rf = sum(diag(confMat))/nrow(email.test)
accuracy_rf

library(ROCR)
pred = prediction(email.predict, email.test$responsive)
AUC = as.numeric( performance(pred, "auc")@y.values)
AUC

## Part 3
email.train$responsive = as.factor(email.train$responsive)
email.test$responsive = as.factor(email.test$responsive)
set.seed(100)
email.rf = randomForest(responsive ~ . , data = email.train)
email.predict = predict(email.rf, newdata = email.test, type = "prob")

varImpPlot(email.rf, type = 2)

confMat = table(email.test$responsive, email.predict[,2] >= 0.5)
confMat

accuracy_rf = sum(diag(confMat))/nrow(email.test)
accuracy_rf

pred = prediction(email.predict[,2], email.test$responsive)
AUC = as.numeric( performance(pred, "auc")@y.values)
AUC

confMat[2,2] / sum(confMat[,2])
confMat[2,2] / sum(confMat[2,])


## Part 4
email.glm = glm( responsive ~ ., data = email.train, family = "binomial")

email.predict = predict(email.glm, type = "response")
confMat = table(email.train$responsive, email.predict>0.5)
accuracy_glm = sum(diag(confMat))/nrow(email.train)
accuracy_glm

email.predict = predict(email.glm, newdata = email.test, type = "response")
confMat = table(email.test$responsive, email.predict >= 0.5)
accuracy_glm = sum(diag(confMat))/nrow(email.test)
accuracy_glm

pred = prediction(email.predict, email.test$responsive)
AUC = as.numeric( performance(pred, "auc")@y.values)
AUC

## Part 5
library(glmnet)
library(glmnetUtils)

set.seed(55)
email.glmnet.cv = cv.glmnet( responsive ~., data = email.train, 
                             family = "binomial", nfolds = 5)
email.glmnet.cv$lambda.min

coefficients(email.glmnet.cv, s = "lambda.min")


email.predict = predict(email.glmnet.cv, newdata = email.test, type = "response")

confMat = table(email.test$responsive, email.predict >= 0.5)
accuracy_glmnet = sum(diag(confMat))/nrow(email.test)
accuracy_glmnet

pred = prediction(email.predict, email.test$responsive)
AUC = as.numeric( performance(pred, "auc")@y.values)
AUC

email.predict = predict(email.glmnet.cv, newdata = email.test, type = "response", s = "lambda.min")

confMat = table(email.test$responsive, email.predict >= 0.5)
accuracy_glmnet = sum(diag(confMat))/nrow(email.test)
accuracy_glmnet

pred = prediction(email.predict, email.test$responsive)
AUC = as.numeric( performance(pred, "auc")@y.values)
AUC
