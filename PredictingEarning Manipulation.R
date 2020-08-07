#Jigyasa Sachdeva
#Predicting Earning Manipulation of Coorporate Firms


library(readxl)
full_data <- read_excel("Desktop/Second\ Sem/Data\ Mining/Assignments/Assignment\ 5/IMB579-XLS-ENG_mscore.xlsx ")
full_data <- IMB579_XLS_ENG_mscore
View(full_data)

str(full_data)
full_data$`C-MANIPULATOR` <- as.factor(full_data$`C-MANIPULATOR`)
fulldata <- subset(full_data, select = -c(`Company ID`, Manipulater))
rm(full_data)

library(dplyr)
fulldata <- fulldata %>% rename(target = `C-MANIPULATOR` )

library(funModeling)
df_status(fulldata)
#The data is clean with no null values 
#The proportion of zero's in LEVI variable is 0.08% and in SGAI is 0.32%
#The proportion of non-manipulators in the target is 96.85%: Indicating unbalanced data

summary(fulldata)




#Univariate for Outliers :

hist(fulldata$DSRI)
f <- which(fulldata$DSRI>10)
f1 <- fulldata[f,]
f1
#Can be hypothesised that higher DSRI values may be manipulators (3)

hist(fulldata$GMI)
g <- which(fulldata$GMI>5 | fulldata$GMI< -10)
g1 <- fulldata[g,]
#Extreme high value has manipulator record (1)

hist(fulldata$AQI)
a <- which(fulldata$AQI>20 | fulldata$AQI< -20)
a1 <- fulldata[a,]
#Extreme high value has manipulator record (1)

hist(fulldata$SGI)
s <- which(fulldata$SGI>5)
s1 <- fulldata[s,]
#Extreme high values are manipulator records (2)

hist(fulldata$SGAI)
sg <- which(fulldata$SGAI>10)
sg1 <- fulldata[sg,]
#Extreme high values are manipulator records (2)

hist(fulldata$LEVI)
l <- which(fulldata$LEVI>5)
l1 <- fulldata[l,]
#Extreme high values are manipulator records (2/3)




#Bivariate Analysis for Correlation :

fulldata_c <- subset(fulldata, select = -c(target)) 
res <- cor(fulldata_c)
round(res, 2)
library(corrplot)
corrplot(res, method="circle", type="upper", order = "hclust")

#Checking for DSRI and SGAI
cor(fulldata$DSRI, fulldata$SGAI)
#0.470764

#Evaluating M-Score

m_score <- (-4.84) + (0.92*fulldata$DSRI) + (0.528*fulldata$GMI) + (0.404*fulldata$AQI) + (0.892*fulldata$SGI) + (0.115*fulldata$DEPI) - (0.172*fulldata$SGAI) + (4.679*fulldata$ACCR) - (0.327*fulldata$LEVI)
table(m_score> -2.22)
#FALSE  TRUE 
#829   410 

m <- rep(NA, nrow(fulldata))
m[m_score>-2.22] <- 1
m[m_score<=-2.22] <- 0
table(m)
m <- as.factor(m)




#Checking whether M-Score is a good predictor for identifying earning manipulations by an industry

library(caret)
confusionMatrix(m, fulldata$target,  positive= '1')
#             Reference
#Prediction   0   1
#         0 829   0
#         1 371  39




library(readxl)
sample_data <- read_excel("Desktop/IMB579-XLS-ENG.xlsx", sheet = "Sample for Model Development")
View(sample_data)  
sampledata <- subset(sample_data, select = -c(`Company ID`, Manipulator))
rm(sample_data)

View(sampledata) 
sampledata$`C-MANIPULATOR` <- as.factor(sampledata$`C-MANIPULATOR`)
sampledata <- sampledata %>% rename(target = `C-MANIPULATOR` )




#Undersampling

set.seed(12345)
index <- sample(2, nrow(sampledata), replace = T, prob = c(0.7,0.3))
TrainData <- sampledata[index == 1, ]
TestData <- sampledata[index == 2, ]

prop.table(table(TrainData$target))
#0        1 
#0.815603 0.184397

prop.table(table(TestData$target))
#0        1 
#0.835443 0.164557 

library(ROSE)
under <- ovun.sample(target~., data=TrainData, p=0.5, seed=123, method="under")$data

table(under$target)
#0  1 
#25 26 




#Step-Wise Logistic Regression
full <- glm(target~., data= under, family= "binomial")
null <- glm(target~1, data= under, family= "binomial")
stepf <- step(null, scope= list(lower= null, upper= full), direction = "both")
summary(stepf)

Pred <- predict(stepf, newdata= TestData, type="response")
Pred
Class <- ifelse(Pred >= 0.5, '1', "0")
Class <- as.factor(Class)

confusionMatrix(Class, TestData$target, positive = '1')




#Multiple sample:
library(ROSE)
under1 <- ovun.sample(target~., data=TrainData,
                     p=0.5, seed=123,
                     method="under")$data
table(under1$target)
#0  1 
#28 26

full1 <- glm(target~., data= under1, family= "binomial")
null1 <- glm(target~1, data= under1, family= "binomial")
stepf1 <- step(null1, scope= list(lower= null1, upper= full1), direction = "both")
summary(stepf1)

library(caret)
Pred <- predict(stepf, newdata= TestData, type="response")
Class <- ifelse(Pred >= 0.5, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')
#         Reference
#Prediction  0  1
#         0 46  1
#         1 20 12





#Different cut-offs
#Using F-Score

s <- seq(from = 0, to= 1, by = 0.1)
fun = function(s)
  {
  Class <- ifelse(Pred >= s, '1', '0')
  Class <- as.factor(Class)
  c_recall <- confusionMatrix(Class, TestData$target, positive = '1')$byClass['Recall']
  c_precision <- confusionMatrix(Class, TestData$target, positive = '1')$byClass['Precision']
  f_score <- (2.5*c_recall*c_precision)/ (1.5*c_precision + c_recall)
  return(f_score)
  }

which.max(lapply(s, fun))  #9
s[9]
#Cut off: 0.8

Pred <- predict(stepf, newdata= TestData, type="response")
Class <- ifelse(Pred >= 0.8, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')
#           Reference
#Prediction  0  1
#         0 55  3
#         1 11 10


s <- seq(from = 0, to= 1, by = 0.1)
fun = function(s)
{
  Class <- ifelse(Pred >= s, '1', '0')
  Class <- as.factor(Class)
  return(confusionMatrix(Class, TestData$target, positive = '1')$byClass)
}

lapply(s, fun)




#Youdan's Index

s <- seq(from = 0, to= 1, by = 0.001)

fun = function(s)
{
  Class <- ifelse(Pred >= s, '1', "0")
  Class <- as.factor(Class)
  i <- confusionMatrix(Class, TestData$target, positive = '1')$byClass['Sensitivity']
  j <- confusionMatrix(Class, TestData$target, positive = '1')$byClass['Specificity']
  return(i+j-1)
}

which.max(lapply(s, fun))
s[776]
#0.775


Pred <- predict(stepf, newdata= TestData, type="response")
Class <- ifelse(Pred >= 0.775, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')

#           Reference
#Prediction  0  1
#         0 52  3
#         1 14 10





#Cost- based function

#False negative having more penalty than False Positive
p1=7
p2=3
s <- seq(from = 0, to= 1, by = 0.001)
fun = function(s)
{
  Class <- ifelse(Pred >= s, '1', "0")
  Class <- as.factor(Class)
  #p10
  false_negative <- confusionMatrix(Class, TestData$target, positive = '1')$table[1,2]
  #p01
  false_positive <- confusionMatrix(Class, TestData$target, positive = '1')$table[2,1]
  return((p1*false_negative + p2*false_positive))
}

which.min(lapply(s, fun))
s[951]
0.95

Class <- ifelse(Pred >= 0.95, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')

#           Reference
#Prediction  0  1
#         0 57  3
#         1  9 10





#Final Model  -> Cost based method

full1 <- glm(target~., data= under1, family= "binomial")
null1 <- glm(target~1, data= under1, family= "binomial")
stepf1 <- step(null1, scope= list(lower= null1, upper= full1), direction = "both")
summary(stepf1)

Pred <- predict(stepf, newdata= TestData, type="response")
Class <- ifelse(Pred >= 0.95, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')






#Tree

#With undersampled data

library(rpart)
library(caret)

r <- rpart(target~., data= under,control = rpart.control(minsplit =10, cp = 0.01))
r
rpart.plot(r)
#ACCR, DSRI, AQI, LEVI

p <- predict(r, newdata = TestData, type = "class")
p <- as.factor(p)
confusionMatrix(p, TestData$target, positive = '1')
#           Reference
#Prediction  0  1
#         0 43  1
#         1 23 12

#Since decision tree is prone to variance, we cannot decide rules base on 1 sample

library(ROSE)
under1 <- ovun.sample(target~., data=TrainData,
                      p=0.5, seed=1,
                      method="under")$data
r1 <- rpart(target~., data= under1, control = rpart.control(minsplit =10, cp = 0.01))
r1
rpart.plot(r1)
#ACCR, SGI, DSRI, DEPI

p <- predict(r1, newdata = TestData, type = "class")
p <- as.factor(p)
confusionMatrix(p, TestData$target, positive = '1')
#           Reference
#Prediction  0  1
#         0 49  4
#         1 17  9


#These give different results: 
#ACCR, DSRI are important


#With unbalanced data:

r2 <- rpart(target~., data= TrainData, control = rpart.control(minsplit =10, cp = 0.01))
r2
rpart.plot(r2)

p <- predict(r2, newdata = TestData, type = "class")
p <- as.factor(p)
confusionMatrix(p, TestData$target, positive = '1')
#           Reference
#Prediction  0  1
#         0 58  4
#         1  8  9





#Logistic regression

str(fulldata)

index <- sample(2, nrow(fulldata), replace = T, prob = c(0.7,0.3))
TrainData <- fulldata[index == 1, ]
TestData <- fulldata[index == 2, ]

prop.table(table(TrainData$target))
#0          1 
#0.96937574 0.03062426 
prop.table(table(TestData$target))
#0          1 
#0.96666667 0.03333333 


#Unbalanced data
full <- glm(target~., data= TrainData, family= "binomial")
null <- glm(target~1, data= TrainData, family= "binomial")
stepf <- step(null, scope= list(lower= null, upper= full), direction = "both")
summary(stepf)

Pred <- predict(stepf, newdata= TestData, type = "response")
Pred
Class <- ifelse(Pred >= 0.5, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')


#Cost based
p1=7
p2=3
s <- seq(from = 0, to= 1, by = 0.01)
fun = function(s)
{
  Class <- ifelse(Pred >= s, '1', "0")
  Class <- as.factor(Class)
  #p10
  false_negative <- confusionMatrix(Class, TestData$target, positive = '1')$table[1,2]
  #p01
  false_positive <- confusionMatrix(Class, TestData$target, positive = '1')$table[2,1]
  return((p1*false_negative + p2*false_positive))
}

which.min(lapply(s, fun))
s[47]
#0.46


Pred <- predict(stepf, newdata= TestData, type = "response")
Pred
Class <- ifelse(Pred >= 0.46, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')

#           Reference
#Prediction   0   1
#         0 374  10
#         1   3   3

full1 <- glm(target~., data= under1, family= "binomial")
null1 <- glm(target~1, data= under1, family= "binomial")
stepf1 <- step(null1, scope= list(lower= null1, upper= full1), direction = "both")
summary(stepf1)

Pred <- predict(stepf1, newdata= TestData, type="response")
Class <- ifelse(Pred >= 0.95, '1', "0")
Class <- as.factor(Class)
confusionMatrix(Class, TestData$target, positive = '1')
#           Reference
#Prediction   0   1
#         0 357   6
#         1  20   7





#Random Forest

#Unbalanced
library(randomForest)
fit = randomForest(target~., data=fulldata, ntree=500,
                   importance=TRUE, proximity=TRUE)
#OOB estimate of  error rate: 2.91%
#Confusion matrix:
#     0 1 class.error
#0 1197 3   0.0025000
#1   33 6   0.8461538

importance(fit)
plot(fit)
#Graph: less trees


library(randomForest)
fit = randomForest(target~., data=fulldata, ntree=8,
                   importance=TRUE, proximity=TRUE)
#OOB estimate of  error rate: 2.86%
#Confusion matrix:
#     0  1 class.error
#0 1178  7 0.005907173
#1   28 10 0.736842105

plot(fit)




#Ada boost 
index <- sample(2, nrow(fulldata), replace = T, prob = c(0.7,0.3))
TrainData <- fulldata[index == 1, ]
TestData <- fulldata[index == 2, ]
install.packages("adabag")
library("adabag")

under <- ovun.sample(target~., data=TrainData, p=0.5, seed=123, method="under")$data
b <- boosting(target ~ ., data = under, mfinal = 100, control = rpart.control(maxdepth = 4))

b$trees
b$weights
b$trees[[34]]
b$class
b$class <- as.factor(b$class)

table(b$class, under$target, dnn = c("Predicted Class", "Observed Class"))
#               Observed Class
# Predicted Class  0  1
#               0 24  0
#               1  0 24

pred <- predict.boosting(b, newdata = as.data.frame(TestData))

#               Observed Class
#Predicted Class   0   1
#              0 278   5
#              1  52  10


cost <- 7*5 + 3*52
cost  #191



train <- c(sample(1:50, 25), sample(51:894, 55))
train
b <- boosting(target ~ ., data = TrainData[train,], mfinal = 100, control = rpart.control(maxdepth = 4))


