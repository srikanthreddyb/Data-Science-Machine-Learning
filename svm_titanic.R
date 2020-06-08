data1 = read.csv("C:/Users/phsivale/Documents/Trainings/titanic.csv",
                 na.strings=c(""," ","NA","?","  "))
names(data1)
colsToUse = c('pclass','survived','sex','age','fare',
              'sibsp','parch','embarked')
data1 = data1[,colsToUse]

str(data1)
data1$pclass = as.factor(data1$pclass)
data1$survived = as.factor(data1$survived)

summary(data1)
###
# install.packages('DMwR')
library(DMwR)
data2 = knnImputation(data = data1,k=5)
summary(data2)
sum(is.na(data2))
summary(data1)

# data2$pclass = as.factor(data2$pclass)
data2$sex = as.factor(data2$sex)
data2$survived = as.factor(data2$survived)

summary(data2)

#### Handling Sibsp and Parch

table(data1$sibsp)

# data2$sibsp_cat = ifelse(data2$sibsp > 2,'>2',data2$sibsp)
# data2$sibsp_cat = as.factor(data2$sibsp_cat)
# 
# table(data1$parch)
# data2$parch_cat = ifelse(data2$parch > 1,'>1',data2$parch)
# data2$parch_cat = as.factor(data2$parch_cat)
summary(data2)

# ### Dropping sibsp and parch
# data2$sibsp = NULL
# data2$parch = NULL

summary(data2)

# data2$pclass = as.factor(data2$pclass)
hist(data2$fare)
quantile(data2$fare,1)

data2$fare[data2$fare > quantile(data2$fare,0.99)] = quantile(data2$fare,0.99)
data2$fare = sqrt(data2$fare)
summary(data2)

##### train test split
data2$survived = as.factor(data2$survived)
set.seed(567)
rows = 1:nrow(data2)
trainRows = sample(rows,round(0.7*nrow(data2)))

trainData = data2[trainRows,]
testData = data2[-trainRows,]

prop.table(table(trainData$survived))
prop.table(table(testData$survived))

library(e1071)
svmmodel = svm(survived~.,data=trainData,
               kernel='poly', cost=10, degree=4)
preds = predict(svmmodel,testData)
table(testData$survived, preds, dnn=c('Actuals','Preds'))

preds = predict(svmmodel,trainData)
table(trainData$survived, preds, dnn=c('Actuals','Preds'))

