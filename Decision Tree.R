#Program : DecisionTrees (C5.0 and CART)
## Analyticspath 
## CART 
library(ISLR)
data("Default")
head(Default)
library(rpart)
tree1 = rpart(default ~ . , data = Default)
table(Default$default)
### Libraries to generate fancyrapart plot 
library(rpart)				    # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)

fancyRpartPlot(tree1)

setwd("D:/AP/baging and  RF")
chrn = read.csv("churn.csv")
prop.table(table(chrn$Churn))
names(chrn)
## remove the variables state, Area.Code, Phone
chrn = chrn[,-c(19:21)]

chrn$Churn = as.factor(chrn$Churn)
set.seed(1234)
ids = sample(nrow(chrn), nrow(chrn)*0.8)
train = chrn[ids,]
test = chrn[-ids,]
### rpart 
library(rpart)
chrntree = rpart(Churn ~ . , data=train, method="class")

fancyRpartPlot(chrntree)

### Accuracy on test 

test$pred = predict(chrntree, newdata = test, type = "class")
table(test$Churn, test$pred)

Accuracy = (67+557)/667

precison = 67/(67+11)
recall = 67/(67+32)

2*precison*recall/(precison+recall)

### Accuracy on training dataset 

pred = predict(chrntree, newdata = train, type = "class")
table(train$Churn, pred)

p = 287/(287+23)
r = 287/(287+97)

2*p*r/(p+r)

(287+2259)/2666

### overfitting 
## pruning the tree 
printcp(chrntree)

chrnprune = prune(chrntree, cp = 0.0131)
fancyRpartPlot(chrnprune)

### performance of pruned tree 
test$pred = predict(chrnprune, newdata = test, type="class")
table(test$Churn, test$pred)

pred = predict(chrnprune, newdata = train, type = "class")
table(train$Churn, pred)

### pruning further 
printcp(chrntree)


chrnprune2 = prune(chrntree, cp = 0.0183)
fancyRpartPlot(chrnprune2)

### performance of pruned tree 

test$pred = predict(chrnprune2, newdata = test, type = "class")

table(test$Churn, test$pred)

### C5.0 

library(C50)
#install.packages("C50")

target = chrn$Churn

chrn$Churn = NULL

### Train and test datasets 

set.seed(1234)

ids = sample(nrow(chrn), nrow(chrn)*0.8)

train_x = chrn[ids,]
train_y = target[ids]

test_x = chrn[-ids,]
test_y = target[-ids]

train_y = as.factor(train_y)
test_y = as.factor(test_y)
## MOdel training

c5model = C5.0(x = train_x, y = train_y)
c5model

## Model perfromance 
pred = predict(c5model, newdata = test_x)
table(test_y, pred)

plot(c5model)

## regression using decision trees 

library(Ecdat)

data("Computers")
head(Computers)


### cart for regression 

ids = sample(nrow(Computers), nrow(Computers)*0.8)

train = Computers[ids,]
test = Computers[-ids,]

### Model trianing 

regtree = rpart(price ~ . , data=train, method = "anova")
fancyRpartPlot(regtree)

## Insurance data 

setwd("D:\\AP\\linear")
ins = read.csv("insurance.csv")

head(ins)

regins  = rpart(charges ~ . , data = ins, method = "anova")

fancyRpartPlot(regins)


## Bagging and Randomforest 

library(randomForest)

train$Churn = as.factor(train$Churn)

rftree = randomForest(Churn ~ . , data = train, ntree = 20, mtry = 5, max_depth = 12, maxnodes = 40 , classwt = c(0.8, 0.2)) # , mtry = 6 )
?randomForest
### Performance on the test dataset 

pred = predict(rftree, newdata = test)

table(test$Churn, pred)

67/(67+7)
67/(67+32)
2*0.9*0.68/(0.9+0.68)

varImpPlot(rftree)
