
setwd("D:\\Data Science - Course\\UC1")

## remove the list if already exist 
rm(lista)

### read the all state purchase file 
allstate  = read.csv("Purchase.csv")

head(allstate)
## check the proportions of target variable 
prop.table(table(allstate$QuoteConversion_Flag))


str(allstate)

dim(allstate)

prop.table(table(allstate$QuoteConversion_Flag))*100




###  COnvert the date(string) variable into a date to extract useful information 

require(lubridate)

allstate$Original_Quote_Date = as.Date(allstate$Original_Quote_Date)

allstate$quote_mon = month(allstate$Original_Quote_Date)
allstate$quote_year = year(allstate$Original_Quote_Date)

allstate$weekday = weekdays(allstate$Original_Quote_Date)

allstate$weeknum = week(allstate$Original_Quote_Date)

table(allstate$quote_mon)

table( allstate$weekday)

table(allstate$quote_year)

#### delete the date and quote number 

allstate$Original_Quote_Date = NULL
allstate$QuoteNumber = NULL

 
## Perform exploratory data analysis 

library(ggplot2)

table(allstate$quote_year)

names(allstate)

allstate$QuoteConversion_Flag = as.factor(allstate$QuoteConversion_Flag)

ggplot(allstate,  aes( Field6, fill=QuoteConversion_Flag)) + geom_bar()

ggplot(allstate,  aes( as.factor(quote_mon), fill=QuoteConversion_Flag)) + geom_bar()

ggplot(allstate,  aes( as.factor(quote_mon), fill=QuoteConversion_Flag)) + geom_bar(position = "fill")


#### Identify all factor variables to recode them to numeric
lista = ''
 
for ( i in  1:ncol(allstate)) {
   
   if (class(allstate[,i]) == "factor") { 
     
     if (i ==1) { 
     lista = i
     } else lista = c(lista, i)  
   }
 }


listb = '' 

for ( i in 1:ncol(allstate)){ 
  
  if  (class(allstate[,i]) == "factor"){
    listb = c(listb, i)
    allstate[,i] = as.integer(allstate[,i])
  }
}

listb

for ( i in 1:ncol(allstate)){ 
  
  if  (class(allstate[,i]) == "factor"){
    listb = c(listb, i)
    #allstate[,i] = as.integer(allstate[,i])
  }
}

## print the list to identify the factor variables 
listb

str(allstate)

table(allstate[,2])
table( allstate[,6])

table(allstate[,21])

### Recoding the character variables to numeric 
library(car)
library(stringr)
allstate[,2] = recode( allstate[,2], "'A' = 1;'B' = 2; 'C' = 3; 'D' = 4; 'E'=5; 'F' = 6; 'J' = 7; 'K' = 8 " )
#allstate[,4] = recode( allstate[,4], "'A' = 1;'B' = 2; 'C' = 3; 'D' = 4; 'E'=5; 'F' = 6; 'J' = 7; 'K' = 8 " )
allstate[,6] = str_replace( allstate[,6], ",", "")
allstate[,6] = as.numeric(allstate[,6])
allstate[,8] = ifelse(allstate[,8] == "Y", 1,0)
allstate[,8] = as.numeric(allstate[,8])
allstate[,21] = recode( allstate[,21], "'T' = 1;'U' = 2; 'V' = 3; 'W'=4; 'X' = 5; 'Y' = 6; 'Z' = 7 " )
allstate[,22] = recode( allstate[,22], "'A' = 1;'B' = 2; 'C' = 3; 'E'=4; 'F' = 5; 'G' = 6; 'H' = 7 ;
                      'I' = 8;'J' = 9; 'K' = 10; 'L'=11 " )

allstate[,33] = recode( allstate[,33], "'K' = 1;'M' = 2; 'P' = 3; 'Q'=4; 'R' = 5; 'T' = 6; 'V' = 7 " )

allstate[,48] = recode( allstate[,48], "'Y' = 1;'N' = 2; '' = 3") 
allstate[,129] = recode( allstate[,129], "'Y' = 1;'N' = 2; '' = 3")
allstate[,130] = recode( allstate[,130], "'Y' = 1;'N' = 2; '' = 3")
allstate[,131] = ifelse( allstate[ ,131] == "Y", 1, 0)

allstate[,133] = recode( allstate[,133], "'A' = 1;'B' = 2; 'C' = 3; 'E'=4; 'F' = 5; 'G' = 6; 'H' = 7 ;
                      'I' = 8;'J' = 9; 'K' = 10; 'L'=11 ; 'M' =12 ; 'N' = 13;
                        'O' = 14; 'P' = 15; 'Q' = 16; 'R' = 17; 'S' = 18" )

allstate[,141] = recode( allstate[,141], "'A' = 1;'B' = 2; 'C' = 3; 'D'=4")
allstate[,159] = recode( allstate[,159], "'A' = 1;'B' = 2; 'C' = 3; 'D'=4")
allstate[,161] = ifelse( allstate[,161] == "Y", 1, 0)
allstate[,162] = recode( allstate[,162], "'K' = 1;'M' = 2; 'N' = 3; 'O'=4")
allstate[,163] = recode( allstate[,163], "'Y' = 1;'N' = 2; '' = 3")
allstate[,164] = recode( allstate[,164], "'E' = 1;'F' = 2; 'G' = 3; 'H'=4")
allstate[,165] = recode( allstate[,165], "'Y' = 1;'N' = 2; '' = 3")
allstate[,167] = recode( allstate[,167], "'Y' = 1;'N' = 2; '' = 3")
allstate[,168] = ifelse( allstate[,168] == "Y", 1, 0)
allstate[,169] = recode( allstate[,169], "'Y' = 1;'N' = 2; '' = 3")
allstate[,296] = recode( allstate[,296], "'Y' = 1;'N' = 2; '' = 3")
allstate[,297] = recode(allstate[,296], "'CA' = 1;'IL' = 2; 'NJ' = 3; 'TX'=4" )
allstate.excl = allstate[,c(58:61)]

table(allstate.excl$PersonalField18)

table(allstate[,58])

allstate = allstate[, -c(58:61)]

table(allstate$Field10)

str(allstate)

for ( i in  1:ncol(allstate)) {
  
  if (class(allstate[,i]) == "factor") { 
    
   allstate[,i] = as.numeric(allstate[,i]) 
  }
}

class(allstate$Field10)

allstate$Field10 = as.numeric( allstate$Field10)
str(allstate)
### check for character data types

listb = '' 

for ( i in 1:ncol(allstate)){ 
  
  if  (class(allstate[,i]) == "character"){
    listb = c(listb, i)
  }
}

listb

### weekday variable 
table(allstate[,296])

allstate$weekday = recode(allstate$weekday, "'Sunday' = 1;'Monday' = 2; 'Tuesday' = 3; 'Wednesday'=4; 'Thursday' = 5; 'Friday'=6; 'Saturday'=7" )

allstate$weekday = as.numeric(allstate$weekday)

### Save the transformed file for future use 

write.csv(allstate, "transformedfile.csv", row.names=F)

## allstate = read.csv("transformedfile.csv")

#allstate = allstate[ , -c(1,2)]

allstate[is.na(allstate)] = -99

sum(is.na(allstate))


table(allstate[, 1])


prop.table(table(allstate[,1]))

table(allstate$weekday) 





allstate$QuoteConversion_Flag = ifelse(allstate$QuoteConversion_Flag == 2, 1,0)

set.seed(1238)
indx = sample(nrow(allstate), nrow(allstate)*0.75)
train = allstate[indx,]
test = allstate[-indx,]

### due to memory issues remove the allstate dataframe 
## in case it is required read it from the saved csv

library(xgboost)

rm(allstate)

str(train)

y = train$QuoteConversion_Flag

#y = ifelse( y == 1, 0, 1)
table(y)


library(xgboost)

?xgboost

xgb <- xgboost(data =  data.matrix(train[,-1]), 
               label = y, 
               eta = 0.01,
               max_depth = 20, 
               nround=500, 
               subsample = 0.5,
               colsample_bytree = 0.6,
               eval_metric = "error",
               objective = "binary:logistic",
               nthread = -1  
) 
  
table(test$QuoteConversion_Flag)
test$QuoteConversion_Flag = ifelse(test$QuoteConversion_Flag == 1, 1,0)


table(test$QuoteConversion_Flag)

table(test$QuoteConversion_Flag)

summary(train)
y_pred = predict(xgb, data.matrix(test[,-1]))

y_class = ifelse(y_pred >= 0.5, 1,0)

table(y_class)
table(test[,1], y_class)

table(test[,1])

P = 8520/(8520+1026)
R = 8520/(8520+3853)

f1 = 2*P*R/(P+R)
f1

2*0.89*0.69/(0.89+0.69)

(8419+51873)/( 8419+51873+1084+3813)

library(gmodels)

CrossTable(test[,1], y_class)

library(caret)
library(gmodels)
CrossTable(test[,1], y_class)

 confusionMatrix(test[,1], y_class)

8436/(8436+1114)
8436/(8436+3796)
2*0.88*0.69/(0.88+0.69)

(51920+8435)/(65189)

2*0.87*0.69/( 0.69+0.87)

### Add cross validation 


dtrain = xgb.DMatrix(train[,-1], label = y)
cv = xgb.cv(data = dtrain, nrounds = 100, nthread = -1, nfold = 3, metrics = list("error","auc"),
             max_depth = 20, eta = 0.1, objective = "binary:logistic")
print(cv)


