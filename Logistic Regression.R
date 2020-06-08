# Read the data
dataset = read.csv('diabetes.csv')

# Exploratory Data Analysis
# Understand the structure of the data
str(dataset)
head(dataset)
tail(dataset)
summary(dataset)

# Histogram of age
library(ggplot2)
hist(dataset$Age)
# We can do more of such univariate & bivariate analysis

# Objective Bivariate Analysis (Correlation)
sub_dataset = dataset[-9]
corrmat = cor(sub_dataset)
library(corrplot)
corrplot.mixed(corrmat)
install.packages('ggcorrplot')
ggcorrplot::ggcorrplot(corrmat)
# No strong correlations observed, no need to drop anything

# Split data into training and test sets
library(caTools)
split = sample.split(dataset$Outcome, SplitRatio = 0.75)
train_data = subset(dataset, split == TRUE)
test_data = subset(dataset, split == FALSE)

# Is there class imbalance?
table(dataset$Outcome)
# Class Imbalance. Minimum accuracy required (65.1%)
500/768

# First version of the model
classifier1 = glm(Outcome ~ ., data = train_data, family = 'binomial')
summary(classifier1)
prob_pred1 = predict(classifier1, type = 'response', newdata = test_data)


# Lets use 0.5 as the threshold to start with
y_pred1 = ifelse(prob_pred1 > 0.5, 1, 0)
cm = table(test_data[,9], y_pred1)
Accuracy = (108+37)/192 # 75.52%
Precision = TP/(TP + FP) = 37/(37 + 17) = 0.685
Recall = TP/(TP + FN) = 37/(37 + 30) = 0.5522
(2*0.685*0.5522)/(0.685 + 0.5522) # F-score (0.6114)

# Lets use 0.7 as the threshold to start with
y_pred1 = ifelse(prob_pred1 > 0.7, 1, 0)
cm = table(test_data[,9], y_pred1)
Accuracy = (119+27)/192 # 76.04%
Precision = TP/(TP + FP) = 27/(27 + 6) = 0.818
Recall = TP/(TP + FN) = 27/(27 + 40) = 0.4029
(2*0.818*0.4029)/(0.818 + 0.4029) # F-score (0.5398)

# Lets use 0.2 as the threshold to start with
y_pred1 = ifelse(prob_pred1 > 0.2, 1, 0)
cm = table(test_data[,9], y_pred1)
Accuracy = (54+58)/192 # 58.33%
Precision = TP/(TP + FP) = 58/(58 + 61) = 0.487
Recall = TP/(TP + FN) = 58/(58 + 9) = 0.8656
(2*0.487*0.8656)/(0.487 + 0.8656) # F-score (0.6233)

# Lets identify the appropriate threshold using the ROC curve
install.packages('ROCR')
library(ROCR)
ROCRpred = prediction(predictions = prob_pred1, 
                      labels = test_data$Outcome)
ROCRperf = performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), 
     text.adj = c(-0.2, 1.7))
abline(a = 0, b = 1)

auc_test = round(as.numeric(performance(ROCRpred, 'auc')@y.values), 2)

# Cut-offs can be chosen based on the cost-benefit analysis, 
# diagonal distance measure or Youden Index

# Lets select 0.4 as the cutoff
y_pred1 = ifelse(prob_pred1 > 0.4, 1, 0)
cm = table(test_data[,9], y_pred1)
Accuracy = (103+41)/192 # 75%
Precision = TP/(TP + FP) = 41/(41 + 22) = 0.65
Recall = TP/(TP + FN) = 41/(41 + 26) = 0.611
(2*0.65*0.611)/(0.65 + 0.611) # F-score (0.6298)

# Ways to deal with data imbalance - Undersampling, Oversampling, SMOTE (part of the unbalanced library)
