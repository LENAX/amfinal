# Predictive analysis of the loan approval dataset
# We will build a predictive model to help our company to improve the efficiency and accuracy of home loan approval.

# install.packages("partition")
# install.packages("caret")
# install.packages("caTools")
# install.packages("dummies")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("rattle")

library(dummies)
library(caTools)
library(GGally)
library(car)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(dplyr)


# Clear all objects in the workspace
rm(list = ls())
setwd("./")
loan <- read.csv("loan_approval_dataset.csv")
# View(loan)
# summary(loan)
loan <- subset(loan, select = -loan_id)

# Label Encoding
loan$education <- ifelse(loan$education == " Not Graduate", 0, 1)
loan$self_employed <- ifelse(loan$self_employed == " No", 0, 1)
loan$loan_status <- ifelse(loan$loan_status == " Rejected", 0, 1)

# Create a scatterplot matrix using ggpairs function
overview <- ggpairs(loan)
# ggsave("overview.png", overview, "png", "./")


# Create a train-validation-test split
set.seed(3451)
# inds <- partition(
#     loan$loan_status.Length,
#     p = c(train = 0.6, valid = 0.3, test = 0.1))
# str(inds)
split1 <- sample.split(loan$loan_status, SplitRatio = 0.7)
train_data <- loan[split1, ]
temp_data <- loan[!split1, ]

summary(train_data)

# Splitting the temporary set into validation (15%) and test (15%)
split2 <- sample.split(temp_data$loan_status, SplitRatio = 0.5)
validation_data <- temp_data[split2, ]
test_data <- temp_data[!split2, ]

# List of numerical columns to plot
numerical_columns <- c(
    "income_annum", "loan_amount", "loan_term", "cibil_score",
    "residential_assets_value", "commercial_assets_value",
    "luxury_assets_value", "bank_asset_value"
)

# List of categorical columns to plot
categorical_columns <- c("education", "self_employed", "loan_status")
# View(loan)

# visualize the training set and see what we can find between features and the target variable
# Create pairwise histogram
categorical_cols_to_visualize <- c(c("loan_status"), categorical_columns)
categorical_var_plot <- ggpairs(train_data, columns = categorical_cols_to_visualize)
# ggsave("cat_var_hist.png", categorical_var_plot, 'png', './')
# categorical_var_plot

numerical_cols_to_visualize <- c(c("loan_status"), numerical_columns)
num_var_plot <- ggpairs(train_data, columns = numerical_cols_to_visualize)
# num_var_plot
# ggsave("num_var_plot.png", num_var_plot, 'png', './')

# clean erroneous features
# residential_assets_value could not be negative
train_data <- train_data[train_data$residential_assets_value >= 0, ]

# Prepare X and Y for models
X_train <- subset(train_data, select = -loan_status)
Y_train <- train_data$loan_status

# dev data, we can tune hyperparameters
X_valid <- subset(validation_data, select = -loan_status)
Y_valid <- validation_data$loan_status

# test data, don't touch it till producing the final result
X_test <- subset(test_data, select = -loan_status)
Y_test <- test_data$loan_status

# check for nans
# which(colSums(is.na(X)) > 0)
# all(is.na(Y))

# Build a decision tree model
tree_model <- rpart(Y_train ~ ., data = X_train, method = "class")

# png(filename = "decision_tree.png", width = 800, height = 600)
rpart.plot(tree_model, main = "Decision Tree for House Loan")
# dev.off()

# Evaluate in-sample performance
in_sample_predictions <- predict(tree_model, newdata = X_train, type = "class")
table(Actual = Y_train, Predicted = in_sample_predictions)

# Evaluate out-of-sample performance on the dev set
out_sample_predictions <- predict(tree_model, newdata = X_valid, type = "class")

get_confusion_matrix <- function(model, X, Y) {
    Y_pred <- predict(model, newdata = X, type = "class")
    return(table(Actual = Y, Predicted = Y_pred))
}

accuracy <- function(confusion_matrix) {
    acc <- (confusion_matrix[1, 1] + confusion_matrix[2, 2]) / sum(confusion_matrix)
    return(acc)
}

# TP / (TP + FP)
precision <- function(confusion_matrix) {
    precision_ <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[1, 2])
    return(precision_)
}

# TP / (TP + FN)
recall <- function(confusion_matrix) {
    rec <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[2, 1])
    return(rec)
}

f1_score <- function(confusion_matrix) {
    f1_score_ <- 2 * ((precision(confusion_matrix) * recall(confusion_matrix)) / (precision(confusion_matrix) + recall(confusion_matrix)))
    return(f1_score_)
}

# calculate accuracy, precision, recall, and f1 score
get_performance_metrics <- function(confusion_matrix) {
    acc <- accuracy(confusion_matrix)
    prec <- precision(confusion_matrix)
    recall_ <- recall(confusion_matrix)
    f1_score_ <- f1_score(confusion_matrix)

    model_performance <- data.frame(
        accuracy = acc,
        precision = prec,
        recall = recall_,
        f1_score = f1_score_
    )

    return(model_performance)
}

# confusion_matrix <- table(Actual = Y_valid, Predicted = out_sample_predictions)
train_confusion_matrix <- get_confusion_matrix(tree_model, X_train, Y_train)
train_performance <- get_performance_metrics(train_confusion_matrix)

vld_confusion_matrix <- get_confusion_matrix(tree_model, X_valid, Y_valid)
vld_performance <- get_performance_metrics(vld_confusion_matrix)
# print(performance)
#    accuracy precision    recall  f1_score
# 1 0.9703125 0.9588378 0.9949749 0.9765721

# We can see the model achieve high out of sample accuracy easily

# Let's try to build a logistic regression model
# Let's start with one variable
# AIC: 1587.4, BIC: 1599.425
lm1 <- glm(Y_train ~ cibil_score, data = X_train, family = "binomial")
BIC(lm1)
summary(lm1)

# AIC: 1454.7, BIC: 1472.731
lm2 <- glm(Y_train ~ cibil_score + loan_term, data = X_train, family = "binomial")
BIC(lm2)
summary(lm2)

# AIC: 1455.6, BIC:  1479.63
lm3 <- glm(Y_train ~ cibil_score + loan_term + loan_amount, data = X_train, family = "binomial")
BIC(lm3)
summary(lm3)

# Best one!
# AIC: 1406.3, BIC(lm3): 1436.27
lm3 <- glm(Y_train ~ cibil_score + loan_term + loan_amount + income_annum, data = X_train, family = "binomial")
BIC(lm3)
summary(lm3)

lm4 <- glm(Y_train ~ cibil_score + loan_term + loan_amount + income_annum + bank_asset_value, data = X_train, family = "binomial")
BIC(lm4)
summary(lm4)

lm1_probs <- predict(lm1, newdata = X_valid, type = "response")
lm1_pred_classes <- ifelse(lm1_probs > 0.5, 1, 0)

cm_lm1 <- table(Actual = Y_valid, Predicted = lm1_pred_classes)
get_performance_metrics(cm_lm1)


lm2_probs <- predict(lm2, newdata = X_valid, type = "response")
lm2_pred_classes <- ifelse(lm2_probs > 0.5, 1, 0)
cm_lm2 <- table(Actual = Y_valid, Predicted = lm2_pred_classes)
get_performance_metrics(cm_lm2)


lm3_probs <- predict(lm3, newdata = X_valid, type = "response")
lm3_pred_classes <- ifelse(lm3_probs > 0.5, 1, 0)

#       Predicted
# Actual   0   1
#      0 218  24
#      1  20 378
cm_lm3 <- table(Actual = Y_valid, Predicted = lm3_pred_classes)
get_performance_metrics(cm_lm3)

lm4_probs <- predict(lm4, newdata = X_valid, type = "response")
lm4_pred_classes <- ifelse(lm4_probs > 0.5, 1, 0)

cm_lm4 <- table(Actual = Y_valid, Predicted = lm4_pred_classes)
get_performance_metrics(cm_lm4)