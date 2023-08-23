# Predictive analysis of the loan approval dataset
# We will build a predictive model to help our company to improve the efficiency and accuracy of home loan approval. 

# install.packages("partition")
# install.packages("caret")
# install.packages("caTools")
# install.packages("dummies")
library(dummies)
library(caTools)
library(GGally)
library(car)
library(ggplot2)
library(dplyr)
library(tidyverse)
# library(splitTools)
# library(partition)
# library(caret)

#Clear all objects in the workspace
rm(list=ls())
setwd("~/Documents/am")
loan <- read.csv("loan_approval_dataset.csv")
View(loan)
summary(loan)
loan <- subset(loan, select = -loan_id)

# preprocess categorical data to dummy variables
data %>%
    mutate(dummy = 1) %>%
    spread(key = Reporting_Airline, value = dummy, fill = 0) %>%
    slice(1:5)

# Create a scatterplot matrix using ggpairs function
ggpairs(loan)

# Create a train-validation-test split
set.seed(3451)
# inds <- partition(
#     loan$loan_status.Length,
#     p = c(train = 0.6, valid = 0.3, test = 0.1))
# str(inds)
split1 <- sample.split(loan$loan_status, SplitRatio = 0.7)
train_data <- iris[split1, ]
temp_data <- iris[!split1, ]

summary(train_data)

# Splitting the temporary set into validation (15%) and test (15%)
split2 <- sample.split(temp_data$loan_status, SplitRatio = 0.5)
validation_data <- temp_data[split2, ]
test_data <- temp_data[!split2, ]

## Load necessary libraries
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)


#Dropping Loan ID
loan <- loan %>% select(-loan_id)
str(loan)

# List of numerical columns to plot
num_cols <- c('income_annum', 'loan_amount', 'loan_term', 'cibil_score',
              'residential_assets_value', 'commercial_assets_value',
              'luxury_assets_value', 'bank_asset_value')

# List of categorical columns to plot
cat_cols <- c('education', 'self_employed', 'loan_status')
View(loan)

# Create histograms using ggplot2
plot_list <- lapply(num_cols, function(col) {
  ggplot(data = loan, aes(x = .data[[col]])) +
    geom_histogram(binwidth = 20, color = "black", fill = "lightblue") +
    labs(title = paste("Distribution of", col))
})

# Arranging and displaying plots
grid.arrange(grobs = plot_list, ncol = 2)

# Label Encoding
loan$education <- ifelse(loan$education == ' Not Graduate', 0, 1)
loan$self_employed <- ifelse(loan$self_employed == ' No', 0, 1)
loan$loan_status <- ifelse(loan$loan_status == ' Rejected', 0, 1)

# visualize the training set and see what we can find between features and the target variable
# Create pairwise histogram
categorical_cols_to_visualize <- c(c("loan_status"), categorical_columns)
categorical_var_plot <- ggpairs(train_data, columns = categorical_cols_to_visualize)
# ggsave("cat_var_hist.png", categorical_var_plot, 'png', './')

numerical_cols_to_visualize <- c(c("loan_status"), numerical_columns)
num_var_plot <- ggpairs(train_data, columns = numerical_cols_to_visualize)
# num_var_plot
ggsave("num_var_plot.png", num_var_plot, 'png', './')
# train_data$numerical_columns

