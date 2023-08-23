# Predictive analysis of the loan approval dataset
# We will build a predictive model to help our company to improve the efficiency and accuracy of home loan approval.

# Dataset Description:
# Variable	Description
# Loan_ID	Unique Loan ID
# Gender	Male/ Female
# Married	Applicant married (Y/N)
# Dependents	Number of dependents
# Education	Applicant Education (Graduate/ Under Graduate)
# Self_Employed	Self employed (Y/N)
# ApplicantIncome	Applicant income
# CoapplicantIncome	Coapplicant income
# LoanAmount	Loan amount in thousands
# Loan_Amount_Term	Term of loan in months
# Credit_History	credit history meets guidelines
# Property_Area	Urban/ Semi Urban/ Rural
# Loan_Status	Loan approved (Y/N)

# install.packages("partition")
# install.packages("caret")
# install.packages("caTools")
library(caTools)
library(GGally)
library(car)
# library(splitTools)
# library(partition)
# library(caret)

setwd("~/Desktop/MBA/Analytics for Managers/final_project")
loan <- read.csv("loan_approval_dataset.csv")
View(loan)
# summary(loan)

# Create a scatterplot matrix using ggpairs function
# ggpairs(loan)

# Create a train-validation-test split
set.seed(3451)
# inds <- partition(
#     loan$loan_status.Length,
#     p = c(train = 0.6, valid = 0.3, test = 0.1))
# str(inds)
split1 <- sample.split(loan$loan_status, SplitRatio = 0.7)
train_data <- loan[split1, ]
temp_data <- loan[!split1, ]

# Splitting the temporary set into validation (15%) and test (15%)
split2 <- sample.split(temp_data$loan_status, SplitRatio = 0.5)
validation_data <- temp_data[split2, ]
test_data <- temp_data[!split2, ]

categorical_columns <- c("education", "self_employed")
numerical_columns <- c(
    "no_of_dependents", "income_annum", "loan_amount",
    "loan_term", "cibil_score", "residential_assets_value",
    "commercial_assets_value", "luxury_assets_value",
    "bank_asset_value")

# numerical_columns <- c(
#     "no_of_dependents", "income_annum", "loan_amount",
#     "loan_term", "cibil_score", "residential_asset_value",
#     "commercial_assets_value", "luxury_assets_value",
#     "bank_asset_value"
# )


# visualize the training set and see what we can find between features and the target variable
# Create pairwise histogram
# categorical_cols_to_visualize <- c(c("loan_status"), categorical_columns)
# categorical_var_plot <- ggpairs(train_data, columns = categorical_cols_to_visualize)
# ggsave("cat_var_hist.png", categorical_var_plot, 'png', './')

numerical_cols_to_visualize <- c(c("loan_status"), numerical_columns)
num_var_plot <- ggpairs(train_data, columns = numerical_cols_to_visualize)
num_var_plot
ggsave("num_var_plot.png", num_var_plot, 'png', './')
# train_data$numerical_columns
