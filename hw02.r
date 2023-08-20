# Clear all objects in the workspace
rm(list=ls())

# Set the working directory to the specified path
setwd("C:/Users/dyang/Dropbox/6093/mine/hw")

# Load necessary libraries
#install.packages("GGally")
library(GGally)
library(car)

# Read the data from the CSV file
loan <- read.csv("hw02_loan.csv", stringsAsFactors=TRUE)

# Check the structure of the data
str(loan)

# Get the summary of the data
summary(loan)

# Convert bankruptcy and term variables to factors
loan$bankruptcy  <- as.factor(loan$bankruptcy)
loan$term <- as.factor(loan$term)

# View the first few rows of the dataset
head(loan)

# Create a scatterplot matrix using ggpairs function
ggpairs(loan)

# Build linear regression models
m1 <- lm(interest_rate ~ bankruptcy, data = loan)
summary(m1)
m2 <- lm(interest_rate ~ income_ver, data = loan)
summary(m2)

# Build more complex models with interaction terms
m3 <- lm(interest_rate ~ bankruptcy*debt_to_income, data = loan)
summary(m3)
m4 <- lm(interest_rate ~ bankruptcy+debt_to_income, data = loan)
summary(m4)

# Build models with different predictor variables
m5 <- lm(interest_rate ~ bankruptcy*credit_util, data = loan)
summary(m5)
m6 <- lm(interest_rate ~ bankruptcy+credit_util, data = loan)
summary(m6)

# Build a model with all predictor variables
m7 <- lm(interest_rate ~ ., data = loan)
summary(m7)

# Build a model without the issued variable
m8 <- lm(interest_rate ~ . -issued, data = loan)
summary(m8)

# Create histograms for debt_to_income variable and its log transformation
hist(loan$debt_to_income)
hist(log1p(loan$debt_to_income))

# Build a model with log transformation of debt_to_income variable
m9 <- lm(interest_rate ~ . 
         - issued 
         - debt_to_income
         + log1p(debt_to_income),
         data = loan)
summary(m9)

# Build a model with log transformation of debt_to_income variable and interaction term
m10 <- lm(interest_rate ~ . 
          - issued 
          - debt_to_income
          + log1p(debt_to_income)
          + bankruptcy*credit_util,
          data = loan)
summary(m10)

# Create a histogram and Q-Q plot of the residuals for the final model
hist(m10$residuals)
qqPlot(m10$residuals)