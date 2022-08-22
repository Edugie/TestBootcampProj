# PCA DATA MODELLING

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(Hmisc)
library(corrplot)
library(mice)
library(missForest)
library(VIM)
library(lattice)
library(caret)
library(dummy)

# input Train and test Datasets
train_data <- read_csv("train.csv")
test_data <- read_csv("test.csv")

spec(train_data) # specification of the dataset

# Show the first five rows of the dataset
head(train_data, 5)
head(train_data$SalePrice, 5)
str(train_data)


# Clean the train_file saleprice column by removing comma and the dollar sign
rem_comma <- gsub(",", "", train_data$SalePrice)
rem_dol <- gsub("\\$", "", rem_comma)

# convert SalePrice to numeric
saleprice_as_num <- as.numeric(rem_dol)

# Replace the SalePrice column with the cleaned values
train_data$SalePrice <- saleprice_as_num

# Log transforing the salePrice for standard distribution
train_data$Log_SalePrice <- log(train_data$SalePrice)

dim(train_data)

# Select only numeric values from Train Data
numeric_values <- select_if(train_data, is.numeric)

# dimension of numeric values
dim(numeric_values)

# Ommitting Id and NA columns
corplot.names <- colnames(numeric_values[2:dim(numeric_values)[2]])
cordata <- na.omit(train_data[, (names(train_data) %in% corplot.names)])

# Printing the correlation of the numeric values using corrplot.mixed plot
corrplot.mixed(cor(cordata), lower = "square", upper = "circle",
               tl = "lt", diag = "l", bg = "blue" )

# Combine train and test data
dim(train_data)
dim(test_data)

full_data <- rbind(train_data [, -c(dim(train_data)[2] - 1, dim(train_data)[2])], test_data)
dim(full_data)

# Finding columns with NA
varNA <- colnames(full_data)[colSums(is.na(full_data)) > 0]
varNA


# Calculating the percentage of missing values in a feature
percentage_NA <- function(x) {
  (sum(is.na(x))/length(x)) * 100
}

# applying the function on the varNA features
percs <- apply(full_data[varNA], 2, percentage_NA)
percs <- percs[order(-percs)]

# Plotting the plot of missing values
par(las = 2)
barplot(percs, main = "percentage of NA", horiz = TRUE, cex.names = 0.5)

describe(full_data$Fence)
describe(full_data$Alley)
describe(full_data$PoolQC)
describe(full_data$MiscFeature)
describe(full_data$LotFrontage)
describe(full_data$GarageYrBlt)











