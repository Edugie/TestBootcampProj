# Instcomp_dataing Packages

install.packages("plotly")
install.packages("Amelia")
install.packages("Rmisc")
install.packages("forcats")
install.packages("caTools")
install.packages("reshape2")
install.packages("ggrepel")
install.packages("psych")
install.packages("caret")


# Importing libraries
library(dplyr)
library(plotly)
library(Amelia)
library(Rmisc)
library(forcats)
library(corrplot)
library(caTools)
library(reshape2)
library(ggrepel)
library(psych) 
library(caret)

# UNABLE TO USE MY SQL DATABASE MY DATASOURCE. DUE TO TIME CONSTRAINT, I HAD TO USE EXCEL
# INSTEAD.
# INCLUDED IS THE SQL CONNECTION CODE
#con <- dbConnect(odbc::odbc(),
#                 Driver = "SQL Server",
#                 Server = "DESKTOP-BLPJI29",
#                 Database = "HomePrediction",
#                 Port = 1433)
#
#dbListTables(con) # Tables in DB
#dbListFields(con, 'HomeSales')
#
#traindata <- tbl(con, 'HomeSales')
#traindata <-collect(traindata)
#View(testdata)
# Reading Data From Files

train_data <- read.csv("C:/Users/AIL/Desktop/PwC Tech Bootcamp/Project One/Home Sales Price Prediction/train.csv")
test_data <- read.csv("C:/Users/AIL/Desktop/PwC Tech Bootcamp/Project One/Home Sales Price Prediction/test.csv")

# Remove the ID columns from both datasets
train_data$Id <-  NULL
test_data$Id <-  NULL

# Add a new column to the test_data to match the number of features in the train_data
test_data$SalePrice <- NA

# Combine both test and train datasets to have a complete data
comp_data <- rbind(train_data, test_data)
dim(comp_data)

# Remove columns from the dataset with large missing values
rem_cols <- c("Alley", "FireplaceQu", "PoolQC", "Fence", "MiscFeature")
comp_data <- comp_data[, !(colnames(comp_data) %in% rem_cols)]
dim(comp_data)

# Dealing with missing values. Extract columns with missing values
train.missing <- as.data.frame(sapply(comp_data, function(x) sum(is.na(x))))

# combine the names of the features with the number of NA values
train.missing <- cbind(rownames(train.missing), data.frame(train.missing, row.names=NULL))

# Rename the columns Features and NA_Count
colnames(train.missing) <- c('Features', "NA_Count")

# Filter the NA_Count  column by selecting only rows more than 0
train.missing <- filter(train.missing, NA_Count > 0)

# Imput features with not avialable Values. Make missing  values explicit

# Impute 'No GArage' for GarageType that is not having cars
comp_data$GarageType <- fct_explicit_na(comp_data$GarageType, na_level = "No Garage")

# Impute 'No Garage' for Grage Finishing for No grage values - train
comp_data$GarageFinish <- fct_explicit_na(comp_data$GarageFinish, na_level = "No Garage")

# Impute 'No Garage' for GrageQuality for No grage values - train
comp_data$GarageQual <- fct_explicit_na(comp_data$GarageQual, na_level = "No Garage")

# Impute 'No Garage' for GarageCond for No grage values - train
comp_data$GarageCond <- fct_explicit_na(comp_data$GarageCond, na_level = "No Garage")

# Impute MasVnr Type to None and its area to 0
comp_data$MasVnrType[is.na(comp_data$MasVnrType)] <- "None"
comp_data$MasVnrArea[is.na(comp_data$MasVnrArea)] <- 0

# Average sale price
# avg_sale_price <- mean(train_data$SalePrice) 

# Based on the average price range, SBrkr is selected to imput Electical categrory
comp_data$Electrical[is.na(comp_data$Electrical)] <- 'SBrkr'

# Imput Utilities in testdf na
comp_data$Utilities[is.na(comp_data$Utilities)] <- 'AllPub'

# Imput None category for BsmtQual, BsmtCond, BsmtExposure,BsmtFinType1, BsmtFinType2 in train_data
comp_data$BsmtQual <- fct_explicit_na(comp_data$BsmtQual, na_level = "No Basement")
comp_data$BsmtExposure <- fct_explicit_na(comp_data$BsmtExposure, na_level = "No Basement")
comp_data$BsmtCond <- fct_explicit_na(comp_data$BsmtCond, na_level = "No Basement")
comp_data$BsmtFinType1 <- fct_explicit_na(comp_data$BsmtFinType1, na_level = "No Basement")
comp_data$BsmtFinType2 <- fct_explicit_na(comp_data$BsmtFinType2, na_level = "No Basement")
comp_data$LotFrontage[is.na(comp_data$LotFrontage)]<- 70
comp_data$GarageYrBlt[is.na(comp_data$GarageYrBlt)] <- comp_data$YearBuilt[is.na(comp_data$GarageYrBlt)]


# Imput test_data GarageCars Na to 1, and GarageArea to 280

#GarageCars
comp_data$GarageCars[is.na(comp_data$GarageCars)] <- 1

# GarageArea
comp_data$GarageArea[is.na(comp_data$GarageArea)] <- 280

#MsZoning 
comp_data$MSZoning[is.na(comp_data$MSZoning)] <- names(sort(-table(comp_data$MSZoning)))[1]
comp_data$MSZoning <- as.factor(comp_data$MSZoning)

comp_data$BsmtFinSF1[is.na(comp_data$BsmtFinSF1)] <- 0
comp_data$BsmtFinSF2[is.na(comp_data$BsmtFinSF2)] <- 0
comp_data$TotalBsmtSF[is.na(comp_data$TotalBsmtSF)] <- 0
comp_data$BsmtUnfSF[is.na(comp_data$BsmtUnfSF)] <- 0
comp_data$BsmtFullBath[is.na(comp_data$BsmtFullBath)] <- 0
comp_data$BsmtHalfBath[is.na(comp_data$BsmtHalfBath)]<-0

#Functional
comp_data$Functional[is.na(comp_data$Functional)] <- names(sort(-table(comp_data$Functional)))[1]
comp_data$Functional <- as.integer(revalue(comp_data$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))

#Sale type
comp_data$SaleType[is.na(comp_data$SaleType)] <- names(sort(-table(comp_data$SaleType)))[1]
comp_data$SaleType <- as.factor(comp_data$SaleType)

# Kitchen Quality
comp_data$KitchenQual[is.na(comp_data$KitchenQual)] <- names(sort(-table(comp_data$KitchenQual)))[1]
comp_data$KitchenQual <- as.factor(comp_data$KitchenQual)

comp_data$Exterior1st[is.na(comp_data$Exterior1st)] <- names(sort(-table(comp_data$Exterior1st)))[1]
comp_data$Exterior1st <- as.factor(comp_data$Exterior1st)

comp_data$Exterior2nd[is.na(comp_data$Exterior2nd)] <- names(sort(-table(comp_data$Exterior2nd)))[1]

comp_data$Exterior2nd <- as.factor(comp_data$Exterior2nd)

# Dealing with missing values. Extract columns with missing values
train.missing2 <- as.data.frame(sapply(comp_data, function(x) sum(is.na(x))))

# combine the names of the features with the number of NA values
train.missing2 <- cbind(rownames(train.missing2), data.frame(train.missing2, row.names=NULL))

# Rename the columns Features and NA_Count
colnames(train.missing2) <- c('Features', "NA_Count")

# Filter the NA_Count  column by selecting only rows more than 0
train.missing2 <- filter(train.missing2, NA_Count > 0)

str(comp_data)

# Convert some features to factors even though they are appearing as integers
comp_data$MSSubClass <- as.factor(comp_data$MSSubClass)
comp_data$OverallQual<- as.factor(comp_data$OverallQual)
comp_data$OverallCond<- as.factor(comp_data$OverallCond)
comp_data$YearBuilt<- as.factor(comp_data$YearBuilt)
comp_data$YearRemodAdd <- as.factor(comp_data$YearRemodAdd)
comp_data$YrSold <- as.factor(comp_data$YrSold)
comp_data$GarageYrBlt <- as.factor(comp_data$GarageYrBlt)

# Numerical Features and the Correlation with Target Feature

# Calculating Correlation Matrix and Heat Map
options(repr.plot.width=30, repr.plot.height=20)
nums <- which(sapply(comp_data, is.numeric))
comp_data_numVar <- comp_data[, nums]
dt <- na.omit(comp_data_numVar)

# Correlation matrix
cormat <- round(cor(dt),2)

# Transform the data
melted_cormat <- melt(cormat)
plt <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +geom_tile() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 18, hjust = 1),
                                                                                            axis.text.y = element_text(size = 18))
plt

# Highly correlating Numeric Predictors
options(repr.plot.width=20, repr.plot.height=10)
nums <- which(sapply(train_data, is.numeric))
comp_data_numVar <- train_data[, nums]
cor_numVar <- cor(comp_data_numVar, use="pairwise.complete.obs")

# Sort the correlation
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

# High correlation for features whose absolute value  is greater than 0.5
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)  > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", title = "Highly correlating Numeric Predictors", mar = c(1,1,1,1))


# Preparing the data set for Model readyness
# Removing the Columns that are Highly correlating
drop_cols <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

comp_data <- comp_data[,!(names(comp_data) %in% drop_cols)]

# Removing outliers
# plotting a scatter plots
ggplot(dt, aes(x = 1: length(SalePrice), y = SalePrice)) + geom_point() # two values which can be removed

# Total Sq. Ft
TotalSqFeet <- dt$GrLivArea + dt$TotalBsmtSF

pt <- ggplot(dt, aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") + scale_y_continuous(breaks= seq(0, 800000, by=100000)) +
  geom_text_repel(aes(label = ifelse(comp_data$GrLivArea[!is.na(comp_data$SalePrice)] > 4500, rownames(comp_data), '')))

pt

#Remove Outliers. I.e, Rows where the Housing price is low even though the total area is very high 
comp_data <- comp_data[-c(524, 1299),]


# PreProcessing the Predictors

# All numerical columns
nums <- which(sapply(comp_data, is.numeric))
comp_data_numVar <- comp_data[, nums]
str(comp_data_numVar)

# All categorical columns
cat_cols <- which(sapply(comp_data, is.factor))
comp_data_catVar <- comp_data[, cat_cols]
str(comp_data_catVar)

# Skewnessusing the pscy library and Normaling the Numerical Predictors 

for(i in 1:ncol(comp_data_numVar)){
  if (abs(skew(comp_data_numVar[,i]))>0.8){
    comp_data_numVar[,i] <- log(comp_data_numVar[,i]+1)
  }
}

dim(comp_data_numVar)

# Normalising the Numerical Predictors
PreNum <- preProcess(comp_data_numVar, method=c("center", "scale"))
PreNum

# DataFrame normalisation
DFnorm <- predict(PreNum, comp_data_numVar)
dim(DFnorm)

# Converting Categorical features to Numerical
  
# Converting the categorical columns to numerical with zeros (0s) and (1s)
DFdummies <- as.data.frame(model.matrix(~.-1, comp_data_catVar))
dim(DFdummies)

head(DFdummies, 3)

# Remove all columns with '0' as they don't contribute to the regression prediction
ZerocolTest <- which(colSums(DFdummies[(nrow(comp_data[!is.na(comp_data$SalePrice),]) + 1 ):nrow(comp_data),]) == 0)
colnames(DFdummies[ZerocolTest])

# Removing zero valued columns
DFdummies <- DFdummies[,-ZerocolTest]

# check if some values are absent in the train set
ZerocolTrain <- which(colSums(DFdummies[1:nrow(comp_data[!is.na(comp_data$SalePrice),]),]) == 0)
colnames(DFdummies[ZerocolTrain])

# Removing predictor
DFdummies <- DFdummies[,-ZerocolTrain]

# Taking out variables with less than 10 ‘ones’ in the train set.
fewOnes <- which(colSums(DFdummies[1:nrow(comp_data[!is.na(comp_data$SalePrice),]),]) < 10)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

# Getting final combined Normalized numerical predictors and categorical column convered to numerical with dummy coding

#combining all (now numeric) predictors into one dataframe
combined <- cbind(DFnorm, DFdummies)

# Get back the train and test data sets from combined
comp_data$SalePrice <- log(comp_data$SalePrice)
train1 <- combined[!is.na(comp_data$SalePrice),]
test1 <- combined[is.na(comp_data$SalePrice),]
train1$SalePrice <- comp_data$SalePrice[!is.na(comp_data$SalePrice)]

# Linear Model Building and Checking its efficiency

# Split the data

set.seed(101)
set <- sample.split(train1$SalePrice, SplitRatio = 0.7)
trainset <- subset(train1, set == TRUE)
testset <- subset(train1, set == FALSE)

# inear Model fitting with lm()
# Building the linear Model on trainset for SalePrice
linear_model <- lm(SalePrice ~ .,trainset)

# summary (linear.model)
actuals <- testset$SalePrice
testset$SalePrice <- NULL

# Predicting the results on test data

predicted_results <- predict(linear_model, testset)

# Checking Vitals for Linear Model efficiency
# Make a data frame of acutals and predicted side by side
predicted <- as.data.frame(predicted_results)
actual <-as.data.frame(actuals)
compare <- cbind(predicted, actual)
compare$error <- compare$predicted_results - compare$actuals
head(compare)

# Root Mean Square Error (RMSE) Calculation
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- compare$error # same as data$Y - predictedY
predictionRMSE <- rmse(error) 
predictionRMSE

# R Square Value Calculation
# Average of actual data
avg_y_actual <- mean(compare$actuals)

# Total sum of squares
ss_total <- sum((compare$actuals - avg_y_actual)^2)

# Regression sum of squares
ss_regression <- sum((compare$predicted.results - avg_y_actual)^2)

# Residual sum of squares
ss_residuals <- sum((compare$actuals - compare$predicted.results)^2)

results <- c(ss_regression, ss_residuals)

# R2 Score
r2 <- 1 - ss_residuals / ss_total

# X,Y Plot to compare, Predicted and Actuals
options(repr.plot.width=20, repr.plot.height=5)
plot(compare$predicted_results,compare$actuals,xlab="predicted values",ylab="actual values", col = c("red", "blue"))
abline(a=0,b=1)
