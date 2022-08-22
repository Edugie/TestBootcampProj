# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

install.packages("")
install.packages("dummies", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("dummy")
package_url <- "http://cran.nexr.com/src/contrib/Archive/dummies/dummies_1.03.tar.gz"
install.packages(package_url, repos = NULL, type = "source")

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

# Any results you write to the current directory are saved as output.
library(dplyr)
library(Hmisc)
library(corrplot)
library(mice)
library(missForest)
library(VIM)
library(lattice)
library(caret)
library(dummy)
library(dummies)

train_data<-read.csv("train.csv")

test_data<- read.csv("test.csv")
## Taking a look at the test dataset and head of train datasets traget varibale(Sales Price)
str(test_data)
head(train_data$SalePrice)

# Clean the train_file saleprice column by removing comma and the dollar sign
rem_comma <- gsub(",", "", train_data$SalePrice)
rem_dol <- gsub("\\$", "", rem_comma)

# convert SalePrice to numeric
saleprice_as_num <- as.numeric(rem_dol)

# Replace the SalePrice column with the cleaned values
train_data$SalePrice <- saleprice_as_num

## Log transforming the salesprice variable for standard distribution.
train_data$log_salesprice <- log(train_data$SalePrice)

#checking out the dimention and describtion of training data
dim(train_data)

## Finding the correlation of the numeric variables in train dataset.
numeric_values <- select_if(train_data,is.numeric) # Only the columns with numeric values are selected 
#Dimension of numeric_values
dim(numeric_values)
## Omitting ID and obervations with NA
corplot.names <- colnames(numeric_values[2:dim(numeric_values)[2]])
cordata <- na.omit(train_data[,(names(train_data) %in% corplot.names)])
## Ploting correlation of the numberic variables using corrplot.mixed plot
corrplot.mixed(cor(cordata), lower = "square", upper ="circle",
               tl= "lt",diag ="l",bg="blue" )
#selecting tables that contain N/A values in them
#First we need to combine both training and test data together
#Dealing with NA in variables

## First lets combine both test and training dataset
dim(train_data)
dim(test_data)

##Need to remove the salesprice and logsalesprice from train data
full_data <- rbind(train_data[,-c(dim(train_data)[2]-1,dim(train_data)[2])],test_data)
dim(full_data)

##Finding variables(Columns) with NA
var_NA <- colnames(full_data)[colSums(is.na(full_data))>0]
var_NA

##Analysing further shows NA in certain features like fence is not actual missing 
# observations. NA in fence means that the house doesnot have fence. So lets remove NA from those
##features and mark it as WT
##Function to calculate the percentage of data missing in a feature.

percentage_NA <- function(x){
  (sum(is.na(x))/length(x))*100
}

##Applying the function "percentage_NA" on our features in var_NA.
perc<- apply(full_data[var_NA],2,percentage_NA)
perc<- perc[order(-perc)]
##Ploting to visualize the percentage of missing data.
par(las=2)
barplot(perc,main="percentage of NA", horiz=TRUE,cex.names=0.5  )
## Usually a safe maximum threshold is 5% of missing data in total for a large dataset.But in this case
## We have features that have huge number of missing data. Lets have a look at few.
describe(full_data$Fence)
describe(full_data$Alley)
describe(full_data$PoolQC)
describe(full_data$MiscFeature)
describe(full_data$LotFrontage)
describe(full_data$GarageYrBlt)
##It can be observed that PoolQC has 99% of NA and MiscFeature has 96% NA and so on
perc
## Converting "BsmtFullBath","BsmtHalfBath" into factors
full_data$BsmtFullBath <- as.factor(full_data$BsmtFullBath)
full_data$BsmtHalfBath <- as.factor(full_data$BsmtHalfBath)
##Now the following features NA means they do not have that particular amenity, hence lets replace
## NA with wt(Without)
var_None <- c("Alley","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
              "FireplaceQu","GarageType","GarageFinish","GarageQual","GarageCond","PoolQC",
              "Fence","MiscFeature","BsmtFullBath","BsmtHalfBath")

fill_None <- function(data,feature){
  levels(data[,feature]) <- c(levels(data[,feature]), "None")
  data[,feature][is.na(data[,feature])] <- "None"
  return(data[,feature])
}

comp_data <- full_data
for(i in 1:length(var_None)){
  comp_data[,var_None][i] <- fill_None(comp_data,var_None[i])
}
##After replacing NA with None for respective features. Lets sort the remaining features that has NA.
rem_NA<-colnames(comp_data[apply(comp_data,2,percentage_NA)>0])
rem_NA
## Lets inpute missing data using missForest Package.
## Take look how many observations are missing in rem_NA
apply(comp_data[rem_NA],2,describe)
str(comp_data)

comp_data_chars <- select_if(comp_data, is.character)
colnames(comp_data_chars)

comp_data$MSZoning <- as.factor(comp_data$MSZoning)
comp_data$Utilities <- as.factor(comp_data$Utilities)
comp_data$Exterior1st <- as.factor(comp_data$Exterior1st)
comp_data$Exterior2nd <- as.factor(comp_data$Exterior2nd)
comp_data$MasVnrType <- as.factor(comp_data$MasVnrType)
comp_data$ExterQual <- as.factor(comp_data$ExterQual)
comp_data$ExterCond <- as.factor(comp_data$ExterCond)
comp_data$Foundation <- as.factor(comp_data$Foundation) 

comp_data$Electrical <- as.factor(comp_data$Electrical)
comp_data$BsmtQual <- as.factor(comp_data$BsmtQual)
comp_data$BsmtCond <- as.factor(comp_data$BsmtCond)
comp_data$BsmtExposure <- as.factor(comp_data$BsmtExposure)
comp_data$BsmtFinType1 <- as.factor(comp_data$BsmtFinType1)
comp_data$BsmtFinType2 <- as.factor(comp_data$BsmtFinType2)
comp_data$Heating <- as.factor(comp_data$Heating)
comp_data$HeatingQC <- as.factor(comp_data$Heating)

comp_data$CentralAir <- as.factor(comp_data$CentralAir)
comp_data$KitchenQual <- as.factor(comp_data$KitchenQual)
comp_data$Functional <- as.factor(comp_data$Functional)
comp_data$FireplaceQu <- as.factor(comp_data$FireplaceQu)
comp_data$GarageType <- as.factor(comp_data$GarageType)
comp_data$GarageFinish <- as.factor(comp_data$GarageFinish)
comp_data$GarageQual <- as.factor(comp_data$GarageQual)

comp_data$GarageCond <- as.factor(comp_data$GarageCond)
comp_data$PavedDrive <- as.factor(comp_data$PavedDrive)
comp_data$PoolQC <- as.factor(comp_data$PoolQC)
comp_data$Fence <- as.factor(comp_data$Fence)
comp_data$MiscFeature <- as.factor(comp_data$MiscFeature)

comp_data$SaleType <- as.factor(comp_data$SaleType)
comp_data$SaleCondition <- as.factor(comp_data$SaleCondition)

describe(comp_data$SaleType)

imp.arg <- missForest(comp_data[rem_NA],verbose= TRUE, maxiter=3,ntree=20)

## Normalized Root Mean Squared error and proposition of falized classification looks satisfying.
## Random forest has imputed plausible data.
imp.arg$OOBerror

## comp_data before imputation
plot1 <- aggr(comp_data, col = c("Navy Blue","Red"),
              numbers = TRUE, sortVars = TRUE,
              labels = names(comp_data), cex.axis =0.7,
              gap=2, ylab = c("Missing data Before Imputation", "Pattern"))

##Total no. of NA in comp_data before imputation
sum(is.na(comp_data))

imp_data <- imp.arg$ximp
## Replacing missing observations with imputed values
for(i in 1:length(rem_NA)){
  comp_data[,rem_NA][i] <- imp_data[i]
}
##After replacing it with imputed values
sum(is.na(comp_data))

## Lets plot using VIM
plot2 <- aggr(comp_data, col =c("Navy blue", "Red"),
              numbers= TRUE, sortVars = TRUE,
              labels = names(comp_data),cex.axis = 0.7,
              gap=2, ylab= c("Missing data AFTER imputation", "Pattern"))

## Preparing data for PCA
## Seperating catergorical variables and create dummies.
cat_variables <- select_if(comp_data, is.factor)

colnames(cat_variables)

x = c('MSZoning', 'Street', 'Alley', 'LotShape', 'LandContour', 
      'Utilities', 'LotConfig', 'LandSlope', 'Neighborhood', 'Condition1',
      'Condition2', 'BldgType', 'HouseStyle', 'RoofStyle', 'RoofMatl', 
      'Exterior1st', 'Exterior2nd', 'MasVnrType', 'ExterQual', 'ExterCond',
      'Foundation', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1',
      'BsmtFinType2', 'Heating', 'HeatingQC', 'CentralAir', 'Electrical', 
      'BsmtFullBath', 'BsmtHalfBath', 'KitchenQual', 'Functional', 
      'FireplaceQu', 'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond',
      'PavedDrive', 'PoolQC', 'Fence', 'MiscFeature', 'SaleType', 
      'SaleCondition')

new_data <- dummy.data.frame(comp_data, names= c('MSZoning','Street','Alley','LotShape','LandContour',
                                                 'Utilities','LotConfig','LandSlope','Neighborhood','Condition1','Condition2','BldgType','HouseStyle',
                                                 'RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType','ExterQual','ExterCond','Foundation',
                                                 'BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC','CentralAir',
                                                 'Electrical','BsmtFullBath','BsmtHalfBath','KitchenQual','Functional','FireplaceQu','GarageType',
                                                 'GarageFinish','GarageQual','GarageCond','PavedDrive','PoolQC','Fence','MiscFeature','SaleType','SaleCondition'))

## Now everything is numeric, lets devide the dataset as test and train.
pca.train <- new_data[1:nrow(train_data),]
pca.test <- new_data[-(1:nrow(train_data)),]
drop<-colnames(pca.train[, colSums(pca.train)== 0])
pca.train <- pca.train[, !names(pca.train)%in% drop]
pca.test <- pca.test[,!names(pca.test)%in% drop]
pca <- prcomp(pca.train,scale. =T)
biplot(pca, scale =0)
names(pca)
'sdev' 'rotation' 'center' 'scale' 'x'
std_dev <- pca$sdev
pca_var <- std_dev^2
pca_varmax <- pca_var/sum(pca_var)
pca_varmax[1:10]
0.0631973085884993 0.0291943881613535 0.0243407916378229 0.0227583009365142 0.0194401440693758 0.01715071139901 0.0152359066301092 0.0149130215284415 0.0123522156454887 0.0120619654471795
plot(cumsum(pca_varmax), xlab= "Principle Component", ylab="Proposition of variance explained",
     type= "b")
## Creating Prediction 
fin_train <- data.frame(log_salesprice = train_data$log_salesprice, pca$x)
fin_train <- fin_train[,1:231]
myTrainControl <- trainControl(method="repeatedcv",number=10,repeats = 4)
fit.glmnet <- train(log_salesprice~.,fin_train,trControl = myTrainControl,
                    method="glmnet",tuneGrid=expand.grid(.alpha = seq(0,1,by=0.05), 
                                                         .lambda = seq(0, 0.08, by = 0.01)))
Loading required package: glmnet
Loading required package: Matrix
Loaded glmnet 2.0-5

fin_test <- predict(pca,pca.test)
fin_test <- data.frame(fin_test)
fin_test <- fin_test[,1:230]
prediction <- predict(fit.glmnet,fin_test)
act_prediction <- exp(prediction)
actual_prediction <- unname(act_prediction)
actual_prediction
submit <- data.frame(Id=test_data$Id,SalePrice=actual_prediction)
write.csv(submit,file="3-18-17.csv",row.names=F)