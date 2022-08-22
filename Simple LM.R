#Alexander Gribtsov DS-503 Class project
# Goal: present simple linear regression in R with custom paremeters for data splitting
# The Ames Housing dataset was compiled by Dean De Cock for use in data science education.
# With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa.
# Around 1500~ entries
# CSV files can be found here: https://rpubs.com/Ekaterina33/650772
#Main changes in data split is set to 52 and data set division is set to 90/10% train split

# Importing packages

library(corrplot) #provides a visual exploratory tool on correlation matrix that supports automatic variable reordering
library(ggplot2) #system for declaratively creating graphics
library(ggpubr) #creation of beautiful ggplot2-based graphs

#The two functions below are used later for conversion
install.packages("ggpubr")
# 2. Functions
#Convert categorical variable to numeric

categoricalToNumeric <- function(data){
  must.convert <- sapply(data ,is.factor)        #variable needs to be displayed as numeric
  data.aux<-sapply(data[,must.convert],unclass)  #data.frame of all categorical variables to convert
  data.out<-cbind(data[,!must.convert],data.aux) #complete data.frame with all variables
  return(data.out)
}


# Evaluation metric function for root mean squared error (RMSE)
RMSE <- function(x,y){
  a <- round(sqrt(sum((log(x)-log(y))^2)/length(y)),5)
  return(a)
}

# 3. Splitting the Data

df.data <- read.csv("C:/Users/HP/Documents/Home Price Prediction/train.csv") #
df.test <- read.csv("C:/Users/HP/Documents/Home Price Prediction/test.csv")

set.seed(52) # setting the seed for the sample function

df.train.split <- sample(2
                         , nrow(df.data)
                         , replace = TRUE
                         , prob = c(0.8, 0.2))
df.train = df.data[df.train.split == 1,]
df.val   = df.data[df.train.split == 2,]

paste('Length Training Set: ',nrow(df.train),'| Length Validation Set: ',nrow(df.val),'| Length Test Set: ',nrow(df.test))
'Length Training Set:  1149 | Length Validation Set:  311 | Length Test Set:  1459'

#4. Explore data
summary(df.train)
head(df.train)

#5. Data prep
#Encode character features as numeric
df.train <- categoricalToNumeric(df.train)
df.val <- categoricalToNumeric(df.val)
df.test <- categoricalToNumeric(df.test)

head(df.train)

#Set NA's to 0
df.train[is.na(df.train)] <- 0
df.val[is.na(df.val)] <- 0
df.test[is.na(df.test)] <- 0

head(df.train)

#6. Model fit and Predict
#Fit on significant features
#Formula(remove Id)
col.names <- colnames(df.train)
col.names <- col.names[col.names != c('Id','SalePrice')]
fmla <- as.formula(paste("SalePrice ~ ", paste(col.names, collapse= "+")))

#Model
model = lm(formula = fmla, data = df.train)#lm is linear model function in our case we use regression
model.predict <- predict(model,df.val) #linear regression fomrmula
Warning message in col.names != c("Id", "SalePrice"):
  “longer object length is not a multiple of shorter object length”Warning message in predict.lm(model, df.val):
  “prediction from a rank-deficient fit may be misleading”

7. RMSE (root mean squared error)
rmse <- RMSE(df.val$SalePrice, model.predict)
paste('RMSE: ',rmse)

7.1 Prediction vs Actual
ggplot(df.val,aes(x=model.predict,y=SalePrice))+
  geom_point(size=.01 , color = "steelblue")+
  geom_smooth(method = "loess", color="darkred")+
  labs(x="Predicted", y="Actual")+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

8. Create submission file
predict.Test <- predict(model,df.test)
df.predict.Test <- df.test[1]
df.predict.Test$SalePrice <- predict.Test

head(df.predict.Test)

write.csv(df.predict.Test, file = 'submission.csv', row.names=FALSE)

