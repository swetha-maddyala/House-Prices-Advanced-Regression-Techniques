#Importing packages
rm(list=ls())
#install.packages("rio")
#install.packages("moments")
library(rio)
library(moments)

#Setting up the random seed
set.seed(123)

#Setting up the working directory
setwd("C:/Users/sweth/OneDrive/Desktop/kavya")
#Loading the dataset
#Importing the train dataset
housesales_train=import("train.csv")
colnames(housesales_train)=tolower(make.names(colnames(housesales_train)))
#Importing the test dataset
housesales_test=import("test.csv")
colnames(housesales_test)=tolower(make.names(colnames(housesales_test)))
attach(housesales_train)

#--------------------------------------------------------------------------------------------------------------
#1. Data Exploration
#--------------------------------------------------------------------------------------------------------------
#Summary statistics
summary(housesales_train)
#Histogram of SalePrice
#The histogram shows the distribution of SalePrice.
hist(housesales_train$saleprice)
#Boxplot of SalePrice by OverallQual 
#OverallQual seems to have an impact on SalePrice based on the boxplot.
boxplot(saleprice ~ overallqual, data = housesales_train)
#Scatterplot of GrLivArea vs SalePrice
#GrLivArea is positively correlated with SalePrice as seen in the scatterplot. 
plot(housesales_train$grlivarea, housesales_train$saleprice)




#--------------------------------------------------------------------------------------------------------------
#2. Data Cleaning
#--------------------------------------------------------------------------------------------------------------
#Determining which features have missing values
sapply(housesales_train, function(x) sum(is.na(x)))
#Imputation
#Imputing numerical missing values with median
housesales_train$lotfrontage[is.na(housesales_train$lotfrontage)] <- median(housesales_train$lotfrontage, na.rm = TRUE)
#Imputing with 0, indicating no masonry veneer.
housesales_train$masvnrarea[is.na(housesales_train$masvnrarea)] <- 0

#Imputing categorical missing values
housesales_train$alley[is.na(housesales_train$alley)] <- "None"

housesales_train$masvnrtype[is.na(housesales_train$masvnrtype)] <- "None"

housesales_train$bsmtqual[is.na(housesales_train$bsmtqual)] <- "None"
housesales_train$bsmtcond[is.na(housesales_train$bsmtcond)] <- "None"
housesales_train$bsmtexposure[is.na(housesales_train$bsmtexposure)] <- "No"
housesales_train$bsmtfintype1[is.na(housesales_train$bsmtfintype1)] <- "None"
housesales_train$bsmtfintype2 [is.na(housesales_train$bsmtfintype2 )] <- "None"

#install.packages("modeest")
library(modeest)
# Calculating the mode of the 'electrical' column
mode_value <- as.character(mlv(housesales_train$electrical))
housesales_train$electrical[is.na(housesales_train$electrical)] <- mode_value

housesales_train$fireplacequ[is.na(housesales_train$fireplacequ)] <- "None"

housesales_train$garagetype[is.na(housesales_train$garagetype)] <- "None"
housesales_train$garageyrblt[is.na(housesales_train$garageyrblt)] <- "None"
housesales_train$garagefinish[is.na(housesales_train$garagefinish)] <- "None"
housesales_train$garagequal[is.na(housesales_train$garagequal)] <- "None"
housesales_train$garagecond[is.na(housesales_train$garagecond)] <- "None"

housesales_train$poolqc[is.na(housesales_train$poolqc)] <- "None"

housesales_train$fence[is.na(housesales_train$fence)] <- "None"
housesales_train$miscfeature[is.na(housesales_train$miscfeature)] <- "None" 
#Confirm imputation
sapply(housesales_train, function(x) sum(is.na(x)))

#Encoding Categorical variables
#Identifying categorical columns 
categorical_cols <- sapply(housesales_train, is.character) 
#Printing categorical column names
cat("Categorical columns:\n")
print(names(housesales_train)[categorical_cols])

# Converting categorical columns to factors
categorical_cols <- sapply(housesales_train, is.character)
for(col in names(housesales_train)[categorical_cols]) {
  
  housesales_train[[col]] <- as.factor(housesales_train[[col]])
  
  if (col %in% c(
    "mszoning", "street", "alley", "lotshape", "landcontour", "utilities", "lotconfig",
    "landslope", "neighborhood", "condition1", "condition2", "bldgtype", "housestyle", "roofstyle",
    "roofmatl", "exterior1st", "exterior2nd", "masvnrtype", "exterqual", "extercond", "foundation",
    "bsmtqual", "bsmtcond", "bsmtexposure", "bsmtfintype1", "bsmtfintype2", "heating", "heatingqc",
    "centralair", "electrical", "kitchenqual", "functional", "fireplacequ", "garagetype", "garageyrblt",
    "garagefinish", "garagequal", "garagecond", "paveddrive", "poolqc", "fence", "miscfeature",
    "saletype", "salecondition"
  )){
    
    housesales_train[[col]] <- relevel(housesales_train[[col]], ref = levels(housesales_train[[col]])[1])
    
    
  }  else {
    
    housesales_train[[col]] <- relevel(housesales_train[[col]], ref = levels(housesales_train[[col]])[1])
    
  }
  
}
# Label Encoding
label_encoding <- c(Ex = 1, Gd = 2, TA = 3, Fa = 4, Po = 5)
housesales_train$encodedexterqual <- as.numeric(factor(housesales_train$exterqual, levels = names(label_encoding), labels = label_encoding))
housesales_train$encodedextercond <- as.numeric(factor(housesales_train$extercond, levels = names(label_encoding), labels = label_encoding))
housesales_train$encodedheatingqc <- as.numeric(factor(housesales_train$heatingqc, levels = names(label_encoding), labels = label_encoding))
housesales_train$encodedkitchenqual <- as.numeric(factor(housesales_train$kitchenqual, levels = names(label_encoding), labels = label_encoding))

label_encoding_1 <- c(Ex = 1, Gd = 2, TA = 3, Fa = 4, Po = 5, None = 6)
housesales_train$encodedbsmtqual <- as.numeric(factor(housesales_train$bsmtqual, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_train$encodedbsmtcond <- as.numeric(factor(housesales_train$bsmtcond, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_train$encodedfireplacequ <- as.numeric(factor(housesales_train$fireplacequ, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_train$encodedgaragequal <- as.numeric(factor(housesales_train$garagequal, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_train$encodedgaragecond <- as.numeric(factor(housesales_train$garagecond, levels = names(label_encoding_1), labels = label_encoding_1))

label_encoding_2 <- c(Gd = 1, Av = 2, Mn = 3, No = 4)
housesales_train$encodedbsmtexposure <- as.numeric(factor(housesales_train$bsmtexposure, levels = names(label_encoding_2), labels = label_encoding_2))

label_encoding_3 <- c(Ex = 1, Gd = 2, TA = 3, Fa = 4, None = 5)
housesales_train$encodedpoolqc <- as.numeric(factor(housesales_train$poolqc, levels = names(label_encoding_3), labels = label_encoding_3))

View(housesales_train)

#Checking categorical columns 
categorical_cols <- sapply(housesales_train, is.character) 
#Printing categorical column names
cat("Categorical columns:\n")
print(names(housesales_train)[categorical_cols])
# Droping  categorical columns for which already new columns have been created
# Specify the columns to be removed
columns_to_remove <- c("exterqual", "extercond", "bsmtqual", "bsmtcond", "bsmtexposure",
                       "heatingqc", "kitchenqual", "fireplacequ", "garagequal", "garagecond", "poolqc")

# Remove the specified columns
housesales_train <- housesales_train[, -which(names(housesales_train) %in% columns_to_remove)]

#--------------------------------------------------------------------------------------------------------------
#3. Feature Engineering
#--------------------------------------------------------------------------------------------------------------

# Total square footage 
housesales_train$totalsqft <- housesales_train$grlivarea + housesales_train$totalbsmtsf

# Total bathrooms
housesales_train$totalbaths <- housesales_train$fullbath + 0.5*(housesales_train$halfbath)

# Age of home
housesales_train$age <- housesales_train$yrsold - housesales_train$yearbuilt 


# Total porch sqft
housesales_train$totalporchsqft <- housesales_train$openporchsf + housesales_train$enclosedporch + 
  housesales_train$x3ssnporch + housesales_train$screenporch

# Total parking size 
housesales_train$totalparkingsize <- housesales_train$garagearea+ housesales_train$wooddecksf

# Has 2nd floor 
housesales_train$hassecondfloor <- ifelse(housesales_train$x2ndflrsf > 0, 1, 0)

# Lot size square 
housesales_train$lotsizesq <- housesales_train$lotfrontage * housesales_train$lotarea

# New construction
housesales_train$newconstruction <- ifelse(housesales_train$yearbuilt == housesales_train$yrsold, 1, 0)

# Write the cleaned data frame to a csv file after feature engineering
write.csv(housesales_train, file = "feature_engineering_housesales_train_cleaned.csv", row.names = FALSE)
View(housesales_train)


#Feature Importance
#-------------------

library(randomForest)
# Identifying columns with >53 levels
many_levels <- sapply(housesales_train, function(x) length(levels(x))) > 53
# Remove those columns
housesales_train <- housesales_train[, !many_levels]
# Now fit model
rf_model <- randomForest(saleprice ~ ., data = housesales_train, importance = TRUE)
print(importance(rf_model))

#--------------------------------------------------------------------------------------------------------------
#4. Model Building
#--------------------------------------------------------------------------------------------------------------
# Load caret package
#install.packages("Metrics")
library(Metrics)
library(caret)
set.seed(123)

# Create 70/30 split
train_index <- createDataPartition(housesales_train$saleprice, p = 0.7, list = FALSE)
train_data <- housesales_train[train_index, ]
valid_data <- housesales_train[-train_index, ]

#Baseline_Model - Calculating the mean sale price in the training data
mean_sale_price <- mean(train_data$saleprice)
baseline_predictions <- rep(mean_sale_price, nrow(train_data))
baseline_rmse <- rmse(baseline_predictions, train_data$saleprice)
print(baseline_rmse)

features_lm <- c('grlivarea', 'totalsqft', 'neighborhood', 'overallqual', 
                 'garagecars', 'totalbaths', 'age', 'enclosedporch')


# MODEL 1: Simple linear regression model
lm_model <- lm(saleprice ~ grlivarea + totalsqft + neighborhood + overallqual + garagecars + 
                 totalbaths + age + enclosedporch, data = train_data)

# Summary of the linear regression model
summary(lm_model)
library(Metrics)
predictions_lm <- predict(lm_model, valid_data[, features_lm])
rmse_lm <- rmse(predictions_lm, valid_data$saleprice)

cat("Baseline RMSE:", baseline_rmse, "\n")
cat("Linear Model RMSE:", rmse_lm, "\n")


#--------------------------------------------------------------------------------------------------------------
#5. Correlation Analysis:
#--------------------------------------------------------------------------------------------------------------
numeric_cols <- sapply(housesales_train, is.numeric)
cor_matrix <- cor(housesales_train[,numeric_cols])

library(ggplot2)
#install.packages("reshape2")
library(reshape2)
# Correlation matrix
cor_matrix <- cor(housesales_train[, numeric_cols]) 
# Melt matrix into data frame
melted_cor <- melt(cor_matrix)
# Clustered dendrogram ordering
dd <- as.dendrogram(hclust(dist(cor_matrix)))
ordered_labels <- labels(dd)
ggplot(melted_cor, aes(x = ordered_labels[Var1], y = ordered_labels[Var2], fill = value)) +
  geom_tile() + 
  xlab("Variables") +
  ylab("Variables") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab" , 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)) +
  coord_fixed() +
  ggtitle("Correlation Matrix")
#--------------------------------------------------------------------------------------------------------------
# MODEL 2: Simple random forest model
features_rf <- c("neighborhood","overallqual","overallcond","grlivarea", 
                 "totalsqft", "totalbaths", "garagecars", "age")
# Fit model
library(randomForest)
rf_model <- randomForest(saleprice ~ grlivarea*age + totalsqft*garagecars +
                           neighborhood + overallqual + overallcond +  
                           totalbaths,
                         data = train_data,
                         ntree=500,
                         importance = TRUE)
# Summary of the random forest model
summary(rf_model)
library(Metrics)
predictions_rf <- predict(rf_model, valid_data[, features_rf])
rmse_rf <- rmse(predictions_rf, valid_data$saleprice)
# Check importance
importance(rf_model)

cat("Baseline RMSE:", baseline_rmse, "\n")
cat("Linear Model RMSE:", rmse_lm, "\n")
cat("Random Forest Model RMSE:", rmse_rf, "\n")

#--------------------------------------------------------------------------------------------------------------
#6. Data Improvement Strategies:
#--------------------------------------------------------------------------------------------------------------
# Count the number of NA values in the sale_price column
sum(is.na(train_data$sale_price))
print(train_data$saleprice)

# Identify the saleprice column
saleprice <- train_data$saleprice

# Calculate the z-score for each data point
z_scores <- (saleprice - mean(saleprice)) / sd(saleprice)
# Identify data points with a z-score of 3 or greater
outliers_z <- saleprice[z_scores >= 3]

# Calculate the interquartile range (IQR)
IQR <- IQR(saleprice)
Q1 <- quantile(saleprice, 0.25)
Q3 <- quantile(saleprice, 0.75) 
# Identify data points that are less than the lower fence or greater than the upper fence
outliers_IQR <- saleprice[saleprice < (Q1 - 1.5 * IQR) | saleprice > (Q3 + 1.5 * IQR)]

# Create a boxplot of the data
boxplot(saleprice, main="Sale Prices", ylab="Sale Price ($)")
# Identify data points that are shown as dots or stars outside of the box
outliers_boxplot <- saleprice[boxplot.stats(saleprice)$out]

# Combine the results from all three methods
outliers <- unique(c(outliers_z, outliers_IQR, outliers_boxplot))
# Print the outliers
print(outliers)
# Check if outliers still present  
any(train_data$saleprice %in% outliers)

# Remove outliers
train_data_no_outliers <- train_data[!train_data$saleprice %in% outliers,]
# Check if outliers still present  
any(train_data_no_outliers$saleprice %in% outliers)
# Write the cleaned data frame to a csv file after removing the outliers
write.csv(train_data_no_outliers, file = "Removed_outliers_train_data.csv", row.names = FALSE)
View(train_data_no_outliers)
#--------------------------------------------------------------------------------------------------------------
#Train_data without Outliers

#Baseline_Model - Calculating the mean sale price in the training data
mean_sale_price_no_outliers <- mean(train_data_no_outliers$saleprice)
baseline_predictions_no_outliers <- rep(mean_sale_price_no_outliers, nrow(train_data_no_outliers))
baseline_rmse_no_outliers <- rmse(baseline_predictions_no_outliers, train_data_no_outliers$saleprice)
print(baseline_rmse_no_outliers)

features_lm_no_outliers <- c('grlivarea', 'totalsqft', 'neighborhood', 'overallqual', 
                             'garagecars', 'totalbaths', 'age', 'enclosedporch')


# MODEL 1(a): Simple linear regression model(No outliers)
lm_model_no_outliers <- lm(saleprice ~ grlivarea + totalsqft + neighborhood + overallqual + garagecars + 
                             totalbaths + age + enclosedporch, data = train_data_no_outliers)

# Summary of the Simple linear regression model(No outliers)
summary(lm_model_no_outliers)
library(Metrics)
predictions_lm_no_outliers <- predict(lm_model_no_outliers, valid_data[, features_lm])
rmse_lm_no_outliers <- rmse(predictions_lm_no_outliers, valid_data$saleprice)

cat("Baseline RMSE:", baseline_rmse, "\n")
cat("Linear Model RMSE:", rmse_lm, "\n")
cat("Baseline RMSE(No outliers):", baseline_rmse_no_outliers, "\n")
cat("Linear Model RMSE(No outliers):", rmse_lm_no_outliers, "\n")


# MODEL 2(a): Simple Random Forest model(No outliers)
features_rf_no_outliers <- c("neighborhood","overallqual","overallcond","grlivarea", 
                             "totalsqft", "totalbaths", "garagecars", "age")
# Fit model
library(randomForest)
rf_model_no_outliers <- randomForest(saleprice ~ grlivarea*age + totalsqft*garagecars +
                                       
                                       neighborhood + overallqual + overallcond +  
                                       totalbaths,
                                     data = train_data_no_outliers,
                                     ntree=500,
                                     importance = TRUE)
# Summary of the random forest model (No outliers)
summary(rf_model_no_outliers)
library(Metrics)
predictions_rf_no_outliers <- predict(rf_model_no_outliers, valid_data[, features_rf_no_outliers])
rmse_rf_no_outliers <- rmse(predictions_rf_no_outliers, valid_data$saleprice)
# Check importance
importance(rf_model_no_outliers)

cat("Baseline RMSE:", baseline_rmse, "\n")
cat("Linear Model RMSE:", rmse_lm, "\n")
cat("Random Forest Model RMSE:", rmse_rf, "\n")
cat("Baseline RMSE(No outliers):", baseline_rmse_no_outliers, "\n")
cat("Linear Model RMSE(No outliers):", rmse_lm_no_outliers, "\n")
cat("Random Forest Model RMSE (No outliers):", rmse_rf_no_outliers , "\n")

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#Sample Submission on Test Dataset (RANDOM FOREST MODEL)
housesales_test=import("test.csv")
colnames(housesales_test)=tolower(make.names(colnames(housesales_test)))
#Importing the test dataset
housesales_test=import("test.csv")
colnames(housesales_test)=tolower(make.names(colnames(housesales_test)))
attach(housesales_test)
#Determining which features have missing values
sapply(housesales_test, function(x) sum(is.na(x)))
#Imputation
#Imputing numerical missing values with median
housesales_test$lotfrontage[is.na(housesales_test$lotfrontage)] <- median(housesales_test$lotfrontage, na.rm = TRUE)
#Imputing with 0, indicating no masonry veneer.
housesales_test$masvnrarea[is.na(housesales_test$masvnrarea)] <- 0

#Imputing categorical missing values
housesales_test$alley[is.na(housesales_test$alley)] <- "None"

housesales_test$masvnrtype[is.na(housesales_test$masvnrtype)] <- "None"

housesales_test$bsmtqual[is.na(housesales_test$bsmtqual)] <- "None"
housesales_test$bsmtcond[is.na(housesales_test$bsmtcond)] <- "None"
housesales_test$bsmtexposure[is.na(housesales_test$bsmtexposure)] <- "No"
housesales_test$bsmtfintype1[is.na(housesales_test$bsmtfintype1)] <- "None"
housesales_test$bsmtfintype2 [is.na(housesales_test$bsmtfintype2 )] <- "None"

#install.packages("modeest")
library(modeest)
# Calculating the mode of the 'electrical' column
mode_value <- as.character(mlv(housesales_test$electrical))
housesales_test$electrical[is.na(housesales_test$electrical)] <- mode_value

housesales_test$fireplacequ[is.na(housesales_test$fireplacequ)] <- "None"

housesales_test$garagetype[is.na(housesales_test$garagetype)] <- "None"
housesales_test$garageyrblt[is.na(housesales_test$garageyrblt)] <- "None"
housesales_test$garagefinish[is.na(housesales_test$garagefinish)] <- "None"
housesales_test$garagequal[is.na(housesales_test$garagequal)] <- "None"
housesales_test$garagecond[is.na(housesales_test$garagecond)] <- "None"

housesales_test$poolqc[is.na(housesales_test$poolqc)] <- "None"

housesales_test$fence[is.na(housesales_test$fence)] <- "None"
housesales_test$miscfeature[is.na(housesales_test$miscfeature)] <- "None" 
#Confirm imputation
sapply(housesales_test, function(x) sum(is.na(x)))

#Encoding Categorical variables
#Identifying categorical columns 
categorical_cols <- sapply(housesales_test, is.character) 
#Printing categorical column names
cat("Categorical columns:\n")
print(names(housesales_test)[categorical_cols])

# Converting categorical columns to factors
categorical_cols <- sapply(housesales_test, is.character)
for(col in names(housesales_test)[categorical_cols]) {
  
  housesales_test[[col]] <- as.factor(housesales_test[[col]])
  
  if (col %in% c(
    "mszoning", "street", "alley", "lotshape", "landcontour", "utilities", "lotconfig",
    "landslope", "neighborhood", "condition1", "condition2", "bldgtype", "housestyle", "roofstyle",
    "roofmatl", "exterior1st", "exterior2nd", "masvnrtype", "exterqual", "extercond", "foundation",
    "bsmtqual", "bsmtcond", "bsmtexposure", "bsmtfintype1", "bsmtfintype2", "heating", "heatingqc",
    "centralair", "electrical", "kitchenqual", "functional", "fireplacequ", "garagetype", "garageyrblt",
    "garagefinish", "garagequal", "garagecond", "paveddrive", "poolqc", "fence", "miscfeature",
    "saletype", "salecondition"
  )){
    
    housesales_test[[col]] <- relevel(housesales_test[[col]], ref = levels(housesales_test[[col]])[1])
    
    
  }  else {
    
    housesales_test[[col]] <- relevel(housesales_test[[col]], ref = levels(housesales_test[[col]])[1])
    
  }
  
}
# Label Encoding
label_encoding <- c(Ex = 1, Gd = 2, TA = 3, Fa = 4, Po = 5)
housesales_test$encodedexterqual <- as.numeric(factor(housesales_test$exterqual, levels = names(label_encoding), labels = label_encoding))
housesales_test$encodedextercond <- as.numeric(factor(housesales_test$extercond, levels = names(label_encoding), labels = label_encoding))
housesales_test$encodedheatingqc <- as.numeric(factor(housesales_test$heatingqc, levels = names(label_encoding), labels = label_encoding))
housesales_test$encodedkitchenqual <- as.numeric(factor(housesales_test$kitchenqual, levels = names(label_encoding), labels = label_encoding))

label_encoding_1 <- c(Ex = 1, Gd = 2, TA = 3, Fa = 4, Po = 5, None = 6)
housesales_test$encodedbsmtqual <- as.numeric(factor(housesales_test$bsmtqual, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_test$encodedbsmtcond <- as.numeric(factor(housesales_test$bsmtcond, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_test$encodedfireplacequ <- as.numeric(factor(housesales_test$fireplacequ, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_test$encodedgaragequal <- as.numeric(factor(housesales_test$garagequal, levels = names(label_encoding_1), labels = label_encoding_1))
housesales_test$encodedgaragecond <- as.numeric(factor(housesales_test$garagecond, levels = names(label_encoding_1), labels = label_encoding_1))

label_encoding_2 <- c(Gd = 1, Av = 2, Mn = 3, No = 4)
housesales_test$encodedbsmtexposure <- as.numeric(factor(housesales_test$bsmtexposure, levels = names(label_encoding_2), labels = label_encoding_2))

label_encoding_3 <- c(Ex = 1, Gd = 2, TA = 3, Fa = 4, None = 5)
housesales_test$encodedpoolqc <- as.numeric(factor(housesales_test$poolqc, levels = names(label_encoding_3), labels = label_encoding_3))
View(housesales_test)
#Checking categorical columns 
categorical_cols <- sapply(housesales_test, is.character) 
#Printing categorical column names
cat("Categorical columns:\n")
print(names(housesales_test)[categorical_cols])
# Droping  categorical columns for which already new columns have been created
# Specify the columns to be removed
columns_to_remove <- c("exterqual", "extercond", "bsmtqual", "bsmtcond", "bsmtexposure",
                       "heatingqc", "kitchenqual", "fireplacequ", "garagequal", "garagecond", "poolqc")
# Remove the specified columns
housesales_test <- housesales_test[, -which(names(housesales_test) %in% columns_to_remove)]
# Total square footage 
housesales_test$totalsqft <- housesales_test$grlivarea + housesales_test$totalbsmtsf
# Total bathrooms
housesales_test$totalbaths <- housesales_test$fullbath + 0.5*(housesales_test$halfbath)
# Age of home
housesales_test$age <- housesales_test$yrsold - housesales_test$yearbuilt 
# Total porch sqft
housesales_test$totalporchsqft <- housesales_test$openporchsf + housesales_test$enclosedporch + 
  housesales_test$x3ssnporch + housesales_test$screenporch
# Total parking size 
housesales_test$totalparkingsize <- housesales_test$garagearea+ housesales_test$wooddecksf
# Has 2nd floor 
housesales_test$hassecondfloor <- ifelse(housesales_test$x2ndflrsf > 0, 1, 0)
# Lot size square 
housesales_test$lotsizesq <- housesales_test$lotfrontage * housesales_test$lotarea
# New construction
housesales_test$newconstruction <- ifelse(housesales_test$yearbuilt == housesales_test$yrsold, 1, 0)
# Write the cleaned data frame to a csv file after feature engineering
write.csv(housesales_test, file = "feature_engineering_housesales_test_cleaned.csv", row.names = FALSE)
View(housesales_test)
# Extract ID column from test data
test_id <- housesales_test$id
test_id
# Make predictions
test_preds <- predict(rf_model, housesales_test[features_rf])
# Create dataframe with ID and predictions
test_out <- data.frame(Id = test_id, SalePrice = test_preds)
# Write CSV file 
write.csv(test_out, file = "test_predictions.csv", row.names=FALSE)
#--------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------- 
