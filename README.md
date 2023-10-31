# House Sales Price Prediction

## Overview
This project aims to predict house sale prices using regression techniques on the Ames Housing Dataset. The Ames Housing Dataset contains details of residential homes sold in Ames, Iowa from 2006 to 2010.
[Kaggle Competition](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/overview)

## Data
The data contains 79 explanatory variables describing various attributes of residential homes. The target variable is SalePrice. The train and test datasets are provided as:
- train.csv
- test.csv
- [Download Dataset](https://www.kaggle.com/c/house-prices-advanced-regression-techniques)

  
## Methods
The following steps were taken in this analysis:

### Data Exploration
- Summary statistics
- Visualizations

### Data Cleaning
- Handle missing values
- Encode categorical variables

### Feature Engineering
- Create new features like total sqft, age, total baths, etc.

### Model Building
- Split data into train/valid sets
- Build linear regression, random forest models
- Compare RMSE

### Model Improvement
- Remove outliers
- Retrain models
- Compare performance

### Make Predictions
- Preprocess test data
- Make predictions with the best model
- Write predictions to CSV file

## Key Findings
- Random forest model performs better than linear regression
- Removing outliers improves model performance
- Most important variables: Overall quality, Gr liv area, Neighborhood

## Conclusion
A random forest model was able to effectively predict house sale prices on this dataset after data cleaning, feature engineering, and hyperparameter tuning. The best RMSE achieved was approximately $22K.

## Next Steps
- Try neural network models like MLP
- Optimize random forest hyperparameters
- Experiment with Gradient Boosting Machines
- Engineer more features

