###
# REMIND ME IF I FORGET, MAKE THE REPOSITORY PUBLIC ON SUBMISSION DAY
###

###
# FOR DETAILS ON WHY METHODS WERE CHOSEN GO TO THE BOTTOM OF THIS README
###

# MEMBERS
Ning Han
Tommy Jun
Justin Niestroy

# GOALS
Everyone should contribute as much as they can. Ideally reaching the minimal goals + 1 model for each person.

Have at LEAST one parametric model:
1. Multiple Linear Regression

Have at LEAST one non-parametric model:
1. kNN
2. Random Forest

# OPTIONAL
Get a good score on the kNN modeling for extra credit.

# RESULTS
Multiple Linear Regression: 0.12564
kNN: 0.15022
Random Forest: 0.14151

# FILES
explore.R: Exploratory analysis and data cleaning.
linear_model.R: Multiple linear regression.
knn.R: k-Nearest Neighbors regression.
forest.R: Random forest regression.

cleaned_data: All of the cleaned data.
original_files: All of the original files from Collab.
final_submission_results: All of the final submissions for Kaggle as well as plots.

FINAL_SCORES.png: Final score table.

# EXPLANATION/TUTORIAL
Here is a step-by-step process/overview of the entire process.

explore.R: Exploratory analysis on the data and data cleaning
1. Read in the data (train and test). Do a few checks to see what kind of data is being read in.
2. Data cleaning.
	- Check the proportion of missing values.
	- Remove anything that isn't a predictor (ID).
3. Outliers.
	- Make some plots for all of the variables to visually scan for outliers. There were a few outliers so remove those rows.
4. Imputation. There are a number of methods for imputation. The ones used in this project are:
	- Regression (i.e. Use sale price to make a linear model to predict another variable)
	- Replacement (i.e. There was a year value of 2207 which should have been 2007)
	- Mode (i.e. Replace missing values with the highest frequency level)
	- Factor level (i.e. Replace NAs with None when relevant)
5. Organization. Set variables as either numerical, factor, or ordinal.
6. General analysis. Perform some analysis on the data looking at:
	- Plots
	- Summary statistics
	- Skew
	- Log (or Box-Cox) transformations
7. Interacting variables. Wherever it makes sense create variables which hold information from other variables.
8. One-Hot Encoding. Make dummy variables for non-ordinal categorical variables. (And also a second table which isn't encoded)
9. Print
10. Feature selection via Recursive Feature Elimination. Checks for the best features to use as well as their relationship to RMSE.

linear_model.R: Multiple Linear Regression
1. RUN EXPLORE.R (At least up to part 9)
2. Use BIC and AIC as metrics for LINEAR feature selection. Test different models using this via lm(...)
   (For parts 2.2 and 2.5 this is particularly partial one-hot-encoded data)

forest.R: Random Forest
1. RUN EXPLORE.R
2. Pass
3. Use randomForest package in order to make two different models.
	- light.rf utiliitzes only some of the predictors found in part 10 of explore.R
	- heavy.rf utilitzes several of them up to about how many are relevant from BIC in the linear case
	- heaviest.rf utilizes all predictors

knn.R: K-Nearest Neighbors
1. RUN EXPLORE.R AND LINEAR_MODEL.R (at least the parts indicated)
2. Feature selection. Perform recursive feature selection again for the data with CATEGORICAL DATA removed.
   Calculate a vector which includes variable importance as determined by %delta RMSE
3. All the functions from scratch including implementation of knn
4. Run grid search and CV to determine which model performs the best on the validation set. Make models according to the results of hyperparameter tuning
EXTRA: Experimental code, not ready to be used, so it's commented out.






