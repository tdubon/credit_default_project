# credit_default_project
This project addresses a binary classification problem to predict the default of consumer loans - credit risk modeling. The data is from the UCI Machine Learning Repository: 
https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients

Features were engineered from the original variables, such as average utility, average bill amount, maximum utility, maximum delinquency, etc., to boost predictive capacity.

An Extreme Gradient Boosting model was trained using variables identified as important by the results of a Random Forest model that assessed the complete suite of variables, and it produced the best results. It had a Type I Error of .16, an AUC of .76, and a Sensitivity of .68. Overall these results were satisfactory, depending on the specific use of the model (exploratory or to release into production) and the acceptable accuracy threshold for Type I and Type II errors.

The approach was developed under the guidance of the professor. The corresponding reports are also included.
