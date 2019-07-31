# credit-risk-analytics (score card and application approval model)
Machine learning credit risk analytics model for scoring and application approval. Model is build on data provided by upGrad. Dataset contains demographics and Credit bureau data for 71295 applicants. 

# What has been done throught out analysis and model?
I have built machine learning model to predict the probability of applicant to become default using input variables. Different algorithms has been implemented and the one with best evaluation parameter is selected. 
After model selection, scorecard is defined and using trade off between credit loss improvement and revenue loss due to not giving loan to potential customer, we have decided particular cut off score. This cutoff score will be used to approve credit card for future applicants. 

# Tools and Skills
- R 
- EDA 
- Feature Transformation using WOE and IV (Information Value)
- Data cleaning and understanding
- Sampling technique for data balancing

# Files
- Credit Risk Analysis.r : Data understanding, Data cleaning, EDA, Feature Transformation and new dataset creation for model building.
- LogicticRegression.r : Model building and evaluation using logistic regression. 
- cloglog modeling.r : Model building and evaluation using cloglog function.
- sampling and Model.r: Sampling data using ROSE package to balance dataset. Since dataset is biased to good customer. Using ROSE package, data is sampled and again models are build using logistic regression, cloglog, decision tree and Random forest. This alos include the final model selection. 
- final_model_scorecard.r: Score card building, Rejected application and other financial analysis using final model.

# Evaluation Matrix Value At Cutoff (0.43)

- Accuracy: 65%
- Sensitivity: 63%
- Specificity: 65%
- AUC: 64%
- KS Statistics: 28% at 4th Decile

# Analysis 

All the analysis are explained in detail in the powerpoint presentation. 

