# credit-risk-analytics
Machine learning credit risk analytics model for scoring and application approval system. Model is build on data provided by upGrad. Dataset contains demographics and Credit bureau data for over 71295 applicants. 

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

All the analysis are provied in details in the power point file in the repository. 

