#### Report for Task 4:

The solution has three jupyter notebooks:

1. Pickler: Raw data is assigned to X_train, y_train and X_test numpy arrays and saved as .npy files.

2. Preprocessing: It is applied to both train and test. It generates corresponding features based on the provided mutations.

Firstly, the strings are split into four columns that correspond to each site where mutations of interest occur. Secondly, to handle the categorical character of the data, each column is one-hot-encoded.

3. Models: Train data is split 80% - 20% into train and validation.

As a baseline, I trained a random forest classifier. I used grid search with cross validation to find good values for the number of estimators and the maximum depth of the trees. To account for the class imbalance, I have also assigned different class weights corresponding to the ratio between the active and inactive observations. Although promising, the best f1 score on validation was 0.818, which is below the required baseline of 0.852.

After researching a number of potential models to use next, I settled onto XGBoost. I used a similar approach as above, employing grid search cross validation to identify suitable parameters. I tuned the learning rate, the maximum depth and the number of estimators. This returned a maximum f1 score of 0.891 on my validation dataset. 
