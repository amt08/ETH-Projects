### Report for Task 1:

In this assignment, I tried different built-in methods in Python, however, the solution yielding the best score is the manual coding of the K-fold CV.

The submission has 3 jupyter notebooks. By running the pickler file, the data is split into two arrays: X (predictors) and y (targets). The arrays are then saved as .npy files.

The preprocessing file aims to prepare the dataset for modeling but as the task requires none, it is just saving the X array into a new file named X_preprocessed.npy. This notebook still needs to run in order to be able to run the models file.

In the models file, firstly I performed a permutation of the rows of the training set (X and y) to account for the fact that datasets can follow certain patterns and so we want to make sure that folds don’t contain data of just one kind when split. 

Moreover, I created the rmse_cv function which iterates over the ten folds. The function selects a subset of the permuted indexes such that, with each fold, a new subset of indexes is chosen. I filter both X and y according to the subset of indexes to obtain both the train set (X_train, y_train) and the test set (X_test, y_test). The function then fits the Ridge model to the training set and then predicts y_hat based on the test set (X_test). I then score y_hat against the test set target (y_test) using RMSE. The function returns the average RMSE over the ten folds.
