### Task 1:
"Predicting the age of a brain from MRI features"

This task is primarily concerned with regression. However, we have perturbed the original MRI features in several ways. You will have to perform the following preprocessing steps:

    1. Outlier detection
    2. Feature selection
    3. Imputation of missing values

### Report for task 1:

I started the task with inputting the missing values by replacing the NaNs with the median of each column. 

For the outlier detection task, I used Isolation Forest on the full training set, which outputs a vector of 1’s and -1’s (denoting outliers). I decided to delete the rows which contained outliers as indicated by the algorithm.

For selecting the relevant features, I looked at the features that were most correlated with the outcome y and selected the ones whose correlation was greater than 0.1 in absolute value, therefore ending up with 206 features.

For this task I split my pre-processed training data into a train set for fitting and used a validation set to predict and assess the r2 score. The subsequent datasets (train and validation) were normalised to 0 mean and unit variance.

Finally, after trying many different algorithms, I ended up using an MLP with 4 hidden layers with ReLU activation and a linear activation at the end in order to fit the training data.  I have chosen Adam as the optimizer and the mean absolute error as the loss. This MLP has been trained for 40 epochs.

Finally, I have applied the same transformations to the test set (except detecting / deleting outliers) and predicted the values for the age.
