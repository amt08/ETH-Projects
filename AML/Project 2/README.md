### Task 2:
Multiclass classification from ECG signals.

This task is about classifing ECG signals into 4 different classes.

### Report for task 2:

I first removed the NAs for each row and saved the data into a list. I then used the neurokit package to clean the raw ECG signal, by using ecg_clean. 

I extracted the R peaks and the heartbeats using biosppy. I aggregated the multiple templates of the heartbeats by calculating the mean, standard deviation and median per individual. I further collected the median, max and min of the aggregated heartbeat templates described above. Next, I extracted the mean, median, standard deviation, max and min of the R peaks. I used neurokit to extract time-domain indices of the heart rate variability, e.g. the mean, the std, the IQR of the RR intervals and many more, by using the hrv_time function. I also extracted the heart rate using biosppy and collected its mean, std and median. I identified the P, Q, R, S, T waves and the PR, QRS and ST segments. Finally, I extracted the approximation and the detail coefficients of the discrete wavelet transform by applying the db2 wavelet.

I split my data in a stratified fashion into train and validation (10%) after gathering the features mentioned above. I normalised both datasets and I performed hyperparameter tuning using 3-fold GridSearchCV on the xgboost classifier. I ended up using a model with 500 estimators but heavily regularised for training, which also gave a good score in validation. I performed the data cleaning and feature extraction for the test set in the same manner and predicted using the model which gave the best CV score.
