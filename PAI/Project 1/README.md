### Task 1:

Gaussian Process regression to model air pollution and predict fine particle concentration at new coordinate locations.

### Report for task 1:

For this task I started with some EDA to understand how the data was visually distributed - everything seemed normal and so I continued exploring some ways of applying low-rank approximation methods together with the GPR(). Even with applying the RBF sampler or the Nystrom approximation it took quite long to run during the model selection and hyperparameter optimization phase, so I decided to randomly sample from the training data a smaller set to iterate faster.

Based on a much smaller sample I split the data into train and validation to do model selection + hyperparameter optimization and evaluated the cost function using the validation set.

Instead of doing an automated kernel selection, I resorted to trying a few combinations of kernels (see jupyter notebook). 

I ended up using a RBF Kernel with a WhiteKernel which gave a low cost and ran fast. Additionally, I have done 2 modifications to the predicted mean, namely to correct for the cases where the model pays a lot for small mistakes - for cases where the prediction is very close to the threshold, but slightly below it. I identified the cases where the difference between the threshold and the predictions was in the interval 0-5 (ie. just slightly off) and re-assigned to those predictions the value of the threshold, which reduced the cost decently. Further, I made a constant shift to the predictions by subtracting 2 in order to account for the case when we pay 5 for overpredicting and that reduced the cost a bit more.