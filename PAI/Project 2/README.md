### Task 2:

Multi-class classification on the MNIST dataset by implementing a bayesian neural network based on the "Bayes by Backprop" algorithm proposed by Blundell et al. (2015) [[1]] (#1).

### Report for task 2:

In the Bayesian layer, the prior was initialised as a mixture of 2 Gaussians with pi = 0.5, sigma1 = 1.5 and sigma2 = 0.5 chosen after running the model with different values. The weights and bias means were initialised based on a normal distribution with 0 mean and std = sqrt(6 / no of incoming features) and the rhos were initialised with values from a normal distribution with mean = -6 and std = 0.5. These last values gave the best score when running the model multiple times using several candidate values. In the forward pass I computed the weights by sampling from a standard normal and multiplying it with the softplus transformation of rho and finally adding mu. The same process was done for the bias. The log prior was calculated by evaluating the log likelihood of the prior at the weights and adding the log likelihood of the prior evaluated at the bias. The log variational posterior was computed in the same manner.

The BayesNet forward method is using the DenseNet one, but it is also adding the log prior and the log variational posterior for each layer and returns them with the output features.

In Model, I’m calling the forward method of the Bayesian Network and collecting the current logits, the log prior and posterior for the batch. I constructed the loss as the sum of the negative log likelihood loss at the log softmax of the current batch logits and the difference between the log posterior and the log prior scaled by the total number of batches, as suggested in the paper.

### References
<a name="1">[1]</a> 
Blundell, C., Cornebise, J., Kavukcuoglu, K. and Wierstra, D., 2015, June. Weight uncertainty in neural network. In International conference on machine learning (pp. 1613-1622). PMLR.


