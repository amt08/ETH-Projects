### Task 4:

The task is to control a lunar lander such that it descends smoothly on the ground, between the two yellow flags. This has to be done using minimal fuel and without damaging the lander. This is a model-free policy gradient approach which employs two neural networks, the Actor and the Critic. The solution implements policy gradients with Generalised Advantage Estimation (GAE), as presented in Schulman et al. (2015)[[1]](#1).

### Report for task 4:

I first sampled an action from the categorical distribution resulting from the forward pass in Actor, then ran the forward pass in Critic to obtain the value function at the given state and computed the log probability given the distribution of actions and the action sampled.

To update the policy, I computed the loss as the negative sum of the product between the log probability of the actions in the buffer and the returns. In stage 3 of the project (GAE), I replaced the returns with the advantage stored in the buffer. 

In the buffer, after the trajectory ended, I assigned all the discounted returns from discount_cumsum, instead of taking only the first value which was the total discounted reward and replicating it.

I updated the value function based on a loss equal to the negative sum of the product between the log probabilities over actions and the advantage function phi.

The advantage function was calculated as the discounted sum of the temporal difference residuals, whose discount is the product of gamma and lambda. The residual terms were obtained as the linear combination of rewards and value functions. Then the advantage was normalized by subtracting the mean and scaling it by the standard deviation.

I also updated the max length of an episode to 250, since the lander was still hovering above the ground for some time after landing, decreased the number of steps per epoch to 2500 and increased the number of epochs to 70, which increased my score.

### References
<a name="1">[1]</a>
Schulman, J., Moritz, P., Levine, S., Jordan, M. and Abbeel, P., 2015. High-dimensional continuous control using generalized advantage estimation. arXiv preprint arXiv:1506.02438.



