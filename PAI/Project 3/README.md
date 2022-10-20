### Task 3:

Bayesian optimisation under constraints following Gelbart et al. (2014).[[1]](#1).

### Report for task 3:

I defined the constraint and the objective model as indicated in the project description. I also added a WhiteKernel to capture the noise in the data.

To recommend the next point, I call the optimize acquisition function, but also sample a point in the interval [0,6] when we start with no previous points.

In the acquisition function, I compute the predictive mean and variance at the new x. I also compute the optimal f evaluated at the previously sampled points. I chose as acquisition function the expected improvement, computed as a combination of the cdf and pdf of the difference between the optimal f and the predicted mean at the new point, scaled by the predicted standard deviation at the new x. To take the constraint into account, I weighted the EI by the cdf at lambda (Pr(c(x) <= lambda), with mean and standard deviation predicted at the new x by the constraint model. If the objective model’s predicted standard deviation at the new point is 0, the acquisition function returns 0, otherwise it returns the constraint weighted EI.

The optimal x is obtained by collecting both function evaluations at the previous points. I then attributed a large number to all function values f, where the constraint is not fulfilled, such that when searching for the optimal x, we don’t consider the cases where the constraint is violated. Therefore the optimal x is returned as the minimizer of the f values excluding the cases when c(x) is greater than lambda.

### References
<a name="1">[1]</a>
Gelbart, M.A., Snoek, J. and Adams, R.P., 2014. Bayesian optimization with unknown constraints. In Proceedings of the Thirtieth Conference on Uncertainty in Artificial Intelligence (UAI'14). AUAI Press, Arlington, Virginia, USA, 250–259.



