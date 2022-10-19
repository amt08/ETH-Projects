from scipy.stats import laplace, norm, t
import math
import numpy as np
from scipy.special import logsumexp

VARIANCE = 2.0

normal_scale = math.sqrt(VARIANCE)
student_t_df = (2 * VARIANCE) / (VARIANCE - 1)
laplace_scale = VARIANCE / 2

HYPOTHESIS_SPACE = [norm(loc=0.0, scale=math.sqrt(VARIANCE)),
                    laplace(loc=0.0, scale=laplace_scale),
                    t(df=student_t_df)]

PRIOR_PROBS = np.array([0.35, 0.25, 0.4])


def generate_sample(n_samples, seed=None):
    """ data generating process of the Bayesian model """
    random_state = np.random.RandomState(seed)
    hypothesis_idx = np.random.choice(3, p=PRIOR_PROBS)
    dist = HYPOTHESIS_SPACE[hypothesis_idx]
    return dist.rvs(n_samples, random_state=random_state)


""" Solution """


def log_posterior_probs(x):
    """
    Computes the log posterior probabilities for the three hypotheses, given the data x

    Args:
        x (np.ndarray): one-dimensional numpy array containing the training data
    Returns:
        log_posterior_probs (np.ndarray): a numpy array of size 3, containing the Bayesian log-posterior probabilities
                                          corresponding to the three hypotheses
    """
    assert x.ndim == 1

    # TODO: enter your code here

    # normal
    n_lik = -(1/2) * len(x) * np.log(2 * np.pi * VARIANCE) - (1/(2 * VARIANCE)) * np.sum(x**2)
    n_prior = np.log(PRIOR_PROBS[0])

    # laplace
    l_lik = - len(x) * np.log(2 * laplace_scale) - (1/laplace_scale) * np.sum(np.abs(x))
    l_prior = np.log(PRIOR_PROBS[1])

    # student
    t_lik = len(x) * np.log(math.gamma((student_t_df + 1) / 2) / (np.sqrt(student_t_df * np.pi) * math.gamma(student_t_df/2))) - ((student_t_df + 1)/2) * np.sum(np.log(1 + (x ** 2) / student_t_df))
    t_prior = np.log(PRIOR_PROBS[2])

    norm_post = n_lik + n_prior
    lapl_post = l_lik + l_prior
    stud_post = t_lik + t_prior

    unnorm = np.array([norm_post, lapl_post, stud_post])

    log_sum_exp = np.max(unnorm) + logsumexp(unnorm - np.max(unnorm))

    log_p = np.array([norm_post - log_sum_exp, lapl_post - log_sum_exp, stud_post - log_sum_exp])

    assert log_p.shape == (3,)
    return log_p


def posterior_probs(x):
    return np.exp(log_posterior_probs(x))


""" """


def main():
    """ sample from Laplace dist """
    dist = HYPOTHESIS_SPACE[1]
    x = dist.rvs(1000, random_state=28)

    print("Posterior probs for 1 sample from Laplacian")
    p = posterior_probs(x[:1])
    print("Normal: %.4f , Laplace: %.4f, Student-t: %.4f\n" % tuple(p))

    print("Posterior probs for 100 samples from Laplacian")
    p = posterior_probs(x[:50])
    print("Normal: %.4f , Laplace: %.4f, Student-t: %.4f\n" % tuple(p))

    print("Posterior probs for 1000 samples from Laplacian")
    p = posterior_probs(x[:1000])
    print("Normal: %.4f , Laplace: %.4f, Student-t: %.4f\n" % tuple(p))

    print("Posterior for 100 samples from the Bayesian data generating process")
    x = generate_sample(n_samples=100)
    p = posterior_probs(x)
    print("Normal: %.4f , Laplace: %.4f, Student-t: %.4f\n" % tuple(p))


if __name__ == "__main__":
    main()
