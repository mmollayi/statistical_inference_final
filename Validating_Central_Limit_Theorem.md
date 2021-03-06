---
title: "Validating Central Limit Theorem for Exponential Distribution"
author: "Mohsen Mollayi"
date: "May, 2017"
output:
  html_document:
    keep_md: yes
---

## Overview

The goal of this project is to demonstrate the central limit theorem in action. To achieve this, I will investigate the properties of the distribution of averages of 40 independent samples from an exponential distribution.

## Basic theory

The probability density function of the exponential distribution is
$$
f(x;\lambda) = \begin{cases}
\lambda e^{-\lambda x} & x \ge 0, \\
0 & x < 0.
\end{cases}
$$
Here $\lambda > 0$ is the parameter of the distribution, often called the *rate parameter*. The expected value of an exponentially distributed random variable $X$ is $\operatorname{E}[X] = \frac{1}{\lambda}$ and it's variance is given by $\operatorname{Var}[X] = \frac{1}{\lambda^2}$.

The central limit theorem states that the distribution of the average of a large number of independent, identically distributed random variables will be approximately normal, regardless of the underlying distribution. This result will be investigated for exponential variable.

## Simulations

In order to investigate the distribution of averages of 40 exponentials, I will generate 1000 random samples from this distribution. Based on this samples, I will estimate the distribution mean and variance and then will compare it with the theoretical mean and variance of the appropriate normal distribution.

### Sample Mean versus Theoretical Mean

The central limit theorem implies asymptotic normality of the sample mean as an estimator of the true mean; that is we expect that as the sample size gets large enough, the  distribution of sample mean can be approximated by a normal distribution whose expected value is equal to the mean of the underlying distribution. Therefore, theoretically the expected value of averages of 40 exponentials with $\lambda = 0.2$ is 5.

In order to show this, we generate 1000 random samples of averages of 40 exponentials:

```r
set.seed(172)
nosim <- 1000
lambda <- .2
sample_means <- apply(matrix(rexp(nosim*40, lambda), nosim), 1, mean)
estimated_mean <- mean(sample_means)
estimated_mean
```

```
## [1] 4.929593
```
So our observed sample mean equals to 4.93 which is close to 5.

### Sample Variance versus Theoretical Variance

Another consequence of the CLT is that for large enough sample sizes, the variance of the sample mean is approximately equal to the variance of the underlying distribution divided by the sample size.


```r
sample_var <- var(sample_means)
sample_var
```

```
## [1] 0.5913148
```
For our observed sample, sample variance equals 0.591 while our theoretical target is 0.625.

Figure 1 compares the variability of 1000 random samples from exponential distribution with 1000 random samples from average of 40 exponentials. Reduction in data variability is noticeable in this figure and also symmetrical distribution of sample means is obvious while the exponential samples are skewed. For better comparison, 1000 samples from an appropriate normal distribution are also plotted.


```r
library(ggplot2)
exponentials <- rexp(1000, lambda)
normals <- rnorm(1000, mean = 1/lambda, sd = 1/(lambda*sqrt(40)))
random <- c(exponentials, sample_means, normals)
df <- data.frame(samples = random,
                 distribution = as.factor(rep(c("exponential", "sample mean", "normal"), each=1000)))

# vertical axis is limited to 20. thus, some exponential samples are not plotted
qplot(distribution, samples, data=df,
      geom = "boxplot", fill = distribution, ylim = c(0,20), show.legend = FALSE)
```

<img src="Validating_Central_Limit_Theorem_files/figure-html/figure_1-1.png" title="Figure 1: Sample means are symmetric and less variable than exponential samples." alt="Figure 1: Sample means are symmetric and less variable than exponential samples." style="display: block; margin: auto;" />

### PDF of averages of 40 exponentials

In order to estimate the probability density function of the mean of 40 exponentials, I will use a normalized histogram. We expect the obtained histogram to look normal with appropiate center of mass and variability. for reference I also drew the density of a normal distribution with mean 5 and variance 0.625. For comparison, a histogram of a random sample of 1000 points from an exponential distribution with mean equal to 5 and varaince equal to 25 is also plotted.


```r
library(gridExtra)
plot1 <- qplot(exponentials, geom = "histogram", boundary = 0,
               binwidth = 1.5, colour = I("black"), fill = I("grey"), y = ..density..,
               xlab = "random exponentials")

plot2 <- ggplot(data.frame(sample_means), aes(x = sample_means)) + 
    geom_histogram(alpha = .2, binwidth = .2, colour = "black", aes(y = ..density..)) +
    labs(x = "averages of 40 random exponentials") +
    stat_function(fun = dnorm, size = 2, args = list(mean = 5, sd = 5/sqrt(40))) +
    geom_vline(xintercept = estimated_mean, size = 1, color = "blue") +
    annotate("text", x = 5.6, y = .57, label = "sample mean", colour = "blue")

grid.arrange(plot1, plot2, nrow=1)
```

<img src="Validating_Central_Limit_Theorem_files/figure-html/figure_2-1.png" title="Figure 2: Distribution of averages of 40 exponentials looks far more
 Gaussian than the original exponentials distribution." alt="Figure 2: Distribution of averages of 40 exponentials looks far more
 Gaussian than the original exponentials distribution." style="display: block; margin: auto;" />
   
   



