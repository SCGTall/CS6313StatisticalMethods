# Question 1
# Compare two estimators.

# (a) Explain how you will compute the mean squared error of an estimator using
# Monte Carlo simulation.


# (b) For a given combination of (n, θ), compute the mean squared errors of both
# θ1 and θ2 using Monte Carlo simulation with N = 1000 replications. Be sure to
# compute both estimates from the same data.

n = 30
theta = 1
N = 1000
estimator <- function(n, theta){
  xs = runif(n, min=0, max=theta)
  c((max(xs)-theta)^2, (2*mean(xs)-theta)^2)
}
MSE <- function(N, n, theta){
  res <- replicate(N, estimator(n, theta))
  c(mean(res[1,]), mean(res[2,]))
}
MSE(N, n, theta)# (MLE, MME)

# (c) Repeat (b) for the remaining combinations of (n,θ). Summarize your results
# graphically.

ns = c(1, 2, 3, 5, 10, 30)
thetas = c(1, 5, 50, 100)
res.mle = matrix(0, ncol=length(thetas), nrow=length(ns))
res.mme = matrix(0, ncol=length(thetas), nrow=length(ns))
for (i in seq_along(ns)){
  for (j in seq_along(thetas)){
    tmp = MSE(N, ns[i], thetas[j])
    res.mle[i,j] = tmp[1]
    res.mme[i,j] = tmp[2]
  }
}

# different n, one theta
for (i in seq_along(ns)){
  lim = c(0, max(max(res.mle[i,]), max(res.mme[i,]))*1.1)
  plot(x=thetas, y=res.mle[i,], type="b", xlab="theta", ylab="MSE", ylim=lim,
       col="red", main=paste("MSE ( n =",ns[i],")"))
  lines(x=thetas, y=res.mme[i,], type="b", col="blue")
  legend("top", inset=.05, title="Estimator", c("MLE","MME"),
       fill=c("red", "blue"), horiz=TRUE)
}
# different theta, one n
for (j in seq_along(thetas)){
  lim = c(0, max(max(res.mle[,j]), max(res.mme[,j]))*1.1)
  plot(x=ns, y=res.mle[,j], type="b", xlab="n", ylab="MSE", ylim=lim,
       col="red", main=paste("MSE ( theta =",thetas[j],")"))
  lines(x=ns, y=res.mme[,j], type="b", col="blue")
  legend("top", inset=.05, title="Estimator", c("MLE","MME"),
       fill=c("red", "blue"), horiz=TRUE)
}

# (d) Based on (c), which estimator is better? Does the answer depend on n or θ?
# Explain. Provide justification for all your conclusions.

# about theta
for (i in seq_along(ns)){
  y1=res.mle[i,]/((thetas)^2)
  y2=res.mme[i,]/((thetas)^2)
  lim = c(0, max(max(y1), max(y2))*2)
  plot(x=thetas, y=y1, type="b", xlab="theta", ylab="MSE/theta^2", ylim=lim,
       col="red", main=paste("MSE/theta^2 ( n =",ns[i],")"))
  lines(x=thetas, y=y2, type="b", col="blue")
  legend("top", inset=.05, title="Estimator", c("MLE","MME"),
         fill=c("red", "blue"), horiz=TRUE)
}

# Question 2
# Calculate MLE and CI

# (a) Derive an expression for maximum likelihood estimator of θ.


# (b) Suppose n = 5 and the sample values are x1 = 21.72, x2 = 14.65, x3 = 50.42,
# x4 = 28.78, x5 = 11.23. Use the expression in (a) to provide the maximum
# likelihood estimate for θ based on these data.


# (c) Even though we know the maximum likelihood estimate from (b), use the data
# in (b) to obtain the estimate by numerically maximizing the log-likelihood
# function using optim function in R. Do your answers match?

# we will work with the lifetime data x in the question
x <- c(21.72, 14.65, 50.42, 28.78, 11.23)

# Negative of log-likelihood function

neg.loglik.fun <- function(par, dat){
  result <- length(dat)*log(par)-(par+1)*sum(log(dat))
  return(-result)
}

# Minimize -log(L), i.e., maximize log(L)

ml.est <- optim(par=0.1, fn=neg.loglik.fun, method="L-BFGS-B", lower=0.1,
                hessian=TRUE, dat=x)
print(ml.est)

# (d) Use the output of numerical maximization in (c) to provide an approximate
# standard error of the maximum likelihood estimate and an approximate 95%
# confidence interval for θ. Are these approximations going to be good? Justify
# your answer.

# standard errors
SE <- sqrt(solve(ml.est$hessian))[1]# announce [1] here is to avoid warning in R

# The confidence interval
ml.est$par + c(-1,1)*SE*qnorm(0.975)
