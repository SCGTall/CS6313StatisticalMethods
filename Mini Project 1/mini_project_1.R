# Question 1
# (a) Use the above density function to analytically compute the probability
# that the lifetime of the satellite exceeds 15 years.

# See solution in document

# (b) Use the following steps to take a Monte Carlo approach to compute E(T)
# and P(T > 15)

# i. Simulate one draw of the block lifetime Xa and Xb. Use these draws to
# simulate one draw of the satellite lifetime T.

lambda = 1 / 10
xa = rexp(n=1, rate=lambda)
xb = rexp(n=1, rate=lambda)
# we can also use runif to build a Exponential Distribution
myExp <- function(n, l){
  unif = runif(n, 0, 1)
  -log(1 - unif) / l
}
# xa = myExp(n=1, l=lambda)
# xb = myExp(n=1, l=lambda)
T0 = max(xa, xb)
sprintf('xa = %f, xb = %f, T = %f', xa, xb, T0)

# ii. Repeat the previous step 10000 times. THis will give you 10000 draws from
# the distribution of T. Try to avoid 'for' loop. Use 'replicate' function
# instead. Save these draws for reuse in later steps. [Bonus: 1 bonus point for
# not taking more than 1 line of code for steps (i) and (ii).]

Ts = replicate(10000, max(rexp(n=1, rate=lambda), rexp(n=1, rate=lambda)))

Ts# last line has finished (i) and (ii) in one line. This line is use to show Ts

# iii. Make a histogram of the draws of T using 'hist' function. Superimpose
# the density function given above. Try using 'curve' function for drawing the
# density. Note what you see.

itl = 3# interval
hist(Ts, probability = T, breaks=seq(0, max(Ts)+itl, itl), xlab='T', main='Histogram of T')
pdf = function(x){
  0.2 * exp(-0.1 * x) - 0.2 * exp(-0.2 * x)
}
curve(pdf(x), add = T, from=0, to=max(Ts)+itl, xname = 'x', xlab='T', ylab='P')

# iv. Use the saved draws to estimate E(T). Compare your answer with the exact
# answer given above.

et = mean(Ts)# compare to 15
sprintf('E(T)= %f', et)

# v. Use the saved draws to estimate the probability that the satellite lasts
# more than 15 years. Compare with the exact answer computed in part(a).

p15 = sum(Ts > 15) / 10000# compare answer (a)
sprintf('P(T>15) = %f', p15)

# vi. Repeat the above process of obtaining an estimate of E(T) and an estimate
# of the probability four more times. Note what you see.

simulate = function(n, l, i){
  Ti = replicate(n, max(rexp(n=1, rate=lambda), rexp(n=1, rate=lambda)))
  hist(Ti, probability = T, breaks=seq(0, max(Ti)+i, i), xlab='T', main='Histogram of T')
  curve(pdf(x), add = T, from=0, to=max(Ti)+i, xname = 'x', xlab='T', ylab='P')
  c(mean(Ti), sum(Ti > 15) / n)
}
replicate(4, simulate(n=10000, l=lambda, i=itl))

# (c) Repeat part (vi) five times using 1000 and 100000 Monte Carlo replications
# instead of 10000. Make a table of results. Comments on what you see and
# provide an explanation

replicate(5, simulate(n=1000, l=lambda, i=itl))
replicate(5, simulate(n=100000, l=lambda, i=itl))

# Question 2
# Use a Monte Carlo approach estimate the value of π based on 10000 replications.
# [Ignorable hint: First, get a relation between π and the probability that a
# randomly selected point in a unit square with coordinates 
# — (0,0), (0,1), (1,0), and (1,1)
# — falls in a circle with center (0.5,0.5) inscribed in the square.
# Then, estimate this probability, and go from there.]
r = 10000 # set up 10000 replications, the number of total random points
x = runif(r, min=0, max=1) # generate 10000 random values x between 0 and 1
y = runif(r, min=0, max=1) # generate 10000 random values y between 0 and 1
circle = (x - 0.5)**2 + (y - 0.5)**2 <= 0.5**2 # points fall into the circle
pi = sum(sum(circle) / r) * 4 # pi = 4 * (the probability that points fall into the circle)

print(pi)