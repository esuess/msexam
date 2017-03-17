### Maximum Likelihood Estimates for the Weibull Distribution

set.seed(1234)

# simulate the data  RUN THIS PART OF THE CODE ONLY ONCE

alpha = 1   # shape
beta = 5    # scale	

n = 100

y = rweibull(n, alpha, beta)

mean(y);var(y); sd(y)

# plot the simulated data

plot(density(y))
rug(y)

#########################################################################

# evaluate the log Likelihood at two different values of beta

logL.5 = sum(dweibull(y, shape = 1, scale = 5, log = TRUE))
logL.5

logL.7 = sum(dweibull(y, shape = 1, scale = 7, log = TRUE))
logL.7

# minimize -logL

logL = function(x){
      -sum(dweibull(y, shape = x[1], scale = x[2], log = TRUE))
}

y.mle = nlm(logL, c(shape = 1, scale = 5), hessian = TRUE)
y.mle

# Asymptotic variance 

AV = solve(y.mle$hessian); AV 

# contour plot

len = 101

grid = matrix(0.0, nrow = len, ncol = len)
shape.vals = seq(0, 2, len = len)
scale.vals = seq(3, 10, len = len)

for(i in seq(along = shape.vals)){
      for(j in seq(along = scale.vals)){
            grid[i,j] = logL(c(shape.vals[i],scale.vals[j]))
      }
}
contour(shape.vals, scale.vals, grid, levels = 200:300)
points(y.mle$estimate[1],y.mle$estimate[2], pch = "+")
title(main = "log-Likelihood contours, Weibull", xlab = expression(alpha), 
      ylab = expression(beta))

# plot fitted weibull model

plot(function(y) dweibull(y, shape = y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, max(y), xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")
rug(y)

# CI alpha

y.mle$estimate[1]

y.mle$estimate[1] - 1.96*sqrt(AV[1,1]); y.mle$estimate[1] + 1.96*sqrt(AV[1,1])

# CI beta

y.mle$estimate[2]

y.mle$estimate[2] - 1.96*sqrt(AV[2,2]); y.mle$estimate[2] + 1.96*sqrt(AV[2,2])

#########################################################################

# Test H_0: alpha = 1 versus H_1: alpha != 1

# Fix alpha = 1, examine the profile likelihood of beta

# examine the profile log-Likelihood of beta for fixed alpha = 1

profile.logL = function(beta){
      -sum(dweibull(y, shape = 1, scale = beta, log = TRUE))
}

# plot the profile likelihood for beta

scale.index = seq(3,15,0.1)

yy = numeric()

for(i in 1: length(scale.index)) {
      yy[i] = profile.logL(scale.index[i])
}

plot(scale.index, yy, xlab = expression(beta), ylab = "-logL", 
      main = "-log-Likelihood, Weibull")

# minimize -logL

y.mle2 = nlm(profile.logL, c(beta = 4.0), hessian = TRUE)

y.mle2

# CI beta

y.mle2$estimate

y.mle2$estimate - 1.96*sqrt(y.mle2$hessian^-1)
y.mle2$estimate + 1.96*sqrt(y.mle2$hessian^-1)

# Test H_0: beta = 5 versus H_1: beta != 5




