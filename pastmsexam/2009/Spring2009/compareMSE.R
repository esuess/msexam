### Compare the MSEs of the MME, MLE, and corrected MLE
### for theta, sampling from the Unif(0,theta).

theta = 1
n = c(1:20)

MSE.MME = theta/(3*n)
X11()
plot(MSE.MME,type="l",ylim=c(0,.35),ylab="MSE")

MSE.MLE = 2*theta^2 / ( (n+2)*(n+1) )
par(new=T)
plot(MSE.MLE,type="p",ylim=c(0,.35),ylab=" ")

MSE.MLE.unbiased = theta^2 / ( n*(n+2) )
par(new=T)
plot(MSE.MLE.unbiased,type="b",ylim=c(0,.35),ylab=" ")

# Parametric Bootstrap

# Consider a random sample from Unif(0,theta)

n = 20
theta = 1
x = runif(n, min=0, max=theta)
x.original = x

theta.tilde = 2* mean(x)
theta.tilde

theta.hat = max(x)
theta.hat

theta.hat.unbiased = ((n+1)/n)*theta.hat
theta.hat.unbiased

theta.hat.unbiased = ((n+1)/n) * theta.hat
theta.hat.unbiased

# Parametric Bootstrap of the MLE

B = 1000
theta.mme.star = numeric(B)
for(i in 1:B){
	x = runif(n, min=0, max=theta.tilde)
	theta.mme.star[i] = 2*mean(x)
}

par(mfrow=c(2,2))

hist(theta.mme.star)

# Parametric Bootstrap of the MLE

B = 1000
theta.mle.star = numeric(B)
for(i in 1:B){
	x = runif(n, min=0, max=theta.hat)
	theta.mle.star[i] = max(x)
}

hist(theta.mle.star)

# Parametric Bootstrap of the corrected unbiased MLE

B = 1000
theta.mle.unbiased.star = numeric(B)
for(i in 1:B){
	x = runif(n, min=0, max=theta.hat.unbiased)
	theta.mle.unbiased.star[i] = ((n+1)/n)*max(x)
}

hist(theta.mle.unbiased.star)