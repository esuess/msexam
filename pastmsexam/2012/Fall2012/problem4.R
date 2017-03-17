### Problem 4

# plot the beta p.d.f 

theta = 10

alpha = theta + 3
beta = 1

x = seq(0,1,0.001)

X11()
curve(dbeta(x, alpha, beta), 0, 1)

# simulate a dataset of size n

n = 15

x.data = rbeta(n, alpha, beta)

hist(x.data)

theta.mle =   ENTER THE MLE OF THETA HERE  
theta.mle		     

# approximate 100(1-alpha)% CI for theta, use alpha = 0.05

theta.mle + qnorm(c(0.025,0.975))*( (theta.mle + 3)/sqrt(n) )

############################################################################
# parametric bootstrap

B = 10000

theta.mle.boot = numeric(B)

alpha.star = theta.mle + 3
beta.star = 1

for(i in 1:B){
	x.boot = rbeta(n, alpha.star, beta.star)
	theta.mle.boot[i] =   ENTER THE MLE OF THETA HERE 
}                                     

X11() 
plot(density(theta.mle.boot, n=50, window="g"),type="l",
        xlab="x",ylab="density")

theta.mle.boot.ci = quantile(theta.mle.boot,c(0.025,0.975))
theta.mle.boot.ci






