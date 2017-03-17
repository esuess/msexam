### bootstrap the UMVUE of P(X<=2) 

####################################################################
### simulate a random sample of size n from the Bin(m, theta)

n = 40	# sample size, X_1,...,X_n

m = 26; theta = 0.05

prob.true = (1-theta)^m + m*theta*(1-theta)^(m-1) + choose(m,2)*theta^2*(1-theta)^(m-2)
prob.true1 = pbinom(2,m,theta)
prob.true; prob.true1

x.orig = rbinom(n, m, theta)
x.orig

hist(x.orig)

suff.stat = sum(x.orig)

# MLE of theta and P(X<=2)

theta.hat = suff.stat/(n*m)	# UMVUE of theta
theta.hat

g_theta.hat = (1-theta.hat)^m + m*theta.hat*(1-theta.hat)^(m-1) + choose(m,2)*theta.hat^2*(1-theta.hat)^(m-2)
g_theta.hat

# Rao-Blackwell estimator of P(X<=2)

theta.raoblackwell = ( choose((n-1)*m,suff.stat) + m*choose((n-1)*m,(suff.stat-1)) + choose(m,2)*choose((n-1)*m,(suff.stat-2)) ) / choose(n*m,suff.stat)
theta.raoblackwell

####################################################################
# parametric bootstrap of the MLE and of the Rao-Blackwell estimator
# of the probability P(X<=2)

B = 1000

g_theta.hat.star = numeric(B)
theta.raoblackwell.star = numeric(B)

for(i in 1:B){
	x = rbinom(n, m, theta.hat) 	# plug in the UMVUE of theta
	theta.hat.star = sum(x)/(n*m)
	suff.stat = sum(x)
      g_theta.hat.star[i] = (1-theta.hat.star)^m + m*theta.hat.star*(1-theta.hat.star)^(m-1) + choose(m,2)*theta.hat.star^2*(1-theta.hat.star)^(m-2)
	# g_theta.hat.star[i] = pbinom(2, m, theta.hat.star)	# note this alternative calculation
      theta.raoblackwell.star[i] = ( choose((n-1)*m,suff.stat) + m*choose((n-1)*m,(suff.stat-1)) + choose(m,2)*choose((n-1)*m,(suff.stat-2)) ) / choose(n*m,suff.stat)
}


X11()
plot(density(g_theta.hat.star, n=50, window="g"),type="l",xlab="theta",ylab="density")

g_theta.hat.star.boot.m = mean(g_theta.hat.star)
g_theta.hat.star.boot.s = sd(g_theta.hat.star)
g_theta.hat.star.boot.s
g_theta.hat.star.boot.ci = quantile(g_theta.hat.star,c(0.025,0.975))
g_theta.hat.star.boot.ci
 
X11()
plot(density(theta.raoblackwell.star, n=50, window="g"),type="l",xlab="theta",ylab="density")

theta.raoblackwell.star.boot.m = mean(theta.raoblackwell.star)
theta.raoblackwell.star.boot.s = sd(theta.raoblackwell.star)
theta.raoblackwell.star.boot.s
theta.raoblackwell.star.boot.ci = quantile(theta.raoblackwell.star,c(0.025,0.975))
theta.raoblackwell.star.boot.ci

# print results

prob.true
c(g_theta.hat, theta.raoblackwell)
c(g_theta.hat.star.boot.m, theta.raoblackwell.star.boot.m) 
c(g_theta.hat.star.boot.s, theta.raoblackwell.star.boot.s) 
c(g_theta.hat.star.boot.ci, theta.raoblackwell.star.boot.ci) 



