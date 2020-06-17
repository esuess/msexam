# observed data

n1 = 100
n2 = 102

y1 = 40
y2 = 42

p1.hat = y1/n1
p2.hat = y2/n2

theta1.hat = log( (y1*(n2-y2))/(y2*(n1-y1)) )

# parametric bootstrap

B = 1000

theta1.boot.star = numeric(B)

for (i in 1:B){
	y1.boot = rbinom(1,n1,p1.hat)
	y2.boot = rbinom(1,n2,p2.hat)
	theta1.boot.star[i] = log( (y1.boot*(n2-y2.boot))/(y2.boot*(n1-y1.boot)) )
}

# bootstrap distribution for the log-odds

plot(density(theta1.boot, n=50, window="g"), type="l", xlab="theta1.boot")

theta1.boot.m = mean(theta1.boot.star)
theta1.boot.s = sd(theta1.boot.star)
theta1.boot.ci = quantile(theta1.boot.star,c(0.025,0.975))
theta1.boot.ci

# asymptotic confidence interval for the log-odds

theta1.hat - 1.96*sqrt( 1/y1 + 1/(n1-y1) + 1/y2 + 1/(n2-y2) )
theta1.hat + 1.96*sqrt( 1/y1 + 1/(n1-y1) + 1/y2 + 1/(n2-y2) )

# Test H_0: \theta_1 = 0 versus H_1: \theta_1 != 0




