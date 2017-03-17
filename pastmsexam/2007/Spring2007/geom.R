### R program: geom.R

n = 35; theta = 0.1;

#help(rgeom)

rgeom.new = function(n,theta){
	# need to add 1 to equal the pdf in the problem
	# f(x|theta) = (1-theta)^(x-1) theta
	rgeom(n,theta) + 1
}

x = rgeom.new(n,theta)
x.max = max(x)
brakes = seq(1,(x.max+1)) - 0.5
X11()
hist(x, br = brakes,freq=FALSE)

# MLE estimate of theta

theta.hat = 1/mean(x)
theta.hat

# plot the fitted model

x.fit = c(0:x.max)
y.fit = dgeom(x.fit,theta.hat)
X11()
plot((x.fit+1), y.fit)

# 95% asymptotic confidence interval for theta

C = 0.95; alpha = (1-C); alpha.2 = alpha/2;

AV.hat = theta.hat^2*(1-theta.hat)/n
ASD.hat = sqrt(AV.hat)
z.alpha.2 = qnorm(1-alpha.2)

theta.aci = c(theta.hat - z.alpha.2*ASD.hat, 
		theta.hat + z.alpha.2*ASD.hat)
theta.aci

# parametric Bootstap 

B = 1000 # number of boostrap samples

theta.hat.star.p = numeric(B)
for(j in 1:B){
	x.boot = rgeom(n,theta.hat)	# bootstrap samples
	theta.hat.star.p[j] = 1/mean(x.boot)
}

X11()
hist(theta.hat.star.p)
mean(theta.hat.star.p)
sd(theta.hat.star.p)
theta.pci = quantile(theta.hat.star.p,c(0.025,0.975))
theta.pci

# nonparametric Bootstrap

B = 1000 # number of boostrap samples

theta.hat.star.np = numeric(B)
for(j in 1:B){
	x.boot = sample(x,n,replace=TRUE)	# bootstrap samples
	theta.hat.star.np[j] = 1/mean(x.boot)
}

X11()
hist(theta.hat.star.np)
mean(theta.hat.star.np)
sd(theta.hat.star.np)
theta.npci = quantile(theta.hat.star.np,c(0.025,0.975))
theta.npci

# Compare

theta.aci
theta.pci
theta.npci





