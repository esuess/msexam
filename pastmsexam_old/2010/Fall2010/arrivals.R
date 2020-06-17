### gamma-arrivals 

arrivals = scan("E:\\univ\\MastersExam\\2010-2011\\MSExamF2010\\gamma-arrivals.txt")

n = length(arrivals); n

hist(arrivals)	

x.max = max(hist(arrivals)$breaks)
y.max = max(hist(arrivals)$density)

########################################################################################
# matrices for output

out.mme = matrix(0,2,5)  # [ MMEs, bootstrap ests., bootstrap se, LCL, UCL ]

########################################################################################
# MMEs

x.bar = mean(arrivals); x.bar

sigmasq.hat = ((n-1)/n)*var(arrivals); sigmasq.hat

lambda.hat.mme = x.bar/sigmasq.hat
lambda.hat.mme

alpha.hat.mme = x.bar^2/sigmasq.hat
alpha.hat.mme

out.mme[1,1] = alpha.hat.mme
out.mme[2,1] = lambda.hat.mme

# plot the fitted model to the simulated data

xx = seq(0,x.max,.1)

yy = dgamma(xx, shape = alpha.hat.mme, rate = lambda.hat.mme)

X11()
plot(xx,yy, main="MME fitted gamma",type="l")

# make plot of histogram with the fitted model

X11()
hist(arrivals,probability=T,xlim=c(0,x.max),ylim=c(0,y.max),main="MME fitted gamma",
  xlab="arrivals",ylab="density")
par(new=T)
plot(xx,yy, type="l",xlim=c(0,x.max),ylim=c(0,y.max),xlab="", ylab="")

# Parametric Bootstrap estimate of the s.e. and sampling distribution of the mme's

B = 1000

y = matrix(0,B,n)

lambda.star.mme = numeric(B)
alpha.star.mme = numeric(B)

for(i in 1:B){
	y[i,] = rgamma(n, alpha.hat.mme,lambda.hat.mme)
	x.bar = mean(y[i,])
	sigmasq.hat = ((n-1)/n)*var(y[i,]) 
	lambda.star.mme[i] = x.bar/sigmasq.hat
	alpha.star.mme[i] = x.bar^2/sigmasq.hat
}

br = seq(0,5,0.10) 	# create breaks for the histogram

X11()
hist(alpha.star.mme,probability = T,plot=T) 

alpha.star.mean = mean(alpha.star.mme)
alpha.star.mean
alpha.star.sd = sqrt(var(alpha.star.mme))
alpha.star.sd

out.mme[1,2] = alpha.star.mean
out.mme[1,3] = alpha.star.sd

alpha.LCL = quantile(alpha.star.mme, 0.025)
alpha.LCL
alpha.UCL = quantile(alpha.star.mme, 0.975)
alpha.UCL

out.mme[1,4] = alpha.LCL
out.mme[1,5] = alpha.UCL

X11()
hist(lambda.star.mme,probability = T,plot=T)

lambda.star.mean = mean(lambda.star.mme)
lambda.star.mean
lambda.star.sd = sqrt(var(lambda.star.mme))
lambda.star.sd

out.mme[2,2] = lambda.star.mean
out.mme[2,3] = lambda.star.sd

lambda.LCL = quantile(lambda.star.mme, 0.025)
lambda.LCL
lambda.UCL = quantile(lambda.star.mme, 0.975)
lambda.UCL

out.mme[2,4] = lambda.LCL

out.mme[2,5] = lambda.UCL

plot(lambda.star.mme, alpha.star.mme)
cor(lambda.star.mme, alpha.star.mme)

########################################################################################
# upper quartile

gamma.q75 = qgamma(.75, alpha.hat.mme, lambda.hat.mme)
gamma.q75


alpha.hat.mme = x.bar^2/sigmasq.hat
alpha.hat.mme

# Parametric Bootstrap estimate of the s.e. and sampling distribution of the mme's

gamma.q75.star.mme = qgamma(.75, alpha.star.mme, lambda.star.mme)

hist(gamma.q75.star.mme)

mean(gamma.q75.star.mme)
sd(gamma.q75.star.mme)

gamma.q75.LCL = quantile(gamma.q75.star.mme, 0.025)
gamma.q75.LCL
gamma.q75.UCL = quantile(gamma.q75.star.mme, 0.975)
gamma.q75.UCL