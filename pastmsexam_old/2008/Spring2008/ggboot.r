# Suppose 

n = 100 
delta = 10 
beta = 3     # scale

# simulate a sample of size n 

x = delta + rgamma(n,shape=1,scale=beta)

hist(x,xlim=c(0,40))

# MLEs delta.hat = <<< ENTER the MLE of delta here, a function of the vector x >>> 
delta.hat

beta.hat = <<< ENTER the MLE of beta here, depends on delta.hat >>> 
beta.hat

# parametric bootstrap the MLE B = 1000

delta.hat.star = numeric(B) 
beta.hat.star = numeric(B)

for(i in 1:B){
        x = delta.hat + rgamma(n,shape=1,scale=beta.hat)
        delta.hat.star[i] =  <<< ENTER the MLE of delta here >>>
        beta.hat.star[i] = <<< ENTER the MLE of beta here >>>
}

X11() 
plot(density(delta.hat.star, n=50, window="g"),type="l",xlab="x",ylab="density")
X11() 
plot(density(beta.hat.star, n=50, window="g"),type="l",xlab="x",ylab="density")

delta.hat.star.m = mean(delta.hat.star) 
delta.hat.star.m
delta.hat.star.s = sd(delta.hat.star) 
delta.hat.star.s
delta.hat.star.ci = quantile(delta.hat.star,c(0.025,0.975))
delta.hat.star.ci

beta.hat.star.m = mean(beta.hat.star)
beta.hat.star.m
beta.hat.star.s = sd(beta.hat.star) 
beta.hat.star.s
beta.hat.star.ci = quantile(beta.hat.star,c(0.025,0.975))
beta.hat.star.ci
