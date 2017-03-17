### R program: landings.R

# Note that the geometric in R is for the number of trials
# before the first success.

# The model used in this problem is for the number of trials
# until the first success.

# See help(rgeom), so we create a function called rgeom.new
# to add 1 to x to give the total number of trials to the
# first success.

rgeom.new = function(n,p){
  # need to add 1 to equal the pdf in the problem
  # f(x|p) = (1-p)^(x-1) * p
  rgeom(n,p) + 1
}

# the data

landings = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,
  48,31,20,9,6,5,4,2,1,1,2,1), ncol=2, byrow=F)

n = sum(landings[,2])
n

# MLE of p

p.hat = (sum(landings[,1]*landings[,2])/n)^-1
p.hat

# simulate a bootstrap sample and plot a histogram

x = rgeom.new(n,p.hat)
x.max = max(x)

X11()
counts = table(x)
barplot(counts, main="An example of a single bootstrap sample",
  xlab="number of landings",ylab="Frequency")
  
# plot the fitted model

x.fit = c(0:x.max)
y.fit = dgeom(x.fit,p.hat)
X11()
plot((x.fit+1),y.fit,type='h',main="Fitted model",
  xlab="x",ylab="Frequency")

# 95% asymptotic confidence interval for p
C = 0.95; alpha = (1-C); alpha.2 = alpha/2;
AV.hat = p.hat^2 * (1-p.hat)   #**** FILL IN THE AV FORMULA HERE ****
ASD.hat = sqrt(AV.hat)
z.alpha.2 = qnorm(1-alpha.2)
p.aci = c(p.hat - z.alpha.2*ASD.hat,
p.hat + z.alpha.2*ASD.hat)
p.aci

# parametric Bootstap
B = 1000 # number of boostrap samples
p.hat.star.p = numeric(B)
for(j in 1:B){
  x.boot = rgeom.new(n,p.hat) # bootstrap samples
  p.hat.star.p[j] = 1/mean(x.boot)
}

X11()
hist(p.hat.star.p)

mean(p.hat.star.p)
sd(p.hat.star.p)

p.bci = quantile(p.hat.star.p,c(0.025,0.975))
p.bci

# Compare
p.aci
p.bci