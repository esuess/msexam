# Check to see if these libraries are installed in R.  
# If not, run the next two lines of code to install them or use the 
# pull-down menu Packages > Install package(s)... .

library(ismev)
library(stats4)

# install.packages("ismev", repos = "http://cran.cnr.Berkeley.edu")
# install.packages("stats4", repos = "http://cran.cnr.Berkeley.edu")

x = c(4, 3, 5, 8, 4, 3, 3, 4, 3, 5, 7, 3, 8)
Y = c(27, 54, 86, 136, 65, 109, 28, 75, 53, 33, 168, 47, 52)

n = length(Y);  n

X11(); plot(x, Y)

Y.log = log(Y)

X11(); plot(x,Y.log)

# minus the log likelihood

ll = function(a,b){
  -sum(Y*(a + b*x)) + sum(exp(a + b*x)) + sum(log(factorial(Y)))
}

model.mle = mle(minuslog=ll,start=list(a=1,b=1));  model.mle

# plot fitted model

a.mle = coef(model.mle)[1];  a.mle
b.mle = coef(model.mle)[2];  b.mle

x.index = seq(min(x), max(x),0.01)
Y.fit = exp(a.mle + b.mle*x.index)

plot(x,Y,xlab="speed",ylab="damaged cans",main="Fitted Model")
lines(x.index,Y.fit,type="l",col=3)

a.0 = log(mean(Y));  a.0

LR.stat = 2*n*(a.mle - a.0)*mean(Y) + 2*b.mle*sum(x*Y)
LR.stat

qchisq(.95,1)