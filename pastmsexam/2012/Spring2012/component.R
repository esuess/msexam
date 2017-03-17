### relative efficiency
### lifetime of component is exponential with mean beta

t.prime = seq(0.1,5,0.1)  # trial time

beta = 1   # mean time to failure

v.beta1.hat = beta^2/n   # var of MLE beta1.hat
v.beta1.hat

av.beta.hat = (beta^4/(n*t.prime^2))*(exp(t.prime/beta)-1)  # AV of beta.hat
av.beta.hat

re = v.beta1.hat/av.beta.hat
re

plot(t.prime, re, ylim=c(0,1))