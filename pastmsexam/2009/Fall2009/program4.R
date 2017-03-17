### Problem 4
### Code to plot the density to investigate the share of the pdf 
### the parameter beta.

beta = c(0.1,0.5,1,2,5,10)
k = length(beta)
x = seq(0,1,0.01)
par(mfrow=c(2,3))
for(i in 1:k){
       y = ( gamma(2*beta[i])/gamma(beta[i])^2 )*( x*(1-x) )^(beta[i]-1)
       plot(x,y,main=beta[i])
}