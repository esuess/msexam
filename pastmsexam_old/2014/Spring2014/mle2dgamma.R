# joint MLE

lik <- function(al){
	alpha=al[1]
	lambda=al[2]
	prod(dgamma(x,alpha,lambda))
}

optim(c(1,2), fn = lik, control = list(fnscale = -1))