library(numDeriv)

df <- read.csv('data.csv')

lambda_func <- function(t, x, params) {
	lam0 <- params[[1]]
	a <- params[[2]]
	tau <- 30 # params[[3]]
	return(  lam0*( 1. + a*x*exp(-t/tau) )  )
}

Lambda_func <- function(t, x, params) {
	lam0 <- params[[1]]
	a <- params[[2]]
	tau <- 30 # params[[3]]
	return(   lam0*(  t + a*x*tau*( 1. - exp(-t/tau) )  )   )
}

likelihood <- function(params) {

	return(    sum(   df$death * log(lambda_func(df$time, df$x, params)) - Lambda_func(df$time, df$x, params))    )
}


params <- c(1.e-3, 0.5) # , 10.)
res <- optim(params, likelihood,
             control = list(fnscale = -1., maxit = 1000), hessian = F) # maximisation

lam0 <- res$par[[1]]
a <- res$par[[2]]
hess <- hessian(likelihood, res$par, method.args = list(d=c(0.01*lam0, 0.01)))

print(hess)

fisher_info <- solve(-hess)
param_sigma <- sqrt(diag(fisher_info))
print(sprintf("best estimates lam0 = %g +/- %g  a = %f +/- %f", lam0, 1.96*param_sigma[[1]], a, 1.96*param_sigma[[2]]))

# res <- optim(params, likelihood, method = "L-BFGS-B", lower = c(0., -1.), 
#              control = list(fnscale = -1., maxit = 1000)) # maximisation
print(res)
