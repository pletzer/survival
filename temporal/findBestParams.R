df <- read.csv('data.csv')

lambda_func <- function(t, x, params) {
	lam0 <- params[[1]]
	a <- params[[2]]
	tau <- params[[3]]
	return(  lam0*( 1. + a*x*exp(-t/tau) )  )
}

Lambda_func <- function(t, x, params) {
	lam0 <- params[[1]]
	a <- params[[2]]
	tau <- params[[3]]
	return(   lam0*(  t + a*x*tau*( 1. - exp(-t/tau) )  )   )
}

likelihood <- function(params) {

	return(    sum(   df$status * log(lambda_func(df$time, df$x, params)) - Lambda_func(df$time, df$x, params))    )
}


params <- c(1.e-3, 0.5, 10.)
res <- optim(params, likelihood, control = list(fnscale = -1., maxit = 1000, retol=1.e-12)) # maximisation

# res <- optim(params, likelihood, method = "L-BFGS-B", lower = c(0., -1.), 
#              control = list(fnscale = -1., maxit = 1000)) # maximisation
print(res)
