set.seed(1234)

# input parameters
lam0 <- 1.e-2 # baseline risk for unvaccinated people
a <- 1.0 # increased risk due to vaccinaton (if > 0)
tau <- 30 # number of days for the vaccination risk to taper off
max_time <- 100 # follow up period

n <- 1000 # number of patients

cum_hazard_func <- function(t, x) {
	return(  lam0*( t + a*x*tau*(1. - exp(-t/tau)) )  )
}

get_time_death <- function(r, x, chfunc) {

	f <- function(t) (chfunc(t, x) + log(r))

	interval <- c(0., 10000)
	res <- uniroot(f, interval, extendInt = "yes", tol = 1.e-5, maxiter = 1000, trace = TRUE)

	return(res$root) # should we make it an integer?
}

# create the data
id <- c() # patient id
status <- c() # 0=censored 1=dead
time <- c() # time to death or censorship
x <- c() # vaccination status, 0=unvaccinated 1=vaccinated

for(i in 1:n) {

	# choose a random number between 0 and 1
	sval <- runif(1)

	# random vaccination status
	r2 <- runif(1)
	xval <- 0
	if (r2 > 0.5) {
		xval <- 1
	}

	# find the time of death for this r
	time_death <- get_time_death(sval, xval, cum_hazard_func)

	id <- c(id, i)
	x <- c(x, xval)
	if (time_death > max_time) {
		# patient survived, censored
		status <- c(status, 0)
		time <- c(time, max_time)
	} else {
		# patient died
		status <- c(status, 1)
		time <- c(time, time_death)
	}

}

df <- data.frame(id = id, status = status, time = time, x = x)
write.csv(df, 'data.csv')
