set.seed(1234)

# input parameters
lam0 <- 2.e-5 # baseline daily risk for unvaccinated people
a <- 1.0 # increased risk due to vaccinaton (if > 0)
tau <- 30 # number of days for the vaccination risk to taper off
max_time <- 100 # follow up period
rvacc = 0.5

n <- 100000 # number of patients

cum_hazard_func <- function(t, x) {
	return(  lam0*( t + a*x*tau*(1. - exp(-t/tau)) )  )
}

get_time_death <- function(r, x, chfunc) {

	f <- function(t) (chfunc(t, x) + log(r))

	interval <- c(0., 10000000)
	res <- uniroot(f, interval, extendInt = "yes", tol = 1.e-10, maxiter = 1000, trace = TRUE)

	return(res$root) # should we make it an integer?
}

# create the data
id <- rep(NA, n) # patient id
death <- rep(NA, n) # 0=censored 1=dead
time <- rep(NA, n) # time to death or censorship
x <- rep(NA, n) # vaccination status, 0=unvaccinated 1=vaccinated

for(i in 1:n) {

	# choose a random number between 0 and 1
	sval <- runif(1)

	# random vaccination status
	r2 <- runif(1)
	xval <- 0
	if (r2 > rvacc) {
		xval <- 1
	}

	# find the time of death for this r
	time_death <- get_time_death(sval, xval, cum_hazard_func)

	id[i] <- i
	x[i] <- xval
	if (time_death > max_time) {
		# patient survived, censored
		death[i] <- 0
		time[i] <- max_time
	} else {
		# patient died
		death[i] <- 1
		time[i] <- time_death
	}

}

df <- data.frame(id = id, time = time, x = x, death = death)
write.csv(df, 'data.csv')
