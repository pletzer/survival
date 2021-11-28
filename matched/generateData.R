set.seed(1234)

random_days_to_death <- function(random_num, cum_risk_func, tol = 1.e-4, maxiter = 1000) {
	# root of this function gives the time to death
	f <- function(t) (cum_risk_func(t) + log(random_num))
	time_obj <- uniroot(f, c(1.e-6,10000), extendInt = "yes", trace = TRUE, tol = tol, maxiter = maxiter)
	return(as.integer(time_obj$root))
}

test_random_days_to_death <- function() {
	cum_risk_func <- function(t) (0.1 * t)
	days_to_death <- random_days_to_death(0.3, cum_risk_func)
	print(days_to_death)
}


# input values
##############

# number of people
n <- 100

vax_date1 <- as.Date("2021-07-01")
vax_date2 <- as.Date("2021-07-21")
beg_monitoring <- as.Date("2021-01-01")
end_monitoring <- as.Date("2021-10-01")

# integral of the hazard/risk function in time
cum_risk_func_unvax <- function(t) (5e-2*t)
cum_risk_func_partial <- function(t) (1.e-3*t)
cum_risk_func_full <- function(t) (1.e-2*t)


id <- c()
status <- c()
event <- c()
time <- c()
group <- c()
for(i in 1:n) {

	# choose a random event date betwen beg_monitoring and vax_date1
	dates <- seq(from = beg_monitoring, to = vax_date1, by = 1)
	event_date <- sample(dates, 1)

	# throw a dice, unvaccinated life expectancy
	unvax_period <- as.integer(vax_date1 - event_date)
	r <- runif(1)
	days_to_death <- random_days_to_death(random_num = r, cum_risk_func = cum_risk_func_unvax)

	if (days_to_death > unvax_period) {

		id <- c(id, i)
		status <- c(status, 1) # censored
		event <- c(event, event_date)
		time <- c(time, unvax_period)
		group <- c(group, 1) # unvaccinated

		partial_period <- as.integer(vax_date2 - vax_date1)
		r <- r*exp(-cum_risk_func_unvax(unvax_period))
		days_to_death <- random_days_to_death(random_num = r, cum_risk_func = cum_risk_func_partial)

		if (days_to_death > partial_period) {

			id <- c(id, i)
			status <- c(status, 1) # censored
			event <- c(event, event_date)
			time <- c(time, partial_period)
			group <- c(group, 2) # partially vaccinated

			full_period <- as.integer(end_monitoring - vax_date2)
			r <- r*exp(-cum_risk_func_partial(partial_period))
			days_to_death <- random_days_to_death(random_num = r, cum_risk_func = cum_risk_func_full)

			if (days_to_death > full_period) {

				id <- c(id, i)
				status <- c(status, 1) # censored
				event <- c(event, event_date)
				time <- c(time, full_period)
				group <- c(group, 3) # fully vaccinated

			} else {

				# death occurred during fully vaccinated period
				id <- c(id, i)
				status <- c(status, 2) # dead
				event <- c(event, event_date)
				time <- c(time, days_to_death)
				group <- c(group, 3) # fully vaccinated

			}

		} else {

			# death occurred during partially vaccinated period (ie after vax_date1 but before vax_date2)
			id <- c(id, i)
			status <- c(status, 2) # dead
			event <- c(event, event_date)
			time <- c(time, days_to_death)
			group <- c(group, 2) # partially vaccinated

		}

	} else {

		# death occurred during the unvaccinated period
		id <- c(id, i)
		status <- c(status, 2) # dead
		event <- c(event, event_date)
		time <- c(time, days_to_death)
		group <- c(group, 1) # unvaccinated

	}

}
df <- data.frame(id = id, status = status, event_end_date = event, time = time, group = group)
write.csv(df, 'data.csv')