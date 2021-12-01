set.seed(1234)

# Generate a random number of days to death
# @param random_num a number between 0 and 1
# @param cum_risk_func time integral of the hazard function
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
n <- 10000

vax_date1 <- as.Date("2021-06-01")
vax_date2 <- as.Date("2021-07-15")
beg_monitoring <- as.Date("2021-01-01")
end_monitoring <- as.Date("2021-12-31")

# integral of the hazard/risk function in time
cum_risk_func_unvax <- function(t) (4.e-3*t)
cum_risk_func_partial <- function(t) (3.e-3*t)
cum_risk_func_full <- function(t) (2.e-3*t)

max_num_days_unvax <- as.integer(vax_date1 - beg_monitoring)


id <- c()
status <- c()
event_end_date <- c()
time <- c()
group <- c()
for(i in 1:n) {

	# choose a random event date betwen beg_monitoring and vax_date1
	num_days <- sample(seq(1, max_num_days_unvax, by = 1), 1)
	event_date <- vax_date1 - num_days
	event_date_str <- as.character(event_date)

	# unvaccinated period
	period <- as.integer(vax_date1 - event_date)
	r <- runif(1) # choose a number between 0 and 1
	days_to_death <- random_days_to_death(random_num = r, cum_risk_func = cum_risk_func_unvax)

	if (days_to_death > period) {

		id <- c(id, i)
		status <- c(status, 1) # censored
		event_end_date <- c(event_end_date, event_date_str)
		time <- c(time, period)
		group <- c(group, 1) # unvaccinated

		# partially vaccinated period
		r <- r/exp(-cum_risk_func_unvax(period)) # prob to die given that patient survived until vax_date1
		period <- as.integer(vax_date2 - vax_date1)
		days_to_death <- random_days_to_death(random_num = r, cum_risk_func = cum_risk_func_partial)

		if (days_to_death > period) {

			id <- c(id, i)
			status <- c(status, 1) # censored
			event_end_date <- c(event_end_date, event_date_str)
			time <- c(time, period)
			group <- c(group, 2) # partially vaccinated

			# fully vaccinated period
			r <- r/exp(-cum_risk_func_partial(period)) # prob to die given that patient survived until vax_date2
			period <- as.integer(end_monitoring - vax_date2)
			days_to_death <- random_days_to_death(random_num = r, cum_risk_func = cum_risk_func_full)

			if (days_to_death > period) {

				id <- c(id, i)
				status <- c(status, 1) # censored
				event_end_date <- c(event_end_date, event_date_str)
				time <- c(time, period)
				group <- c(group, 3) # fully vaccinated

			} else {

				# death occurred during fully vaccinated period
				id <- c(id, i)
				status <- c(status, 2) # dead
				event_end_date <- c(event_end_date, event_date_str)
				time <- c(time, days_to_death)
				group <- c(group, 3) # fully vaccinated

			}

		} else {

			# death occurred during partially vaccinated period (ie after vax_date1 but before vax_date2)
			id <- c(id, i)
			status <- c(status, 2) # dead
			event_end_date <- c(event_end_date, event_date_str)
			time <- c(time, days_to_death)
			group <- c(group, 2) # partially vaccinated

		}

	} else {

		# death occurred during the unvaccinated period
		id <- c(id, i)
		status <- c(status, 2) # dead
		event_end_date <- c(event_end_date, event_date_str)
		time <- c(time, days_to_death)
		group <- c(group, 1) # unvaccinated

	}

}
#print(event)
df <- data.frame(id = id, status = status, event_end_date = as.Date(event_end_date), time = time, group = group)
#print(df)
write.csv(df, 'data.csv')
#saveRDS(df, 'data.rds')
