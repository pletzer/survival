library(dplyr)

set.seed(456)

df <- read.csv('data100.csv')
nmatch <- 2

# add cumulative time since event
tau_df <- df %>% group_by(id) %>% summarise(tau = cumsum(time))
print(tau_df)
df <- df %>% left_join(tau_df, by = "id")

# find all the deaths
df_deaths <- df[df[, "status"] == 2,]
for (i in 1:nrow(df_deaths)) {

	# get the group (1=unvaccinated, 2=partial vaccination, 3=full vaccination)
	g <- df_deaths[i, "group"]

	# get the Id
	id <- df_deaths[i, "id"]

	# get the entries associated with this id
	this_guy <- df[df[, "id"] == id,]

	# estimate the total time since the event for this guy
	total_time <- sum(this_guy[, "time"])

	# select nmatch random matches among the patients who are still alive
	# after total_time
	df_select <- df[df[, "tau"] < total_time,]
	matches <- df_select[sample(nrow(df_select), nmatch),]
	print(matches)

	print(sprintf("patient %d in group %g died %d days after event", id, g, total_time))
}
