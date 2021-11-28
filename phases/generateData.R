# input parameters
pend <- 1000        # number of days
pdie <- 0.12/365.25 # prob to die every day (constant)
c <- c(1, 2)  # increased chance dying due to some treatment
n <- 20 # number of patients
tswitch <- 500 # average number of days until patient switches over

time <- c()   # number of days until event
status <- c() # 1=censored, 2=dead
group <- c()

# iterate over the patients
for (i in 1:n) {

    tm <- 0
    st <- 1
    # patients will drop out (censored) after tfinal days
    tfinal <- runif(1, 1., pend)

    tsw <- rpois(1, tswitch)

    # phase 1
    while (st == 1 & tm < min(tsw, tfinal)) {
        # compute the probability of dying today
        p <- runif(1, 0., 1.)
        if (p < c[1]*pdie) {
            # no luck, you're dead
            st <- 2
        }
        tm <- tm + 1 # include the day of death, if the person dies
    }
    time <- append(time, tm)
    status <- append(status, st)
    group <- append(group, 1)

    # phase 2
    if (st == 1 & tfinal > tsw) {
        # the person is still alive

        # reset time
        tm <- 0
        while (st == 1 & tm < tfinal - tsw) {
            # compute the probability of dying today
            p <- runif(1, 0., 1.)
            if (p < c[2]*pdie) {
                # no luck, you're dead
                st <- 2
            }
            tm <- tm + 1
        }
        time <- append(time, tm)
        status <- append(status, st)
        group <- append(group, 2)
    }

}

df <- data.frame(time = time, status = status, group = group)
write.csv(df, 'phases.csv')
