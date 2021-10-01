# input parameters
pend <- 1000         # number of days
pdie <- 0.12/365.25 # prob to die every day (constant)
c <- 2  # increased chance dying due to some treatment
n <- 100 # number of patients

time <- c()   # number of days until event
status <- c() # 1=censored, 2=dead
for (i in 1:n) {
    tm <- 0
    st <- 1
    # patients will drop out (censored) after tfinal days
    tfinal <- runif(1, 1., pend)
    while (st == 1 & tm < tfinal) {
        # compute the probability of dying today
        p <- runif(1, 0., 1.)
        if (p < c*pdie) {
            # no luck, you're dead
            st <- 2
        }
        tm <- tm + 1
    }
    time <- append(time, tm)
    status <- append(status, st)
}

df <- data.frame(time = time, status = status)
write.csv(df, 'dropout2.csv')
