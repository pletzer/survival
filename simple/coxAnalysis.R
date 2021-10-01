library(survival)
library(tidyverse)
library(ggfortify)

df1 <- read.csv('simple1.csv') %>% mutate(group = rep(1))
df2 <- read.csv('simple2.csv') %>% mutate(group = rep(2))


df <- rbind(df1, df2)

cox <- coxph(Surv(time, status) ~ group, data = df)
summary(cox)
autoplot(survfit(Surv(time, status) ~ group, data = df))
