dat <- da36151.0007
library("tidyverse")

table(dat$SURVEY)
table(dat$NR12)
prop.table(table(dat$NR12))
table(dat$NR13A)
table(dat)

#Rs_received
Rs_received <-dat$NR13A
Rs_received_in_ranges <- cut(Rs_received, breaks =20, include.lowest = TRUE)
table(Rs_received_in_ranges)
summary(Rs_received)

#Rs_sent
Rs_sent <- dat$NR13B
summary(Rs_sent)

#Rs_sent filtered by gender
dat %>% group_by(NR5) %>% summarize(mean = mean(NR13B,na.rm=TRUE), median = median(NR13B,na.rm=TRUE))

#Rs_received filtered by gender
dat %>% group_by(NR5) %>% summarize(mean = mean(NR13A,na.rm=TRUE), median = median(NR13A,na.rm=TRUE))


table(dat$NR8)

dat %>% filter(NR8==(1))

datHH <- da36151.0002

#Summary statistics: INCOME
datHH %>% summarize(mean = mean(INCOME,na.rm=TRUE),median = median(INCOME, na.rm = TRUE))

#Summary statistics: INCREMIT/INCOME
datHH %>% summarize(mean = mean(INCREMIT/INCOME,na.rm = TRUE),median = median(INCREMIT/INCOME, na.rm = TRUE))

#Summary Statistics: INCREMIT

datHH %>% summarize(mean = mean(INCREMIT,na.rm=TRUE),median = median(INCREMIT, na.rm = TRUE), min = min(INCREMIT, na.rm = TRUE),max=max(INCREMIT, na.rm = TRUE))
