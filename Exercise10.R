#I missed it :(
library("UsingR")
library("tidyverse")

pi2000
t <- table(pi2000[1:200])
chisq.test(t)

prob <- c(0.1270, 0.0956, 0.0817, 0.0751, 0.0697, 0.0675, 0.4834)
x <- c(102, 108, 90, 95, 82, 40, 519)

chisq.test(x, p = prob)

#read from terminal
#Task 4
x <- scan()

#Look at the other tasks. They are interesting
