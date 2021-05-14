library("tidyverse")
heights <- rnorm(n = 100000, mean = 176, sd = 10)
hist(heights, breaks = 40)

rnorm(3)
#the above is the same as:
rnorm(1)
rnorm(1)
rnorm(1)

x <- rnorm(100, mean = 40, sd = 20)
y <- rnorm(100, mean = 120, sd = 40)

#the mean of x + y is the sum of the means 40 and 120
#and the sd is the sum of the sds 20 and 40
#the sum of normal distributions is a normal distribution
hist(x + y)

#the same as hist(x) just offset to be around the zero value
#normal distribution with mean = 0 and sd = 1
hist((x - 40) / 20)

#the value of x that the values are lower than it with the given probability
qnorm(p = 0.975, mean = 176, sd = 10)

n <- 20
x <- rnorm(n = 20, mean = 3, sd = 2)
m <- mean(x)
sigm <- 2
s <- sd(x)

#95% доверителен интервал е когато остава по средата на нормалното
#разпределение 95% лице
#интервал на достоверност е percent confidence interval

qs <- qnorm(p = 0.975)

#manual calculation of percent confidence interval
c(m - (qs * sigm) / sqrt(n), m + (qs * sigm) / sqrt(n))
c(m - (qs * s) / sqrt(n), m + (qs * s) / sqrt(n))
hist(rt(1000, df = 19), breaks = 40)

#rt and qt are t distributions
#Извадка от стандартно отклонение sqrt(1/(n-1) * sum((x - avg(x))^2) = s
q <- qt(p = 0.975, df = n - 1)


#generates percent confidence inteval
d <- t.test(x, conf.level = 0.99)

str(d)


sim <- function () {
    n <- 20
    x <- rnorm(n, mean = 3, sd = 2)
    m <- mean(x)
    s <- sd(x)

    #df is degrees of freedom
    q <- qt(p = 0.975, df = n - 1)
    between(3, m - (q * s) / sqrt(n), m + (q * s) / sqrt(n))
    #we can use t.test
}

vec <- map(1:1000, ~sim()) %>% unlist()
sum(vec) / 1000

#Task 2
#по х теоритичните, а по у теоритичните квантили се дава от qqnorm
levkimiq <- c(10.0, 13.6, 13.2, 11.6, 12.5, 14.2, 14.9,
              14.5, 13.4, 8.6, 11.5, 16.0, 14.2, 16.8, 17.9, 17.0)

mean(levkimiq)
t.test(levkimiq, conf.level = 0.95)
t.test(levkimiq, conf.level = 0.90)


#shahpazow
hist(levkimiq, breaks = 10)

#ако боксплота е симетричен и няма много отклонения е нормално вероятно
boxplot(levkimiq)

#Понеже данните са в рпава линия вероятно има линейна връзка
qqnorm(levkimiq)
qqline(levkimiq)

#Понеже е нормално това, което правим е:
t.test(levkimiq, conf.level = 0.95)
t.test(levkimiq, conf.level = 0.90)

#Тази функция се използва, ако връзката не е линейна при qqnorm
wilcox.test(levkimiq, conf.int = T, conf.level = 0.96)

#Пропорции на случване на нещо. Стрелец стреля по мишена и има шанс да оцели.
#Интервал на достоверност е с prop.test

#Task 3
library("UsingR")

#Тези според графиките са нормално разпределени
qqnorm(rat)
qqline(rat)
hist(rat)
boxplot(rat)
t.test(rat, conf.level = 0.96)

#Тези според графиките не са нормално разпределени
qqnorm(exec.pay)
qqline(exec.pay)
hist(exec.pay)
boxplot(exec.pay)

wilcox.test(exec.pay, conf.int = T, conf.level = 0.96)

#t.test(exec.pay, conf.level = 0.96)
#За задача 4 prop.test
