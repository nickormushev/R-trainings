x <- c(4, 1, 7, 9)
y <- c(10, 3, 2, 11)

mx <- mean(x)
my <- mean(y)

sx <- var(x)
sy <- var(y)

t  <- 2 * (mx - my) / sqrt(sx + sy)

2 * pt(t, df = 3, lower.tail = T)

#t.test само, ако знаем, че данните са нормално разпределени
t.test(x, y, alternative = "two.sided")

#това се ползва, ако не знаем, че са нормално разпределени
wilcox.test(x, y, alternative = "two.sided")
#paired test. Данните идват от един и същи индивид след
#време. Примерно приел е лекарство и гледаме дали има разлика
#в нивото на холестерола и виждаме, че има промяна
#Разликите са нормално разпределени.


#Task2
mat <- matrix(c(351, 254, 71, 195), 2, 2)
prop.test(mat, alternative = "two.sided")

#alternative
#не ни обясни как точно става
#това е вярно с alternative greater
prop.test(c(351, 71), c(605, 195), alternative = "greater")

#Task3
non_placebo <- c(15, 10, 13, 7, 9, 8, 21, 9, 14, 8)
placebo <- c(15, 14, 12, 8, 14, 10, 7, 16, 10, 15, 12)

qqnorm(placebo)
qqline(placebo)
qqnorm(non_placebo)
qqline(placebo)

boxplot(placebo)
boxplot(non_placebo)

#Test for normal distribution
shapiro.test(placebo)
shapiro.test(non_placebo)

#Ако са нормално разпределени t.test(non_placebo, placebo, alternative = "less")
#Трудно е да преценя дали данните са нормално разпределени затова wilcox
wilcox.test(non_placebo, placebo, alternative = "less")

#Task4
t.test(radar1, radar2, alternative = "two.sided", paired = T)


#Task 5
library("UsingR")
colnames(ewr)

ewr_out <- ewr[ewr$inorout == "out", ]
ewr_out

shapiro.test(ewr_out$AA)
shapiro.test(ewr_out$NW)

qqnorm(ewr_out$NW)
qqline(ewr_out$NW)
boxplot(ewr_out$NW)
hist(ewr_out$NW)

qqnorm(ewr_out$AA)
qqline(ewr_out$AA)
boxplot(ewr_out$AA)
hist(ewr_out$AA)

wilcox.test(ewr_out$AA, ewr_out$NW, alternative = "two.sided")
#t.test(ewr_out$AA, ewr_out$NW, alternative = "two.sided")


#Task 6
#Too lazy to copy the data
#Probably the same as above


#Task 7
diodes1 <- c(39, 50, 61, 67, 40, 40, 54)
diodes2 <- c(60, 53, 42, 41, 40, 54, 63, 69)

shapiro.test(diodes1)
shapiro.test(diodes2)

qqnorm(diodes1)
qqline(diodes1)
boxplot(diodes1)
hist(diodes1)

qqnorm(diodes2)
qqline(diodes2)
boxplot(diodes2)
hist(diodes2)

t.test(diodes1, diodes2, alternative = "two.sided")

#Task 8
genTtest <- function (n) {
    s1 <- sample(n)
    s2 <- sample(n)
}
#Ask Shahpazov
