#These are solutions to most of the tasks during the semester solved for
#exam preparation

#Ex1 
x <- c(8, 3, 8, 7, 15, 9, 12, 4, 9, 10, 5, 1)

m <- matrix(x, nrow = 4, ncol = 3)

colnames(m) <- c('a', 'b', 'c')

col <- seq(1, 7, by = 2)

cbind(m, col)

m[order(m[,1]),]

#Task 2
library("UsingR")

colnames(homedata)
head(homedata[order(homedata$y2000),], 1)

head(homedata[order(homedata$y2000),], 5)

sum(homedata$y2000 > 750000)

mean(homedata[homedata$y2000 > 750000, ]$y1970)

homedata[homedata$y2000 < homedata$y1970, ]$y2000

#Ex2
library("MASS")

colnames(survey)

prop.table(table(survey$Smoke))

prop.table(table(survey$Smoke, survey$Sex))

prop.table(table(survey$Smoke, survey$Sex), 2)

prop.table(table(survey$Smoke, survey$Sex), 1)

#Ex3
barplot(prop.table(table(survey$Smoke, survey$Sex), 2), beside = T, legend = T)
pie(prop.table(table(survey$Smoke, survey$Sex), 2))

pie(prop.table(table(survey$Smoke)))

#Ex4
a <- survey[survey$Age < 20, ]
b <- survey[survey$Age >= 20 & survey$Age < 25, ]
c <- survey[survey$Age >= 25, ]

survey$AgeCat <- ifelse(survey$Age < 20, "A",
                        ifelse(survey$Age < 25, "B", "C"))

pie(table(survey$AgeCat))
barplot(prop.table(table(survey$Smoke, survey$AgeCat), 2), beside = T, legend = T)

m <- mean(survey$Height[!is.na(survey$Height)])
s <- sd(survey$Height[!is.na(survey$Height)])
quantile(survey$Height, na.rm = T)

ct <- cut(survey$Height[!is.na(survey$Height)], breaks = c(0, m - s, m + s, 300))

table(ct)


##3
menH <- survey[survey$Sex == "Male",]$Height
womenH <- survey[survey$Sex == "Female",]$Height
plot(density(menH, na.rm = T), col = 'red', ylim = c(0, 0.07))
lines(density(womenH, na.rm = T), col = 'blue')
legend('topright', legend = c('Male', 'Female'), fill = c('red', 'blue'))


h <- hist(survey$Height)
#Нещо наречено полигона
#Чертае линия по графиката
lines(h$mids, h$counts)

hist(survey$Height, probability = T)
lines(density(survey$Height, na.rm = T))

boxplot(survey$Height)

#Task 4
split.screen(c(1, 2))
screen(1)
hist(homedata$y2000)
screen(2)
hist(homedata$y1970)

cor(homedata$y1970, homedata$y2000)
plot(homedata)
l = lm(y2000 ~ y1970, homedata)
abline(l)

identify(homedata$y1970, homedata$y2000)

library("UsingR")
plot(density(homedata$y2000), col = "red", ylim = c(0, 4e-05))
lines(density(homedata$y1970), col = "blue")

anscombe
m <- cbind(anscombe$x1, anscombe$y1)
quantile(m)

#Ex 4
p <- function() {
  s <- sample(1:6, size = 100, replace = T)
  sum(s == 6)
}

library(tidyverse)
f <- function(n) {
  t <- rep(p(), n)
  mean(t)
}


f(100)

t <- seq(1, 1000) %>%
  map(~f(.x)) %>%
  cummean() %>%
  plot(type = "l")


#Task 2 Ex 4
daysInYear <- 365
birthday <- function(p) {
  different <- 1
  for (i in seq(daysInYear - 1, 1)) {
       if(1 - different > p) {
         return(daysInYear - i)
       } 
       different <- different * i/daysInYear
  }
}

birthday(1/2)

#Task 3 Ex 4

p1 <- 0.3
p2 <- 0.4
#Play dad first
(1 - p1) * p2 * p1 + p1 * p2
#Play mom first
(1 - p2) * p1 * p2 + p1 * p2

p <- function(p1, p2) {
  s <- sample(c(0,1), size = 2, prob = c(1 - p1, p1), replace = T)
  s[3] <- sample(c(0,1), size = 1, prob = c(1 - p2, p2), replace = T)
  s
}

try100 <- 1:100000 %>%
  map(function(x){
    r <- p(0.3, 0.4)
    (r[1] == r[3] & r[1]) | (r[2] == r[3] & r[2])
  }) %>%
  unlist()

sum(try100)/100000

#Ex 4 Task 4
n <- 20

presentOrder <- function(variables) {
  sample(1:20)
}

tries <- 10000
r <- 1:tries %>%
  map(function(x) {
    t <- presentOrder()
    all(t == seq(1,20))
  }) %>%
  unlist()

mean(r)

1/factorial(20)

x <- c(1, 2, 3)
t <- c(1, 1, 3)
  all(x == t)
  
#Ex 4 Task 5
#Грешно е. Напиши си функция. Опитите не са независими до достигане на успех.
mean(rgeom(1000, prob = 1/32))

#Ex 5 Task 1
pbinom(4, 30, 1/6)

sum(rbinom(1000, 30, 1/6) < 5)/1000

qbinom(0.75, 30, prob = 1/6, lower.tail = F)
pbinom(3, 30, prob = 1/6, lower.tail = F)


#Ex 5 Task 2
dnbinom(5, 3, prob = 0.2)
pnbinom(3, 3, prob = 0.2, lower.tail = F)
pnbinom(5, 3, prob = 0.2) - pnbinom(1, 3, prob = 0.2)

#Ex 5 Task 3

#a)
test_count <- 1000
sims <- rhyper(test_count, 7, 6, 8)

min(sims)
max(sims)

sum(sims == 3)/test_count
var(sims)
mean(sims)

#b)
probs <- dhyper(2:7, 7, 6, 8)
sum(0:8 * probs)
sum((0:8)^2 * probs) - sum(0:8 * probs)^2

emp <- prop.table(table(sims))
emp
probs

g <- rbind(emp, probs)

barplot(g, beside = T)

#Ex 5 Task 4
t <- c()
for (n in c(10, 100, 1000, 10000)) {
  t[length(t) + 1] <- dbinom(2, n, prob = 5/(2*n))
}
t

#Ex 5 Task 5
pgeom(10, prob = 4/52) - pgeom(3, prob = 4/52)

x <- rgeom(10000, prob = 4/52)

sum(x > 3 & x <= 10)/10000

quantile(x)

qgeom(c(0, 0.25, 0.50, 0.75, 1), prob = 4/52)

c <- table(x)


#Exercise 6 Task 1
#a)
d <- rnorm(100, mean = 5, sd = sqrt(2))
boxplot(d)
hist(d, probability = T)
lines(density(d), col = 'red')

x <- seq(1,8, by = 0.2)
y <- dnorm(x, mean = 5, sd = sqrt(2))
lines(x, y, col = 'blue')

#b)
d <- runif(100, 1, 5)
boxplot(d)
hist(d, probability = T)
lines(density(d), col = 'red')

x <- seq(1,8, by = 0.2)
y <- dunif(x, 1, 5)
lines(x, y, col = 'blue')

#c)
d <- rexp(100, 3)
boxplot(d)
hist(d, probability = T)
lines(density(d), col = 'red')

x <- seq(0,2, by = 0.02)
y <- dexp(x, 3)
lines(x, y, col = 'blue')

#d)
d <- rgamma(100, 5, 1)
boxplot(d)
hist(d, probability = T)
lines(density(d), col = 'red')

x <- seq(0,15, by = 0.02)
y <- dgamma(x, 5, 1)
lines(x, y, col = 'blue')


#e)
x <- rnorm(100, 1, sqrt(2))
y <- rnorm(100, 5, sqrt(2))

x <- append(x, y)

hist(x)
boxplot(x)

#Ex 6 Task 2
library(tidyverse)

v <- 1:1000 %>%
  map(~rgamma(1000, 5, 1))

hist(Reduce('+', v))

library("UsingR")
age <- babies$age[babies$age != 99]

shapiro.test(age)
boxplot(age)
hist(age)
hist(babies$ht)
shapiro.test(babies$ht)

qqnorm(babies$age)
qqline(babies$age)

#Ex6 Task 4
pnorm(20, mean = 25, sd = 6)
qnorm(0.21, mean = 25, sd = 6)

qnorm(0.61, mean = 25, sd = 6)

#Ex7 Task 1.
#Тук се построяват теоритично доверителни интервали. По-добре виж решението просто
x <- rnorm(20, 3, 2)
t.test(x, conf.level = 0.95)

#Ex7 Task 2
x <- c(10.0,13.6,13.2,11.6,12.5,14.2,14.9,14.5,13.4,8.6,11.5,16.0,14.2,16.8,17.9,17.0)

hist(x)
shapiro.test(x)
qqnorm(x)
qqline(x)
boxplot(x)

mean(x)

t.test(x, conf.level = 0.95)
t.test(x, conf.level = 0.90)

#Ex7 Task 3
hist(rat)
shapiro.test(rat)
qqnorm(rat)
qqline(rat)

t.test(rat, conf.level = 0.95)

#b)
hist(exec.pay)
shapiro.test(exec.pay)

wilcox.test(exec.pay, conf.level = 0.96, conf.int = T)

#Ex7 Task 4
prop.test(87, 150, conf.level = 0.95)
prop.test(870, 1500, conf.level = 0.95)

#Ex7 Task 5
data <- read.csv("../data.txt", sep = ";")
data$x <- as.numeric(gsub(",", ".", data$x))
data$y <- as.numeric(gsub(",", ".", data$y))
data$z <- as.numeric(gsub(",", ".", data$z))

meanData <- (data$x + data$y + data$z)/3

hist(meanData)
qqnorm(meanData)
qqline(meanData)
shapiro.test(meanData)
boxplot(meanData)


wilcox.test(meanData, conf.level = 0.95, conf.int = T)

prop.table(table(survey$Smoke, survey$Sex))[2,2]


#Ex 8 Task 1
#Test with 10, 30, 100
x <- rnorm(100, 2, 2)

t.test(x, mu = 5)
t.test(x, mu = 3)

#Ex8 Task 2
vacation
hist(vacation)
shapiro.test(vacation)

t.test(vacation, mu = 24)

#Ex8 Task 3
prop.test(42, 100, p = 0.5, alternative = "less")

#Ex8 Task 4
x <- c(12.8,3.5,2.9,9.4,8.7,0.7,0.2,2.8,1.9,2.8,3.1,15.8)
hist(x)
shapiro.test(x)
qqnorm(x)
qqline(x)

wilcox.test(x, mu = 5, alternative = "greater")

#Ex8 Task 5
shapiro.test(cancer$stomach)
wilcox.test(cancer$stomach, mu = 100, alternative = "less")

#Ex8 Task 6
men <- survey[survey$Sex == "Male", ]
men <- men[!is.na(men$Smoke), ]

sum(men$Smoke == "Never")
nrow(men)

prop.test(89, 117, p = 0.8, alternative = "less")


#Ex9 Task 1
x <- c(4, 1, 7, 9)
y <- c(10, 3, 2, 11)

t.test(x, y)
wilcox.test(x, y)

#Ex9 Task 2
prop.test(c(351, 71), c(605, 195), alternative = "greater")

#Ex9 Task 3
x <- c(15,10,13,7,9,8,21,9,14,8)
y <- c(15,14,12,8,14,10,7,16,10,15,12)

hist(x)
shapiro.test(x)
qqnorm(x)
qqline(x)

hist(y)
shapiro.test(y)

wilcox.test(x, y, alternative = "less")

#Ex9 Task 4
x <- c(70, 85, 63, 54, 65, 80, 75, 95, 52, 55)
y <- c(72, 86, 62, 55, 63, 80, 78, 90, 53, 57)

hist(x)
shapiro.test(x)
shapiro.test(y)

t.test(x, y, paired = T)

#Ex9 Task 5
colnames(ewr)
shapiro.test(ewr$AA)

wilcox.test(ewr$AA, ewr$NW)
wilcox.test(ewr$AA, ewr$NW, alternative = "less")

#Tasks 6 and 7 are boring
#Ex9 Task 8
sum <- 0
for (i in 1:1000) {
  x <- rgamma(20, rate = 1, shape = 2)
  y <- rnorm(200, mean = 4, sd = 2)
  
  sum <- sum + t.test(x, y)$p.value > 0.05
}
sum/1000


#Ex10 Task 1
deaths <- c(125,410,310,300,318,298,148)

chisq.test(deaths)

#Ex10 Task 2
pi200 <- pi2000[1:200]

chisq.test(table(pi200))

#Ex10 Task 3
text <- c(102,108,90,95,82,40,519)

chisq.test(text, p = c(0.127, 0.0956, 0.0817, 0.0751, 0.0697, 0.0675, 0.4834))


#Ex10 Task 4

x <- c(12813,647,359,42)
x <- append(x, c(65963,4000,2642,303))

m <- matrix( x, nrow = 2, byrow = T)

chisq.test(m)
#Not the same apparently
chisq.test(m[1,], m[2, ])

#Ex10 Task 5
x <- c(44,74,79,72,31,14,25,27,24,10,15,20,20,23,9,3,5,5,0,0)

m <- matrix(x, nrow = 4, ncol = 5, byrow = T)

chisq.test(m)

#Ex10 Task 6
x <- read.csv("../data10.txt")
x <- as.numeric(x[2:nrow(x),])
dist <- table(cut(x, breaks = c(0, 1/8, 1/4, 1/2, 1, 2, 5)))
dist

probs <- c()
probs[1] <- pexp(1/8, rate = 2)
probs[2] <- pexp(1/4, rate = 2) - pexp(1/8, rate = 2)
probs[3] <- pexp(1/2, rate = 2) - pexp(1/4, rate = 2)
probs[4] <- pexp(1, rate = 2) - pexp(1/2, rate = 2)
probs[5] <- pexp(2, rate = 2) - pexp(1, rate = 2)
probs[6] <- 1 - pexp(2, rate = 2)

chisq.test(dist, p = probs)

#Ex11 Task 1
library("tidyverse")
x <- data.frame(
  age = c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37),
  pulse = c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)
)

lm(x$pulse ~ x$age) %>%
  summary()

m <- lm(pulse ~ age, x)

predict.lm(
  m,
  newdata = data.frame(age = c(30, 40, 50)),
  interval = "confidence",
  level = 0.9
)

#Ex 12 Task 3

df <- data.frame(
  height = c(100,200,300,450,600,800,1000),
  distance = c(253,337,395,451,495,534,574)
)

lm(distance ~ height, df) %>%
  summary()

plot(df$distance, df$height)
abline(lm(distance ~ height, df))


plot(df)
m <- lm(distance ~ height + I(height^2), df) %>% summary()
lines(df, fitted(m), type = 'b', col = 'blue')

#Playing around
plot(density(rnorm(x, 50, 20)))
x <- seq(1,100, by = 0.2)
y <- dnorm(x, 50, 20)

lines(x, y, col = 'blue')

#Ex12 Task 4
library('UsingR')

colnames(homeprice)
lm(sale ~ ., homeprice %>% dplyr::select(-list)) %>% summary()
lm(sale ~ 0 +., homeprice %>% dplyr::select(-list)) %>% summary()