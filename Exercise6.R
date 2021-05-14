library(ggplot2)
library(tidyverse)

rbinom(1000, size = 10, prob = 1/2)

#Разпределене, което се определя от средното и дисперсията си
#Генерира ръстове около 170 със стандартно отклонение 4 и средно 170
rnorm(100000, mean = 170, sd = 4) %>% 
    hist(breaks = 1000)
#breaks определя  колко правоъгълиника ще има в хистограмата. По-грануларно представяме
#данните. Колкото повече breaks толкова по-плавно. Получава се почти непрекъсната крива. 
#Май кривата се нарича плътност. breaks съответства на сумата на Риман или Дарбу. 


#Така се прави приближение
#При нормално разпределение има симетрия. Определя се от средното и вариацията
x <- rnorm(100000, mean = 170, sd = 4)
hist(x, probability = TRUE)
lines(density(x))

#rexp. Време, което чакаме до някакво събитие
#експоненциално започва от нулата и намаля експоненциално
x1 <- rexp(1000, rate = 1)
hist(x1, probability = TRUE)
lines(density(x1))

#плътността показва около кои стойности е по-вероятно да се случи нещо. Тя е крива. PDF - probability density function. 
boxplot(x)

#скалиране и центраилизиране 
(x - mean(x)) / 12

boxplot(rnorm(1000))

#равномерно разпределение. Теоритичната му функция е правоъгълник.
#Всичко е еднаков често срещани. За разлика от нормалнато
#тук няма нещо около което варираме.
runif(1000, 110, 140) %>% hist()


#Задачи
#Първата задача е да използваме горните разпределения
#Гамма разпределение
rgamma(1000, shape = 12)

#Задача 1
#a)
rn <- rnorm(100, 5, 2)
hist(rn, probability = TRUE)
lines(density(rn))
boxplot(rn)

#b)
ru <- runif(100, 1, 5)
hist(ru, probability = TRUE)
lines(density(ru))
boxplot(ru)

#c)
ex <- rexp(100, rate = 3)
hist(ex, probability = TRUE)
lines(density(ex))
boxplot(ex)

#g)
rg <- rgamma(100, shape = 5, rate = 1)
hist(rg, probability = TRUE)
lines(density(rg))
boxplot(rg)

#Shahpazov
N <- 10000
rv_list <- list(
    "Normal" = rnorm(N, mean = 5, sd = 2),
    "Uniform" = runif(N, 1, 5),
    "Exp" = rexp(N),
    "Gamma" = rgamma(N, 5, 1))

hist(rv_list$Normal)
hist(rv_list$Uniform)
hist(rv_list$Exp)
hist(rv_list$Gamma)


boxplot(rv_list$Normal)
boxplot(rv_list$Uniform)
boxplot(rv_list$Exp)
boxplot(rv_list$Gamma)


rv_df <- rv_list %>%
    imap(~data.frame(x = .x, name = .y)) %>%
    bind_rows()

rv_df

ggplot(rv_df, mapping = aes(x = x, fill = name)) + geom_boxplot() + coord_flip()
ggplot(rv_df, mapping = aes(x = x, fill = name)) + geom_boxplot()

#Показва как са изместени данните.
ggplot(rv_df, mapping = aes(x, name, fill = name)) + geom_violin() 

idx <- sample(1:2, size = N, prob = c(0.3, 0.7), replace = TRUE)

#Комбинация от 2 разпределения
temp <- cbind(rnorm(N, mean = 0), rnorm(N, mean = 5))

#Където е 1 се тегли от първото разпределение иначе от второт
temp[idx]

temp[idx] %>%
    hist(probability = TRUE)
#Бимодална плътност се нарича това
lines(density(temp[idx]))

#Тежка опашка == много outliers

xsim <- function(k, n, fn, ...) {
    sums <- rep(0, k)
    for (i in 1:k) {
        sums <- sums + fn(n, ...)
    }

    sums
}

xsim(100, 1000, rnorm, sd = 4, mean = 10) %>%
   hist(breaks = 100)

#Централна гранична теорема
#Като сумираме случайни величини винаги получаваме нормална
#Личи си от графиката

#Задача 3
library(UsingR)

max(babies$age)

#y - квантили на извадката x - емпирични квантили
#Ако са на прав линия то теоритичните и извадковите квантили съвпадат
qqnorm(rexp(1000))
