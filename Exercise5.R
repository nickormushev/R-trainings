library('tidyverse')

#Биномна случайна величина
sum(sample(c(0, 1), size = 1000, replace = TRUE,  c(0.5, 0.5)) == 1)

# Биномна случайна величина. size е брой опити
# size is the number of things that we may get and the prob is the probability for each side of the dice. This is a coin flip below
# Симулира опити. Примерно хвърляне на зар и монета
rbinom(n = 100, size = 1, prob = 1/2) %>%
 table() %>% 
 prop.table()

#плътност на функция. Питаме за теритичната вероятност да имаме 5 от 30 опита
dbinom(x = 5, size = 30, prob = 1/6)
dbinom(x = 0:3, size = 30, prob = 1/6)

#комулативна плътност на функция
dbinom(x = 0:30, size = 30, prob = 1/6) %>%
    cumsum() %>%
    barplot()

#<= is used in the function below. Вероятността стойностите да са по-малки или равни от 5 връща
pbinom(q = 5, size = 30, prob = 1/6)

#Питаме кое е числото, което ще се падне число по-малко от него с вероятност 0.62
qbinom(p = 0.62, size = 30, prob = 1/6)

#Има rgeom, dgeom и тн само че с геометрична веорятност


#Task 1
#30 пъти хвърляме зар 1000 пъти и гледаме колко 6ци са се паднали. 1/6 е вероятността да се падне 6
#Числото показва колко пъти се е паднала 1 страна
x <- rbinom(n = 1000, size = 30, prob = 1/6) %>%
    table() %>%
    prop.table()

x %>% barplot()

dbinom(x = 0:15, size = 30, prob = 1/6) %>%
    barplot()

# можем да твърдим, че с вероятност 0.25 ще се паднат най-много колко шестици?
qbinom(p = 0.25, size = 30, prob = 1/6)

# Докъде трябва да интегрираме да е сумата 0.75
qbinom(p = 0.75, size = 30, prob = 1/6, lower.tail = FALSE)


#Task 2
#а)
# x number of failures. size is number of successes. Броя опити е 3 + 5 = 8
dnbinom(x = 5, prob = 0.2, size = 3)

#б)
#lower.tail = FALSE гледа от тази стойност нататък, т.е. от Q нататък. Опитите са 6. 3 успеха и 3 неуспесха
pnbinom(q = 3, size = 3, prob = 0.2, lower.tail = FALSE)

#в)
#2-5 неуспеха. 3 успеха да имаме. Показва вероятността да имаме 2 или 3 или 4 или 5 неуспеха
#Това е между 5 и 8, защото към всяко от 2:5 прибавяме 3, за броя успехи
sum(dnbinom(x = 2:5, size = 3, prob = 0.2))


# смятаем вероятността да хвърли под 8 и махаме случаите, в които е хвърлил под 3. Алтернативно решение
pnbinom(5, size = 3, 0.2) - pnbinom(q = 1, 3, 0.2)

#Task 3
attempts = 1000
#Hypergeometric Distribution
#dhyper, qhyper, rhyper...
#За теглене на топки е идеално
#rhyper nn е броя симулации, m - брой бели топки, n брой черни, брой тегления
#a)
sims <- rhyper(nn = attempts, m = 7, n = 6, k = 8)

xRange <- unique(sims)

sum(sims == 3)/attempts

ex <- 0
#Why do I get an error for the {
for (i in xRange) {
   ex <- ex + i * sum(xsim == i)/attempts
}

ex

#Шахпазов
xsim <- rhyper(nn = attempts, m = 7, n = 6, k = 8)
#EX smart way
mean(xsim)
#DX дисперсия
var(xsim)

#медианата е най-често срещаната стойност
#b)
th_probs <- dhyper(x = 0:8, m = 7, n = 6, k = 8)

#Умножаватсе поелементно. Като моето без for-а
th_mean <- sum(th_probs * 0:8)
#E((X - EX)^2)
sum((th_probs * (0:8 - th_mean)^2))

emp_probs <- xsim %>%
    table() %>%
    prop.table()

#rbind добавя колони
rbind(emp_probs, th_probs[2:7]) %>%
    barplot(beside = TRUE, legend.text = c("empirical", "theoretical"))
