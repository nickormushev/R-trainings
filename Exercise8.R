#Като сумираме нормални разпределения
#получаваме по-малка вариация. N(m, s^2) <- преди сумиране
#Става на N(m, s^2/n) при сумиране на n нормални случайни величини
#Трябва да нормираме сумата (да я разделим на 1/n)
#Разпределението клони към истинското при повече опити

#Пример:
#Двете стойности отдолу не съвпадат.
#Колкото по-голямо е n толкова по-близо ще сме до 100
#при mean(x)
x <- rnorm(100, mean = 100, sd = 10)
mean(x)


t.test(x)

#Функция на правдоподобие
#L(x1,....,xn | miu, sigma^2) = П (fx(xi | (miu)^2, sigma^2))
# - наблюдавани стойности са x1, до xn

x <- rbinom(100, size = 10, prob = 1 / 2)
hist(x)

#Вероятността 2 от 10 пъти да имаме успех
pbinom(q = 2, size = 10, prob = 1 / 2)

#Хипотези
#H0 - основна хипотеза
#H1 - алтернатива

#Задача 1
n <- 30
x <- rnorm(n, mean = 2, sd = 2)
m <- mean(x)
s <- sd(x)

hypoth_mean <- 5
#t статистика
#центираме и скалираме нашата t статистика
t <- (m - hypoth_mean) / (s / sqrt(n))

#ниво на доверие/съгласие
alpha <- 0.5

#Долното е много малко вероятно да съвпадне.
#degrees of freedom(df) е n - 1.
#degrees of freedom - степените на t разпределнието 
#Има pt, dt, st, rt. - p значи вероятността да сме дефинирали такава
#или по-малка стойност.
2 * pt(t, df = n - 1) < alpha


#Смятаме горното, като трябва да зададем да е двустранен тестът
#mu - да проверим дали средната стойност е 5
#Прави горните стъпки по-кратко
t.test(x, alternative = "two.sided", mu = hypoth_mean)
#p.value - вероятността да се случи тази стойност или по-малка
#за mu


#б)
t.test(x, alternative = "two.sided", mu = 3)


#Task 2
library('UsingR')

#normal distribution check
hist(vacation)
qqnorm(vacation)
qqline(vacation)

#if p.value is above 0.05 the data is normal
shapiro.test(vacation)


#the same calculations as above by hand
n <- length(vacation)
s <- sd(vacation)
m <- mean(vacation)

mean_hypothesis <- 24

t <- (m - mean_hypothesis) / (s / sqrt(n))

alpha <- 0.04

pt(t, df = n - 1)
2 * pt(t, df = n - 1) <  alpha

#the calculation using t.test
#понеже  p.value > 0.02 не отхвърлям хипотезата
t.test(vacation, mu = 24, alternative = "two.sided", conf.level = 0.2)

#Първи аргумент е брой успехи, втори брой опити.
#3-ти дали гледаме отгоре или отдолу или от двете страни
#Гледаме компанията дали е права, че 50% са доволни
#С ниво на съгласие 0.05 сме съгласни, но с по-високо
#може да не сме
prop.test(42, 100, alternative = "less", conf.level = 0.05)


#Тук понеже хората са повече е по-малко вероятно да са прави
#от компанията
prop.test(420, 1000, alternative = "less", conf.level = 0.5)

#Грешка от първи род отхвърляме началната хипотеза, а тя е вярна
#Грешка втори род. Не се отхвърял H0, а тя е била грешна.

talks <- c(12.8, 3.5, 2.9, 9.4, 8.7, 0.7, 0.2, 2.8, 1.9, 2.8, 3.1, 15.8)

#разпределнието не е нормално. затова е wilcox.test
wilcox.test(talks, mu = 5, alternative = "greater")
