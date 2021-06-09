#Колкото повече симулирани данни имаме толкова по-точно оценки имаме
#като ги сумираме.
#Ако подадем lm на summary виждаме дали beta0 и beta1 влиаят.
#Ако бетата пред x-а е 0, то x-а не влияе на y

library("tidyverse")

age <- c(18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37)
pulse <- c(202, 186, 187, 180, 156, 169, 174,
           172, 153, 199, 193, 174, 198, 183, 178)

lm(pulse ~ age) %>%
    summary()

t_stat <- (-0.79773 - (-1)) / 0.06996
d <- length(age) - 2
pval <- 2 * pt(t_stat, df = d, lower.tail = F)
pval

#С долната команда виждаме най-ниския, максималния,
#и средния пулс на човек на съответно 30 40 и 50 години
predict.lm(
    lm(pulse ~ age),
    data.frame(age = c(30, 40, 50)),
    interval = "confidence",
    level = 0.90
)

#Task 2
colnames(mtcars)

#mpg is the y and vs is the x
#it asks is there a linear relation
#between mpg and vs

#if i am corrcet beta1 is 7.94
#which means that a straight vs
#means more miles/gallon
lm(mpg ~ vs, mtcars) %>%
    summary()

lm(mpg ~ wt, mtcars) %>%
    summary()

#Here a straight vs means less horsepower
lm(hp ~ vs, mtcars) %>%
    summary()


patients <- data.frame(
   age = c(18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37),
   pulse = c(202, 186, 187, 180, 156, 169,
             174, 172, 153, 199, 193, 174, 198, 183, 178)
)

plot(patients)
abline(lm(pulse ~ age), patients)
