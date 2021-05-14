library(tidyverse)

#Задача 1
lambda <- 1 / 3
rexp(500, rate = lambda)
sim_num <- 500
observation_num <- 1000

simulated_observations <- 1:sim_num %>%
    map(~rexp(observation_num, rate = lambda))

simulated_observations

x_1 <- simulated_observations[1] %>%
    unlist()

hist(x_1)

#а)
#Теоритични квартили
qexp(c(0, 0.25, 0.5, 0.75, 1), rate = lambda)

#емпирични квартили
quantile(x_1)

#б)
x_2 <- simulated_observations[2] %>%
    unlist()

y <- x_1 / (x_1 + x_2)

#Според диаграмата разпределението ми изглежда равномерно
hist_output <- hist(y)
lines(density(y))
hist(y)

qqnorm(y)
qqline(y)

#Ще направим chisq.test, да проверим хипотезата ми дали е вярна.
#p-value-то е единици, което е свръх категорично, че съм прав.
#това е грешно
chisq.test(y)

#вярно
#Ще направя chisq.test, да проверя хипотезата си дали е вярна.
#Първо разделям данните на секции и гледам колко често попадаме
#във всяка секция. После в prob смятам теоритичните вероятности
#и накрая с chisq.test проверявам хипотезата, че теоритичните
#и емпиричните вероятности съвпадат. Има 26% това да е в сила,
#което означава, че хипотезата ми не може да се изхвърли и
#приемам, че е вярна.
t <- table(cut(y, breaks = c(0, 1 / 4, 2 / 4, 3 / 4, 1)))

prob <- c(1, 1, 1, 1)
prob[1] <- punif(1 / 4) - punif(0)
prob[2] <- punif(2 / 4) - punif(1 / 4)
prob[3] <- punif(2 / 4) - punif(3 / 4)
prob[4] <- punif(3 / 4) - punif(1)

chisq.test(t, prob)

#в)
#Намерих Reduce в интернет и ми изглежда по-чисто от това да пиша for.
x_sum <- Reduce(`+`, simulated_observations)

#по хистограмата ми изглежда, като нормално разпределение, а и
#според централна гранична теорема е логично това да получим.
hist(x_sum)

#shapiro тестът даде доста високо p-value, което потвърждава хипотезата ми
shapiro.test(x_sum)


#Задача 2
colnames(morley)

exp4 <- morley[morley$Expt == 4, ]

#От хистограмата не е много ясно дали е нормално разпределно,
#макар че леко прилича не бих казал, че бих бил на 100% сигурен
hist(exp4$Speed)

#С qqnorm и qqline е по-явно обаче, че има линейна зависимост
#и че данните са нормално разпределени
qqnorm(exp4$Speed)
qqline(exp4$Speed)

#За всеки случай реших и един shapiro test да пусна
#И той е съгласен с мен
shapiro.test(exp4$Speed)

#Горните заключения означават, че можем да използваме t.test,
#да получим доверителния интервал. Излиза, че има 97% шанс
#скоростта на светлината да е в доверителния интервал (789.008, 851.992)
t.test(exp4$Speed, conf.level = 0.97)

#Задача 3
library("MASS")
colnames(birthwt)

#Като начало реших да направя barplot, който сравнява показва
#в случай, че майката е пуши каква е вероятността да има
#бебе с ниско тегло и ако не пуши каква е вероятността да стане
#същото. На диаграмата се вижда, че се увеличава значително
#вероятността да имаш дете с ниско тегло, ако пушиш, както
#би се очаквало.
barplot(prop.table(table(birthwt$low, birthwt$smoke), 2),
        legend = T, names.arg = c("Does not smoke", "Smokes"),
        legend.text = c("baby is not low", "baby is low"), beside = T)

#Идеята ми да потвърдя хипотезата си е да я ползвам prop.test
#Взимам множеството от пушачите
smokers <- birthwt[birthwt$smoke == T, ]

#Също и множеството от непушачите
non_smokers <- birthwt[birthwt$smoke == F, ]

#Гледам броя случаи, в които пушач е имал дете с ниско тегло
smokers_low_babies <- sum(smokers$low == T)
#И общия брой на пушачите
smoker_count <- nrow(smokers)

#Аналогично за непушачите
non_smokers_low_babies <- sum(non_smokers$low == T)
non_smoker_count <- nrow(non_smokers)

#Правя prop.test, в който проверям дали вероятността на това пушач да има
#дете с тегло под 2.5 кг е по-голяма от тази при непушач
#Резултатът е катогерично да с 98% вероятност.
prop.test(c(smokers_low_babies, non_smokers_low_babies),
          c(smoker_count, non_smoker_count), alternative = "less")

#Задача 4
colnames(iris)

#а)

#Като начало гледаме хистограмата, qqnorm, qqplot
#и за здраве правим и един shapiro.test, от което
#явно се вижда, че данните не са нормално разпределени
hist(iris$Petal.Length)
qqnorm(iris$Petal.Length)
qqline(iris$Petal.Length)

shapiro.test(iris$Petal.Length)

#Затова използваме wilcox.test и смятаме и гледаме
#дали разликата между дължината и ширината * 3 е нула
#Вижда се, че вероятността това да е така е 16%, което
#означава, че не можем да отхвърлим тази хипотеза
wilcox.test(iris$Petal.Length, 3 * iris$Petal.Width,
            alternative = "two.sided")

#б)
#Започнах с проверка дали данните са нормално разпределени
#Работих аналогично на а). Тук според диаграмите и шапиро
#тестът се вижда, че имаме нормално разпределени данни
#и можем да използваме t.test
versicolor <- iris[iris$Species == "versicolor", ]
virginica <- iris[iris$Species == "virginica", ]

hist(versicolor$Sepal.Width)
qqnorm(versicolor$Sepal.Width)
qqline(versicolor$Sepal.Width)

shapiro.test(versicolor$Sepal.Width)

hist(virginica$Sepal.Width)
qqnorm(virginica$Sepal.Width)
qqline(virginica$Sepal.Width)

shapiro.test(virginica$Sepal.Width)

#Тестът показва, че вероятността versicolor да са с по-голяма дължина от
#virginica е под 1%, което означава, че хипотезата ни е грешна и можем да
#я отхвърлим
t.test(versicolor$Sepal.Width, virginica$Sepal.Width, alternative = "less")

#в)
#Първо взимаме iris-ите от тип setosa с дължина на венчелистчетата по-малка или
#равна на 1.4
setosa <- iris[iris$Petal.Length <= 1.4 & iris$Species == "setosa", ]

#Според графиките и shapiro.test не са нормално разпределени данните
#Затова ще използваме wilcox.test
hist(setosa$Petal.Width)
qqnorm(setosa$Petal.Width)
qqline(setosa$Petal.Width)

shapiro.test(setosa$Petal.Width)

#Вижда се, че вероятността да е по-голямо от 0.26 е много малка (под 1%)
#Затова хипотезата ни може да се отхвърли
wilcox.test(setosa$Petal.Width, mu = 0.26, alternative = "less")
