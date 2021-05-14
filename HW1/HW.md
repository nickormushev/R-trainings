---
title: "HW1"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
library("ggplot2")
library("tidyverse")
colnames(diamonds)
```
###Задача 1
#####a
```{r}
nrow(diamonds[diamonds$price > 15000,])
```

#####б
```{r}
nrow(diamonds[diamonds$carat < 2 & diamonds$price > 15000,])
```
#####в
```{r}
m <- mean(diamonds$price)
s <- sd(diamonds$price)
nrow(diamonds[diamonds$price < m + s & diamonds$price > m - s,])
```
#####г
```{r}
ideal <- diamonds[diamonds$cut == "Ideal",]
quantile(ideal$carat)
median(ideal$carat)
mean(ideal$carat)
```
#####д
```{r}
quantile(diamonds$price, prob = 0.8)
```
#####е
```{r}
diamonds[order(diamonds$x, diamonds$y, diamonds$z, decreasing = T),][1:5,]
```

###Задача 2
#####а
```{r}
hist(diamonds$price, probability = T)
lines(density(diamonds$price), na.rm = T)
```

#####б
Долният barplot показва условнаta вероятност, aко знаем каква е 
шлифовката каква е вероятността да имаме диамант с дадена чистота. 
Чистотата е подредена по качество. Тоест най-долу на легендата
е най-хубава(IF), a най-горе е най-лоша(I1). На диаграмата
се вижда, че колкото по-добра е шлифовката толкова по-висока е вероятността
диамантът да е с по-хубава чистота. Тоест ако шлифовката е идеално е доста
по-вероятно да имаме супер чист(IF) диамант отколкото, ако е ставаща(Fair)

```{r}
barplot(prop.table(table(diamonds$clarity, diamonds$cut), 2), legend = T, 
        beside = T, col = c("grey", "black", "purple", "blue", "green", 
        "yellow", "pink", "cyan")) 
```
Долният barplot показва  каква е вероятността, ако знаем чистотата
да имаме някоя от шлифовките. Както се вижда колкото по-чист ни е диамантът
толкова по-вероятно е да е с по-хубава шлифовка. Колкото по-мръсен е веряотностите
за качествена и некачествена и качествена шлифовка са доста по-близки. Вижда се
все пак, че дори от най-замърсените диаманти можеш да получиш идеална шлифовка, 
което също е интересно.
```{r}
barplot(prop.table(table(diamonds$cut, diamonds$clarity), 2), legend = T, 
        beside = T, col = c( "black", "green", "yellow", "purple", "pink")) 
```

#####в

Долната графика покзва каратите на диамантите и съответната им цена
разпределени в 2d равнина. От нея се вижда, че са зависи, както и може да се 
начертае проста линейна регресия, която да описва връзката между данните.

```{r}
plot(diamonds$price, diamonds$carat)
l = lm(carat ~ price, data = diamonds)
abline(l)
```

#####г

от трите диаграми се вижда, че чистотата влияе най-силно на цената
най-скъпи са чистите диаманти.
като цяло е интересно, че ако включ и диамантите под 1 карат 
диаграмите изглеждаха доста различно и беше много странно, защото
тогава цвета, като че ли играеше основна роля. Колко е чист диаманта
почти не беше от значение. Даже най-чистите бяха доста нодолу според
boxplot-а(разбира се с много outliers)

```{r}
caratsMoreThan1 <- diamonds[diamonds$carat > 1,]

plot(caratsMoreThan1$color, caratsMoreThan1$price)
plot(caratsMoreThan1$clarity, caratsMoreThan1$price)
plot(caratsMoreThan1$cut, caratsMoreThan1$price)
```
#Задача 3
#а)

#i не се ползва. По-долу обяснявам защо.
#Реших да си поиграя с функции от по-висок ред
#общо взето
add1 <- function (x, i) {
    x <- x + 1
}

#Изпълнение на един опит
draw <- function(whiteNum = 2, blackBalls = 2, ballNum = 4, N = 20, fn = add1, result = 0) {
    for (i in 1:N) {
        ball <- sample(c("white", "black"), 1, 
                    prob = c(whiteNum/ballNum, blackBalls/ballNum))
        if(ball == "white") {
            whiteNum <- whiteNum + 2
            ballNum  <- ballNum + 2
            result <- fn(result, i)
        } else {
            blackBalls <- blackBalls + 1
            ballNum  <- ballNum + 1
        }
    }

    result
}
 
attempts = 10000

#резултати от attempts брой опита
whiteBalls <- 1:attempts %>%
    map(~draw()) %>%
    unlist()

eProb <- function(n, whiteBalls) {
    sum(whiteBalls == n)/attempts
}

#Емпиричната вероятност X = 15
eProb(15, whiteBalls)

#Емпиричната вероятност X < 10
sum(whiteBalls < 10)/attempts

#б)
#Смятаме разпределението на X
xDistribution <- 0:20 %>%
    map(~eProb(.x, whiteBalls)) %>%
    unlist()

median(xDistribution)
quantile(xDistribution)

#в)
#Индексът съвпада с броя извадени бели топки
#Взимаме на кой индекс е максималната вероятност
#И получаваме най-вероятният брой бели топки
#Отдолу добавих и самата вероятност
which.max(xDistribution)
max(xDistribution)

#г)
#Теглили сме 2 черни топки и 0 бели
#Значи сме с +2 черни топки.
#тоест имаме 4 черни и 2 бели и общо 6 топки
#Ще симулирам само последните тегления
#Започваме от 2

#Ползвам функции от по-висок ред,
#да намаля повтарянето на код.
#Затова fifthBall и add1 приемат бонус
#ненужен аргумент, което не е много добре
#от гледна точка качество на код, ама върши 
#работа
fifthBall <- function (x, i) {
    #Тук гледаме дали на третото теглене
    #се вика тази функция. Ако се вика,
    #то петата изтеглена топка е бяла,
    #защото в този случай се вика функцията
    #i == 3, защото вече сме теглили два пъти.
    #Общо взето слага стойността на result на TRUE,
    #ако при последното теглене получим бяла
    i == 3   
}

#резултат от опитите
fifthIsWhite <- 1:attempts %>%
    map(~draw(2, 4, 6, 3, fifthBall, FALSE)) %>%
    unlist()

#Виждам броя пъти, в които петата топка е била бяла
#и деля на броя опити и получавам емпиричната вероятност
sum(fifthIsWhite)/attempts

#д)

Общо взето умножавам на всеки ход
по вероятността да извадим бяла топка
и добавям двете нови бели топки

tProb <- 1
whiteNum <- 2
ballNum <- 4
for (i in 1:20) {
    tProb <- tProb * whiteNum/ballNum
    
    whiteNum <- whiteNum + 2
    ballNum <- ballNum + 2
}

tProb
eProb(20, whiteBalls)