mean(1:100)
#varaince explained:
#http://www.r-tutor.com/elementary-statistics/numerical-measures/variance
var(1:100)

#How is the data distributed:
#https://www.investopedia.com/terms/s/standarddeviation.asp#:~:text=A%20standard%20deviation%20is%20a,square%20root%20of%20the%20variance.&text=If%20the%20data%20points%20are,the%20higher%20the%20standard%20deviation.
#Farther away from the mean means higher sd
#This is the square root of the variance. This is part of lecture 4 where variance is explained
sd(1:100)

#Takes a sample of a given size from the given range
#If no size is given it generates a permutation
sample(1:100)
sample(1:100, 5)

#quantiles explained:
#https://www.statisticshowto.com/quantile-definition-find-easy-steps/#:~:text=In%20simple%20terms%2C%20a%20quantile,into%20areas%20of%20equal%20probability.
quantile(sample(1:100))
quantile(sample(0:100))
x <- sample(1:100, 50)

#summary depends on the class of the given argument
#prints out stuff like the max element, median and
#mean value etc.
summary(x)

#density is a single line that is continuous that approximates the data. 
#It actually generates data for x and y simmilar to summary and that
#data is used by the command lines to draw the line
density(x)
hist(x)

#rnorm generates n numbers close to the mean depending on sd
rnorm(n = 1000, mean = 176, sd = 7)
hist(x, probability = T)

#create a screen 2 x 2 and open it. Do not close if you want
#the screen commands to work
split.screen(c(2,2))
#choose screen 1
screen(1)
hist(x, probability = T)
lines(density(x))

#choose screen 2
screen(2)
hist(1:50, probability = T)
lines(density(1:50))

library(MASS)
library(tidyverse)

between(1:100, 3, 6)
#If I use sum on boolean values they are coverted like so false to 0 and true to 1
#This can be used to see number of true statements


#cut divides the data into n - 1 parts based on the breaks. Where the number of breaks is n
#These parts could be given labels to be more expressive. Here we have divided the data into two
#parts. For the people with height less then m - 2 * s + 2 * s and the people with a greather height
#s and m are at the end of the code
cut(survey$Height, breaks = c(0, m - 2 * s + 2 * s, 250)) %>%
    table()

#dot product - колко засилваме вектора показва преносно казано
#boxplot - shows how the data is distributed using a box. The line
#is the median and the circles above and below the lines are outliers
boxplot(1:100)

# Task 2
split.screen(c(2,2))
screen(1)
#The tilde makes it so it shows both for men and women
boxplot(survey$Height ~ survey$Sex)
screen(2)
#without probability true the lines does not work
hist(survey$Height, probability = T)
lines(density(survey$Height, na.rm = T))

screen(3)
hist(survey$Height[survey$Sex == 'Male'], probability = T)
lines(density(survey$Height[survey$Sex == 'Male'], na.rm = T))

screen(4)
hist(survey$Height[survey$Sex == 'Female'], probability = T)
lines(density(survey$Height[survey$Sex == 'Female'], na.rm = T))

#Task 3
hist(survey$Pulse, probability = T)
lines(density(survey$Pulse, na.rm = T))

#Task 4
library(UsingR)
#shows a table of the data
View(homedata)
colnames(homedata)
split.screen(c(2,2))
screen(1)
hist(homedata)
screen(2)
hist(homedata$y1970)
screen(3)
hist(homedata$y2000)

lines(cor(homedata$y1970, homedata$y2000))

# Task 1
m <- mean(survey$Height, na.rm = T)
s <- sd(survey$Height, na.rm = T)
median(survey$Height, na.rm = T)

sum(abs(survey$Height - m) <= 3 * s, na.rm = T)

plot(density(hf, na.rm = TRUE), xlim = c(130, 220), col = "red")
lines(density(hm, na.rm = TRUE), xlim = c(130, 220), col = "blue")

#Task 5
anscombe
