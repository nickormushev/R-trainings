# function is a first class object
sum3 <- function (x = 12, y = 15, z = 320) {
    #the last row has an implicit return
    x + y + z
}

sum3(1, 2, 3)

#closure
func <- function (x = 12) {
    function (y = 12) {
        x + y
    }
}

func(14)(15)

for (i in 1:100) { 
    print(i/100)
}

library(tidyverse)
library(purrr) # Google cheat sheet

#lambda functions
map(1:19, ~.x + 14) %>%
    unlist()
#unlist makes list a vector

#Like filter
unlist(keep(1:10, ~.x %% 2 == 0))

#Tells you if you have an element that gives 0 mod 2
some(1:10, ~.x %% 2 == 0)


#Task 1
birthdaysLeft <- function (n) {
    (366 - n)/365
}


sameBirthday <- function (p = 1/2) {
    probability <- 1;

    for (i in 1:365) {
        probability <- probability * birthdaysLeft(i)
        if(probability <= p) {
           return(i)
        }
    }
    print(i) #i is not local to the for
    return(-1);
}

sameBirthday()

#Alternate solution
sameBirthday2 <- function (p = 1/2) {
    #cumprod on index one returns the multiplication of the first two elements. On the second index of the first three
    cumprod((366 - 1:365)/365) %>%
        detect_index(~. <= p) #gets the first index that is lower than p
}

sameBirthday2()


#Task 2
#sample is a random generator
sample(1:6, 6) # throw 6 times withour repeating
sample(1:6, 100, replace = TRUE) 
#throwing 100 times without repeating 1:6
#is impossible so we give replace = TRUE which allows results to repeat

x <- c(1,2)
x <- c(x, 3)

throw100 <- function (n = 0) {
    x <- c();
    for (i in 1:n) {
        x <- c(x, sum(sample(1:6, 100, replace = TRUE) == 6)/n)
    }
    x
}


#how to plot a line. It connects the dots from the vector
plot(sample(1:100, 100), type = "l")
#draws a line
abline(h = 40, col = "red")

plot(throw100(100), type = "l")
abline(h = 1/6, col = "red")

#repeat 1 100 times
rep.int(1, 100)

#alternate
dice_prob <- function(n = 100) {
    mean(sample(1:6, n, replace = TRUE) == 6)
}

N <- 10000
1:N %>%
    map(~dice_prob()) %>%
    cummean() %>%
    plot(type = "l")

#cummean is like cumsum. First place is the mean of the first element. 
#Then the second and first. After that
#the mean of the first three etc
abline(h = 1/6, col = "red")

#Task 3
#p1 <- P(boy wins against dad)
#p2 <- P(boy wins against mom)

p1 <- 0.3
p2 <- 0.4

#The first case is if he wins against his dad and then against his mom
#Second case is he loses thefirst game but wins the other two games
#First plays with dad
dadFirst <- p1 * p2 + (1 - p1) * p2 * p1

#First plays with mom
momFirst <- p2 * p1 + (1 - p2) * p1 * p2

c(dadFirst, momFirst)
