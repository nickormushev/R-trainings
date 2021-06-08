A <- c(1, 2, 3)
help(package= 'dplyr')
x  <- 12

y  <- 1:100

typeof(y)
typeof(A)

some_vector <- c(1, 2, 3, 4, 5, 11, 1, 1, 4)
some_vector[c(1, 2, 3)]
some_vector[5:8]

5 > 1

#Returns result for every part of a vector
A > 2

#Like %/modulus
A %% 2

#Something like a filter. Mask is an array with true an false When we give it
#as an argument to some_vector we get the true positions only
mask <- some_vector %% 2 == 0
some_vector[mask]

#Below also works
some_vector[some_vector %% 2 == 0]

#matrices
myMatrix <- matrix(data = 1:9, nrow = 3, ncol = 3)
myMatrix[3,2]                          # 
myMatrix

#Get last row
myMatrix[3,]

#Get last column
myMatrix[,3]

#submatrix
myMatrix[c(1,2),c(2,3)]

sort(some_vector)
#Returns the indexes ordered so the elements are sorted
order(some_vector)
some_vector[order(some_vector)]

min(some_vector)
max(some_vector)

#index of max and min elements
which.max(some_vector)
which.min(some_vector)

which(some_vector %% 2 == 0)

#install.packages('tidyverse', dependencies=TRUE)
#install.packages('UsingR', dependencies=TRUE)
library('tidyverse')
library('UsingR')

#vector
v  <- c(8, 3, 8, 7, 15, 9, 12, 4, 9, 10, 5, 1)
#matrix
m  <- matrix(v, 4, 3, dimnames = list(c("a", "b", "c", "d"),c()))
#colnames(matrix) <- c("a","b","c") gives column names
#rownames(matrix) <- c("a",...) gives row names

#order by first column
sr <- order(m[,1])
m[sr,]

#order by first two columns
#According to order docs the second argument is a tie breaker
sr <- order(m[,1], m[,2])
m[sr,]

#No duplication just change v
#dim(v) <- c(4, 3)
#dimnames(v) <- list(row_names, col_names)

library('dplyr')
v %>% map(function(x) x + 1) %>% filter(x == 11)


#Examples
#Add column 1,2,3,4
cbind(m, c(1,2,3,4))
#adds columns with names start end and by
cbind(m, start = 1, end = 7, by = 2)
#seq generates a sequence by start and end 
#number and by what amount to jump and skip 
cbind(m, seq(from = 1, to = 7, by = 2))

#like a matrix. Extend matrices in some way
df <- data.frame(ages = c(1, 5, 12), names = c("a", "b", "radio gaga"))
df$ages
df

#Like a hashmap
l <- list(age = 2, firstName = "Nikolay")
l$age
l$firstName
#Indexes work as well
l[1]
l

df %>%
    select(ages, names) %>% #like sql select
    filter(ages > 10) %>%
    mutate(ages = ages + 1)

homedata[order(homedata[,2], decreasing = TRUE)[1], ]
which.max(homedata[,2])
which.min(homedata[,2])

#The above is easier
most_expensive <- order(homedata[,2], decreasing = TRUE)[1]
cheapest <- order(homedata[,2])[1]

homedata[c(most_expensive, cheapest),]

top5 <- order(homedata[,1], decreasing = TRUE)[1:5]
homedata[top5,]

#number of elementes that cost more than 750000
len <- length(homedata$y2000[homedata$y2000 > 750000])

#probably a smarter and simpler way to get the length is:
nrow(homedata[homedata$y2000 > 750000,])

#average of the cost of the above houses 
sum(homedata$y1970[homedata$y2000 > 750000])/len
#the average with a function
mean(homedata$y1970[homedata$y2000 > 750000])

homedata$y2000 < homedata$y1970
#price in the year 2000 of the houses whos price decreased
homedata[homedata$y2000 < homedata$y1970,]

testL <- c(1,2,3)
testR <- c(3,4,5)
#Compares each element separately
testL < testR

# The ten houses with the greatest percentage increase
greatestIncrease <- order((homedata$y2000 - homedata$y1970)/homedata$y1970, decreasing = TRUE)[1]
homedata[greatestIncrease,]

#install.packages('MASS', dependencies = TRUE)
library('MASS')
survey
men = survey[survey$Sex == "Male",]
numMen <- length(survey$Sex[survey$Sex == "Male"])

smokersIndex <- survey[survey$Sex == "Male",]$Smoke != "Never"
nrow(survey[smokersIndex,])

validHeights <- men[men$Height != "NA",]$Height
mean(validHeights, na.rm = TRUE)

survey[order(survey$Age),]$Age

survey[order(survey$Age)[1:6],] %>%
    dplyr::select(Sex, Height) %>%
    print

#Excercise 2
#Alternative solution of the above
survey %>%
    arrange(Age) %>%
    dplyr::select(Sex, Height)

#This is valid
#Three times column 1
survey[,c(1,1,1)]

#Three times row 1
survey[c(1,1,1),]

#add element to end of vector: vector[ length(vector) + 1 ] <- value

#Shows the number of people of each sex
table(survey$Sex)
#Shows for each pulse value how many people have that pulse
table(survey$Pulse)

hardSmokersIndex <- survey[survey$Sex == "Male",]$Smoke == "Regul"
mean(hardSmokersIndex, na.rm = TRUE)

#Calculates the probability of someone being a smoker
prop.table(table(survey$Smoke))

#The second argument is used to say what we want 
#to divide the table by. In this case based on sex. This is в). 
#This is relative probability.
prop.table(table(survey$Sex, survey$Smoke), 1)

hardSmokersMen <- na.omit(men[men$Smoke == "Regul",])
hardSmokeMenNum <- length(hardSmokersMen)
hardSmokeManChance <- hardSmokeMenNum / length(men$Sex)

#б) Probability that a randomly chosen person is a smoker
prop.table(table(survey$Sex, survey$Smoke))
sum(prop.table(table(survey$Sex, survey$Smoke))[2,])

#г)
table(survey$Sex, survey$Smoke)
prop.table(table(survey$Sex, survey$Smoke), 2)

pie(prop.table(table(survey$Smoke)))
#prop.table - this is a conditional probability table. So the percentages is the probability that
#someone is a man and a smoker divided by the number of men. The same goes for the women.
#This is not classical probability. I think the 2 determines that we have the above mentioned
#probability. If I place a one it is the probability that x is a hard smoker that he is a man 
#Now we have if x is a man what is the probability that he is a hard smoker with the 2
barplot(prop.table(table(survey$Smoke, survey$Sex), 2), beside = TRUE, legend = TRUE)

#The below shows how many people of each kind we have. The above shows the probability of
#a certain person being a smoker
barplot(table(survey$Sex, survey$Smoke), beside = TRUE, legend = TRUE)
pie(prop.table(table(survey$Smoke, survey$Sex), 2))

#Returns possible values of Smoke/levels
levels(survey$Smoke)

levels(survey$Smoke) <- levels(survey$Smoke)[c(2,3,4,1)]

#ggplot - library for graphics. Install ggplot to test below
#I think something is wrong for the text below
survey[!is.na(survey$Sex) & !is.na(survey$Smoke), ] %>%
    ggplot(mapping = aes(x = Smoke, group = Sex, fill = Sex))
        + geom_bar(position = "dodge")
        + theme_dark()


survey$AgeCategory <- ifelse(survey$Age < 20,
                             "A",
                             ifelse (survey$Age < 25, "B", "C"))

pie(table(survey$AgeCategory))
barplot(table(survey$Smoke, survey$AgeCategory), beside = TRUE, legend = TRUE)

diamonds$carat
