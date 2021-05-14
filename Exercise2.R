#two types of variables. категорни - дискретни обекти.
#Example:
x <- factor(c("A", "V", "d"))
levels(x) <- c(levels(x), "new_level_d")

svector <- c(1, 2, 3, 4, 4, 5, 6, 7, 7, 1, 0, 1)

#lists number of time an elemente is in the vector
table(svector)

prop.table(table(svector))

#Displays a graph
barplot(table(svector))

pie(table(svector))

#The rest of exercise 2 is part of Exercise1.R
