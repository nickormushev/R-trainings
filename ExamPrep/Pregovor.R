#Exam prep tasks
library(tidyverse)
library("MASS")

colnames(anorexia)

an <- anorexia[-c(3, 5, 7), ]

anorexia[order(anorexia$Postwt - anorexia$Prewt, decreasing = T), ] %>%
  head(1)


#Task 1 b)
anorexia %>%
  mutate(diff = Postwt - Prewt) %>%
  arrange(desc(diff)) %>%
  head(1)

#Mutate е яко. Добавя колоната diff. arrange после сортира.
#Готинки функции

anorexia %>%
  group_by(Treat) %>%
  summarise(mn = mean(Postwt - Prewt))

  #group_map(~mean(.x$Postwt - .x$Prewt)) %>% unlist()

anorexia %>%
  group_by(Treat) %>%
  summarise(md = median(Postwt - Prewt), qs = quantile(Postwt - Prewt),
            mn = mean(Postwt - Prewt), sd = sd(Postwt - Prewt))

library(ggplot2)

anorexia %>%
  ggplot(mapping = aes(x = Postwt - Prewt, fill = Treat)) +
  geom_density(alpha = 0.5)

anorexia %>%
  ggplot(mapping = aes(x = Postwt - Prewt, fill = Treat)) +
  geom_boxplot(alpha = 0.5)

anorexia %>%
  ggplot(mapping = aes(x = Prewt, y = Postwt, color = Treat)) +
  geom_point(alpha = 0.5)


anCBT <- anorexia[anorexia$Treat == "CBT", ]
quantile(anCBT$Postwt - anCBT$Prewt, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))

boxplot(anorexia$Postwt - anorexia$Prewt ~ anorexia$Treat)

control <- (anorexia$Postwt - anorexia$Prewt)[anorexia$Treat == "Cont"]
cbt <- (anorexia$Postwt - anorexia$Prewt)[anorexia$Treat == "CBT"]
ft <- (anorexia$Postwt - anorexia$Prewt)[anorexia$Treat == "FT"]

plot(density(control, na.rm = TRUE), xlim = c(-20, 30), ylim = c(0, 0.5), col = "red")
lines(density(cbt, na.rm = TRUE), col = "blue")
lines(density(ft, na.rm = TRUE), col = "green")


anCBT <- anorexia[anorexia$Treat == "CBT", ]
anFT <- anorexia[anorexia$Treat == "FT", ]
anCont <- anorexia[anorexia$Treat == "Cont", ]

shapiro.test(anCBT$Postwt - anCBT$Prewt)
shapiro.test(anFT$Postwt - anFT$Prewt)
shapiro.test(anCont$Postwt - anCont$Prewt)

t.test(anFT$Postwt, anCont$Postwt, alternative = "less")
wilcox.test(anCBT$Postwt, anCont$Postwt, alternative = "less")

t.test(anFT$Postwt - anFT$Prewt, anCont$Postwt - anFT$Prewt, alternative = "two.sided")

anorexia$diff <- anorexia$Postwt - anorexia$Prewt

df <- data.frame(
  diff = anorexia$diff,
  treat = anorexia$Treat
)

#Понеже не са нормално разпределени всички групи това не става
lm(df) %>%
  anova()

#Затова ползваме Крускал тест
#p-value-то е голямо и значи има група, която се различава от другите
kruskal.test(diff ~ Treat, anorexia)

#Task 2
#a)
  sample(1:6, 1000, replace = T, prob = c(1/10, 1/10, 1/10, 1/10, 1/10, 1/2))
  
#b)
throw <- function() {
  dice <- sample(1:6, 50, replace = T)
}

1:1000 %>%
  map(function(x) {
    t <- sum(throw() == 6)
    t >= 10 & t <= 20 
  }) %>% unlist() %>%
  sum()/1000

pbinom(20, 50, prob = 1/6) - pbinom(9, 50, prob = 1/6)

qbinom(0.5, 30, prob = 1/6, lower.tail = F)
  
prop.table(table(dice))

#c)
data <- rpois(1000, 4)
split.screen(c(1, 2))
screen(1)
barplot(prop.table(table(data)))
screen(2)
barplot(dpois(1:14, 4))

#d)
pexp(10, rate = 1/5)

#e)
pnbinom(4, 3, prob = 1/8, lower.tail = F)

#f)
#Тук няма значение дали p е <= или не защото е непрекъснато
pchisq(q = 20, df = 10) - pchisq(q = 10, df = 10)

#g)
qchisq(p = 0.75, df = 10, lower.tail = F)

#h)
dhyper(5, 20, 280, 30)
sum(dhyper(5:10, 20, 280, 30))
phyper(7, 20, 280, 30, lower.tail = F)



barplot(data)

