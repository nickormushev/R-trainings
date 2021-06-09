#anova - за една променлива. В смисъл само оценката
#как се влияе от метод на учене
#manova - за две променливи. Използвай cbind.
#Заплата и учене как се влияят от университет
#aov - гледа за различни пациенти как влияят лекарства.
#Може за един пациент да има няколко реда
library("tidyverse")

#Задача 1
exam_results <- data.frame(
    "examinor1" = c(5, 4, 4, 6, 4, 6, 3, 3, 4, 5),
    "examinor2" = c(3, 2, 4, 5, 3, 4, 3, 4, 2, 4),
    "examinor3" = c(4, 6, 4, 2, 4, 5, 5, 3, 6, 4)
)

qqnorm(exam_results$examinor1)
qqline(exam_results$examinor1)
shapiro.test(exam_results$examinor1)
shapiro.test(exam_results$examinor2)
shapiro.test(exam_results$examinor3)

#Първо гледаме дали предположенията за модела са валидни
#нормални ли са данните. Отговор да

exam_results %>%
    stack() %>%
    lm() %>%
    anova()

#kruskal.test, ако не са нормално разпределени данните
kruskal.test(values ~ ind, data = stack(exam_results))

#Задача 2

shapiro.test(InsectSprays[InsectSprays$spray == "E", ]$count)
#p value под 0.05 значи не е нормално разпределно
shapiro.test(InsectSprays[InsectSprays$spray == "C", ]$count)
shapiro.test(InsectSprays[InsectSprays$spray == "D", ]$count)

kruskal.test(count ~ spray, data = InsectSprays)

#Това с филтър май е по-лесно
data_id <- InsectSprays$spray == "D" |
    InsectSprays$spray == "C" | InsectSprays$spray == "E"

#Можем да отхвърлим нулевата хипотеза, че спрея няма значение
kruskal.test(count ~ spray, data = InsectSprays[data_id, ])

#За по-кратко shapiro.test
InsectSprays %>%
    group_by(spray) %>%
    summarise(pval = shapiro.test(count)$p.value)

#Добре e да се пробва и с двата теста, да си потвърдим резултатите,
#май защото има няколко нормално разпределени

InsectSprays %>%
    lm() %>%
    anova()

#Задача 3
drug_data <- read.csv("./drug.csv")

#Тества и сдвоеността на данните
aov(response ~ drug + Error(patient), data = drug_data) %>%
    summary()

#Задача 4
iris %>%
    manova(cbind(Sepal.Width, Sepal.Length) ~ Species, data = .) %>%
    summary.aov()
