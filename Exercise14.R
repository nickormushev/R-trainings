library("tidyverse")

st <- read.csv("students.txt")

train <- st[101:400, ]
test <- st[1:100, ]

chisq.test(train$admit, train$rank)
n <- nrow(st)

correct_sum <- 0

for (i in 1:n) {
    train <- st[-i, ]
    test <- st[i, ]

    m <- glm(admit ~ ., data = train, family = "binomial")
    predictions <- predict.glm(m, test, type = "response") > 0.5
    correct_sum <- correct_sum + (predictions  == test$admit)
}

#Показва колко често модела ни познава
correct_sum / n

library(mlbench)
data(BreastCancer)

train_idx <- sample(1:nrow(BreastCancer), size = nrow(BreastCancer) * 0.7)
train <- BreastCancer[train_idx, ]
test <- BreastCancer[-train_idx, ]
test
m <- glm("Class ~ Cl.thickness" , train, family = "binomial")
p <- predict.glm(m, newdata = test, type = "response")

v <- 0
correctBest <- 0

for (i in colnames(BreastCancer)[-c(1, 7, 11)]) {
    f <- as.formula(paste0("Class ~ ", i))
    m <- glm(f, train, family = "binomial")

    p <- ifelse(predict.glm(m, newdata = test, type = "response") > 0.5, "malignant", "benign")
    correct <- sum(p == test$Class)
    if(correct > correctBest) {
        correctBest <- correct
        v <- i
    }
}
v

# зареждане на пакета и данните
library("mlbench")
data("BreastCancer")

# разделяне на 70% на 30% за трениране и за тестване 
# 70% за трениране на модел и 30% за тестване


# общ брой елементи
n <- nrow(BreastCancer)

# Проверка на данните
BreastCancer %>%
  select_if(function(x) any(is.na(x))) %>%
  summarise_each(funs(sum(is.na(.))))

# Bare.nuclei има 16 NA наблюдения
# можем да ги махнем за да не влошават модела

set.seed(10)
# индекси на елементите които ще участват в тренировката
train_idx <- sample(1:n, size = round(n * 0.7))

# множество от наблюдения за трениране на модел
train_df <- BreastCancer[train_idx, ]

# множество от елементи върху което ще тестваме колко добре прогнозираме
test_df <- BreastCancer[-train_idx, ]

# базов модел в който няма никакви предиктори\независими променливи (features)
base_model <- glm(Class ~ 1, data = train_df, family = "binomial")


# Алгоритъм на forward subset selection (FSS)
# започваме с базов модел и взимаме точността му

# итерираме по всички променливи
# пробваме да видим дали всъщност някоя от променливите
# подобрява прогнозата на базовия модел
# Избираме тази променлива която дава най-добра точност

# прогнозирани вероятности на базовия модел
predicted_probs <- predict.glm(base_model, newdata = test_df, type = "response")

# прогнозирани класове на базовия модел
predicted_class <- ifelse(predicted_probs > 0.5, "malignant", "benign")

# базовия модел класифицира всичко като класа който е по-често срещан (не е много умен)

# точност на базовия модел
base_accuracy <- mean(predicted_class == test_df$Class)
# > base_accuracy
# [1] 0.6333333
# все пак дава някаква базова точност

# най-добра точност и най-добра променлива
best_accuracy <- base_accuracy
best_variable <- NULL

# итерираме по всички променливи без Id и Class
for (var in colnames(test_df)[-c(1, 6, 7)]) {

  # изграждане на формула за текущия модел
  current_formula <- as.formula(paste0("Class ~ ", var))

  # текущ модел
  current_model <- glm(current_formula, data = train_df, family = "binomial")

  # прогнозирани вероятности
  predicted_probs <- predict.glm(current_model, newdata = test_df, type = "response")

  # прогнозирани класове
  predicted_class <- ifelse(predicted_probs > 0.5, "malignant", "benign")

  # Текуща точност на прогнозата
  current_accuracy <- mean(predicted_class == test_df$Class)

  # гледаме дали текущата точност е по-добра от базовата
  if (current_accuracy > best_accuracy) {
    # ако е по-добра, казваме че най-добрата е текущата (линейно търсене на максимум)
    best_variable <- var
    # обновяваме най-добрата точност
    best_accuracy <- current_accuracy
  }
}

# резултат
c("най-добра променлива" = best_variable, "точност" = best_accuracy)

# Избираме да добавим променливата "Cell.size" понеже дава най-добра точност при прогнозиране

# Можем да повторим подхода, но този път базовата ни точност ще е точността която дава
# променливата "Cell.size"

# при всяка итерация базовия модел ще е Class ~ beta0 + beta1 * Cell.size и ще търсим променлива 
# която да има по-добра точност от него

# А текущата формула във всяка итерация ще е paste0(Class ~ Cell.size + ", var)

# получаваме че следващата най добра променлива е "Cl.thickness".
# Тя качва точността на прогнозата до около 95%

# Повтаряме още веднъж с нова формула paste0("Class ~ Cell.size + Cl.thickness + ", var)

# получаваме следваща променлива Marg.adhesion

# Следващи итерации на модела не ни качват точността на прогнозиране върху тестовото множество

# Финалния модел който получаваме е Class ~ Cell.size + Cl.thickness + Marg.adhesion
# а финалната точност е 96%

# Евентуално може да добавим и Bl.cromatin, макар че не качва точността особено

# пакети
library(tidyverse)


############################################################
############ Задача 1 - Решение ############################
############################################################

# зареждане на данните
students_df <- read.csv("./student.txt")

# първите пет елемента
head(students_df, 5)

# таблица и barplot на приетите студенти
students_df$admit %>%
  table() %>%
  prop.table() %>%
  barplot()

# визуализация на разпределенията на двете групи (приети и неприети)
students_df %>%
  ggplot(mapping = aes(x = gre, fill = as.factor(admit))) +
  geom_density(alpha = 0.4)

# t.test за двете групи (приети и неприети)
t.test(
  students_df$gre[students_df$admit == 0],
  students_df$gre[students_df$admit == 1]
)

# смяна от числени към фаткорни променливи
students_df <- students_df %>%
  mutate(admit = as.factor(admit), rank = as.factor(rank))

# тест дали има зависимост между rank на колеж и приемането в университет след това
table(students_df$admit, students_df$rank) %>%
  # Хи-квадрат тест върху таблицата
  chisq.test()
# => rank и admit са зависими

# модел на логистична регресия с всички променливи
students_model <- glm(admit ~ ., family = "binomial", data = students_df)


# тестване за модела
# Jackknife алгоритъм

# 1. махаме 1 наблюдение от данните
# 2. тренираме модела върху останалите наблюдения
# 3. гледаме колко добре прогнозираме върху наблюдението което сме махнали
# връщаме точност като процент колко сме познали

# Пример с първата итерация
students_df <- read.csv("./student.txt")
THRESHOLD <- 0.5 # праг над който да класифицираме като приет в универститет

sum <- 0
for (idx in 1:nrow(students_df)) {
  # 1. Разделяне на множества за напасване и за тестване на модела
  train_df <- students_df[-idx, ]
  test_df <- students_df[idx, ]
  
  # 2. Напасваме модела върху данните за напасване
  model <- glm(admit ~ ., train_df, family = "binomial")
  
  # прогнозираме върху наблюдение за което вече знаем дали е приет или не
  predicted_probability <- predict.glm(model, newdata = test_df, type = "response")
  predicted <- as.numeric(predicted_probability > THRESHOLD)
  
  # истински стойности
  actual <- test_df$admit[1]

  # гледаме дали сме познали
  sum <- sum + (predicted == actual)
}

# резултат
sum / nrow(students_df)

# продължаваме по останалите
# сумираме броя на верните прогнози и делим на броя на всички елементи (mean)
# така че да видим процентово колко сме прогнозирали\класифицирали правилно
