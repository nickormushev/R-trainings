#Residula vs fitted - моделът не е добър, ако има много различни линии
#вторият plot standardized residues and theoretical quantiles
#трябва да са на права линия
#plot от lm генерира тези графики
#F statistics проверява дали всички коефициенти са равни на нула.
#нейната нулева хипотеза е, че са нула.
#R^2 R-squared - добро напасване. Може да е твърде добре напсанато

#търсим променливте и правим следното
#lm(y ~ +xi) - итерира
#Избираме стойността, която дава най-висок R^2
#Това е forward subset selection

library("tidyverse")
library("dplyr")
library("MASS")

lm(mpg ~ hp, data = mtcars) %>%
    summary()

lm(mpg ~ vs, data = mtcars) %>%
    summary()

lm(mpg ~ wt, data = mtcars) %>%
    summary()

lm(mpg ~ cyl, data = mtcars) %>%
    summary()

lm(mpg ~ disp, data = mtcars) %>%
    summary()

lm(mpg ~ carb, data = mtcars) %>%
    summary()

#ако сложа 0 + отпред се маха intercept
lm(mpg ~ hp + wt + cyl, data = mtcars) %>%
    summary()

#умния начин
mtcars %>%
    imap(~cor(mtcars$mpg, .x)) %>%
    unlist() %>%
    sort()

#добре е данните да си ги скалираме
#вадим средното и делим на стандартното отклонение
scale(mtcars$mpg)

#създава нов data frame като промени променливата
mtcars %>%
    mutate(mpg = scale(mpg))

#така се добавя нова променлива
mtcars %>%
    mutate(mpg2 = scale(mpg))


#Така по-добре се работи с линейна регресия
mtcars %>%
    mutate(
        mpg = scale(mpg),
        disp = scale(disp),
        hp = scale(hp),
        wt = scale(wt),
        drat = scale(drat),
        qsec = scale(qsec),

        cyl = as.factor(cyl),
        vs = as.factor(vs),
        am = as.factor(am),
        gear = as.factor(am),
        carb = as.factor(carb)
    ) -> scaled_mtcars

#dummy.encoding виж, ако искаш да видиш as.factor

#Можем да видим R^2 дали расте
#между горното и долното, където
#сме добавили cyl. Това може да ни е индикатор
#колко добър ни е моделът
lm(mpg ~ wt, data = scaled_mtcars) %>%
    summary()

lm(mpg ~ wt + cyl, data = scaled_mtcars) %>%
    summary()

#може string да го превърнем във формула
#с as.formula
as.formula("x ~ y")

#конкатенация на стрингове
paste0("alabala-", "xxxx")

#този списък, ако го оправя поакзва,
#кои променливи подобряват модела.
#примерно horsepower подобряват модел
scaled_mtcars %>%
    dplyr::select(-mpg) %>%
    imap(
        ~paste0("mpg~ ", .y) %>%
        as.formula() %>%
        lm(data = scaled_mtcars) %>%
        summary() %>%
        .$r.squared
    ) %>%
    unlist() %>%
    sort()

model  <- lm(mpg ~ wt + cyl + hp, data = mtcars)
model_summary  <- summary(lm(mpg ~ wt + cyl + hp, data = mtcars))

#Отрицателни са защото са скалирани данните около нулата
predict.lm(model, data.frame(wt = c(100),
                cyl = c(2), hp = c(80)), interval = "confidence")

#Добра идея е да разделим данните на две части.
#Такива за трениране и такива за тестване
#Това е по-скоро, ако искаме успешно да предсказваме y
#спрямо подаден x. Иначе
train_idx <- sample(1:nrow(mtcars), size = nrow(mtcars) * 0.8)
test_idx <- -train_idx

train_set  <-  scaled_mtcars[train_idx, ]
test_set <- scaled_mtcars[test_idx, ]

model1  <- lm(mpg ~ wt + cyl + hp, data = scaled_mtcars)
model2  <- lm(mpg ~ wt + cyl + hp + disp, data = scaled_mtcars)

predicted <- predict.lm(model1, test_set)
actual  <- test_set$mpg

#Това дава грешката. Може да го направим за model2 и да ги сравним така
mean(abs(predicted - actual))

predicted <- predict.lm(model2, test_set)
actual  <- test_set$mpg
mean(abs(predicted - actual))

#Task 2
heights <- read.csv("./Height.txt", sep = "\t")

colnames(heights)

#Голяма корелация между променливите има
cor(heights$Height, heights$momheight)
cor(heights$Height, heights$dadheight)

heights_cm <- heights * 2.45

#Голяма корелация между променливите има
lm(Height ~ ., data = heights_cm) %>%
            summary()

model <- lm(Height ~ ., data = heights_cm)


#Това е идеята, ама имам неякакъв бъг
predict.lm(
           model,
           data.frame(
                momheight = c(160, 162, 166),
                dadheight = c(176, 180, 185)
           ),
           interval = "confidence",
           level = 0.98
)

#Линейна регресия е, защото е линейна функция на параметричното
#пространство

#Task 3
galileo <- data.frame(
    height = c(100, 200, 300, 450, 600, 800, 1000),
    distance = c(253, 337, 395, 451, 495, 534, 574)
)

#прилича на полином
plot(galileo)

#Затова добавяме ^2. Ако тестваш ще видиш, че иначе R
#намаля без height^2
model <- lm(distance ~ height + I(height ^ 2), data = galileo) %>%
    summary()


plot(galileo)
#type p e points, b e за точки и линия, l е за линия
lines(galileo, fitted(model), type = "b", col = "red")

#grammar of graphic - графиките са рагледани като граматики
#и от там идва ggplot и gganimate
