# Data
library(readxl)
poc <- read_excel("R/STAT310/POC_All Regions.xlsx")
View(poc)

# Modify Variables
poc$ethnicity <- as.factor(poc$ethnicity)
poc$position <- as.factor(poc$position)
poc$region <- as.factor(poc$region)

# Subset Data into Two Datasets
library(tidyverse)
poc0 <- poc %>% group_by(region)
poc1 <- poc %>% select(1:8) #Group 1: 2013-2015
poc2 <- poc %>% select(1:3, 9:13) #Group 2: 2016-2019
View(poc1); View(poc2)

# Add Living Wage Variable 
# Add Binary Response Variable
# Add Race Variable

## Group 1: 2013-2015
poc1 <- poc1 %>% mutate(living_wage = case_when(
  is.na(averagecontract_salary...8) ~ "missing",
  averagecontract_salary...8 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...8 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...8 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no")); poc1$living_wage <- factor(poc1$living_wage)

poc1 <- poc1 %>% mutate(y1 = case_when(living_wage == 'yes' ~ 1,
                                      living_wage == 'no' ~ 0))

poc1 <- poc1 %>% mutate(race = case_when(
  ethnicity == 'White or European American' ~ 1, TRUE ~ 0,)) 
poc1$race <- factor(poc1$race)

poc1 <- poc1 %>% relocate(averagecontract_salary...8, .before = contract_2013_15)
poc1 <- poc1 %>% relocate(living_wage, .after = averagecontract_salary...8)
poc1 <- poc1 %>% relocate(y1, .after = living_wage)
poc1 <- poc1 %>% relocate(race, .after = ethnicity)
View(poc1)

## Group 2: 2016-2019
poc2 <- poc2 %>% mutate(living_wage = case_when(
  is.na(averagecontract_salary...13) ~ "missing",
  averagecontract_salary...13 >= 816 & region == 'central' ~ "yes",
  averagecontract_salary...13 >= 936 & region == 'western' ~ "yes",
  averagecontract_salary...13 >= 889 & region == 'eastern' ~ "yes",
  TRUE ~ "no")); poc2$living_wage <- factor(poc2$living_wage)

poc2 <- poc2 %>% mutate(y2 = case_when(living_wage == 'yes' ~ 1,
                                      living_wage == 'no' ~ 0))

poc2 <- poc2 %>% mutate(race = case_when(
  ethnicity == 'White or European American' ~ 1, TRUE ~ 0,))
poc2$race <- factor(poc2$race)

poc2 <- poc2 %>% relocate(averagecontract_salary...13, .before = contract_2016_19)
poc2 <- poc2 %>% relocate(living_wage, .after = averagecontract_salary...13)
poc2 <- poc2 %>% relocate(y2, .after = living_wage)
poc2 <- poc2 %>% relocate(race, .after = ethnicity)
View(poc2)

################################################
# Models
################################################

##General information
library(tree)
library(rpart)
library(rattle)
library(RColorBrewer)
library(rpart.plot)
##Group1: 2013-2015
tree.tit1 <- rpart(poc1$y1~.-poc1$y1, data=poc1, method="class")
fancyRpartPlot(tree.tit1)
##Group2: 2016-2019
tree.tit2 <- rpart(poc2$y2~.-poc2$y2, data=poc2, method="class")
fancyRpartPlot(tree.tit2)

library(MASS)
# Model Selection Group 1
model1 <- glm(poc1$y1 ~ poc1$contract_2013_15+poc1$position+poc1$region+poc1$ethnicity+poc1$averagecontract_salary...8
              +poc1$race+poc1$average_overscale...7+poc1$averagemin_salary...6)
summary(model1)
stepAIC(model1)
logmodel1 <- glm(poc1$y1~poc1$region+poc1$averagemin_salary...6)
summary(logmodel1)
# Model Selection Group 2
model2 <- glm(poc2$y2 ~ poc2$contract_2016_19+poc2$position+poc2$region+poc2$ethnicity+poc2$averagecontract_salary...13
              +poc2$race+poc2$average_overscale...12+poc2$averagemin_salary...11)
summary(model2)
stepAIC(model2)
logmodel2 <- glm(poc2$y2 ~ poc2$contract_2016_19+poc2$position+poc2$region
                 +poc2$averagecontract_salary...13)
summary(logmodel2)

# ggplot: analysis average contract salary by different race
library(ggplot2)
## Group1: 2013-2015
ggplot(aes(x=poc1$averagecontract_salary...8, y=poc1$region, colour=poc1$ethnicity), 
       data=poc1)+geom_point()
## Group2: 2016-2019
ggplot(aes(x=poc2$averagecontract_salary...13, y=poc2$region, colour=poc2$ethnicity), 
       data=poc2)+geom_point()

##GEE Test 
library(gee)
##Group2: 2013-2015
model3 <- gee(poc1$y1~poc1$race, id = poc1$race, 
                           family = binomial(link = "identity"), data = poc1)
model4 <- gee(poc2$y2~poc2$race, id = poc2$race, 
                           family = binomial(link = "identity"), data = poc2)

##Group2: 2016-2019
model4 <- gee(poc2$y2~poc2$race, id = poc2$race, 
              family = binomial(link = "identity"), data = poc2)
summary(model4)
ggplot(aes(x=poc2$averagemin_salary...11, y=poc2$region, colour=poc2$race), 
       data=poc2)+geom_point()

##Compare living wage by race
##Group1: 2013-2015
living_wage1=ifelse(poc1$y1 == 0, "No", "Yes")
white1=ifelse(poc1$race == 0, "No","Yes")
poc11 <- table(living_wage1, white1)
poc11

##Group2: 2016-2019
living_wage2=ifelse(poc2$y2 == 0, "No", "Yes")
white2=ifelse(poc2$race == 0, "No","Yes")
poc22 <- table(living_wage2, white2)
poc22

## For Question 2 Only
# Model 1: Group 1 2013-2015
poc$ethnicity <- relevel(poc$ethnicity, "White or European American")
poc$position <- relevel(poc$position, "chorus")
poc$region <- relevel(poc$region, "central")

library(car)
model1 <- glm(y1 ~ position + region + ethnicity + contract_2013_15, 
              family = binomial(link = "logit"), data = poc1)

model11 <- glm(y1 ~ position + region + race + contract_2013_15, 
               family = binomial(link = "logit"), data = poc1)

# Model 2: Group 2 2016-2019
model2 <- glm(y2 ~ position + region + ethnicity + contract_2016_19, 
              family = binomial(link = "logit"), data = poc2) 

model22 <- glm(y2 ~ position + region + race + contract_2016_19, 
               family = binomial(link = "logit"), data = poc2)

Anova(model1)
Anova(model2)
summary(model11)
summary(model22)
Anova(model11)
Anova(model22)

################################################
# Appendix Only
################################################

# Living Wage
library(dplyr)
poc1 <- na.omit(poc1)
poc2 <- na.omit(poc2)

p1 <- poc1 %>% 
  group_by(living_wage) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(p1, aes(x = "", y = perc, fill = living_wage)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(
    title = "Group 1 (2013-15): Meets Living Wage Thresholds?")) +
  coord_polar(theta = "y") + 
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_void()

p2 <- poc2 %>% 
  group_by(living_wage) %>%
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(p2, aes(x = "", y = perc, fill = living_wage)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(
    title = "Group 2 (2016-19): Meets Living Wage Thresholds?")) +
  coord_polar(theta = "y") + 
  scale_fill_discrete(labels = c("No", "Yes")) +
  theme_void()

# Ethnicity & Living Wage
par(mfrow = c(1,2))
e1$poc1.living_wage <- as.numeric(poc1$living_wage)
e1 <- table(poc1$living_wage,poc1$race)
barplot(e1, xlab = 'White and BIPOC', ylab = 'Count', main 
        = "2013-15 Living Wage on Region Threshold",
        col = c("red","lightgreen"),
        density = 50,
        legend = rownames(e1), 
        args.legend = list(x = "topleft"))

e2$poc2.living_wage <- as.numeric(poc2$living_wage)
e2 <- table(poc2$living_wage,poc2$race)
barplot(e2, xlab = 'White and BIPOC', ylab = 'Count', main 
        = "2016-19 Living Wage on Region Threshold",
        col = c("red","lightgreen"),
        density = 50,
        legend = rownames(e1), 
        args.legend = list(x = "topleft"))

e.1 <- data.frame(poc1$ethnicity, poc1$living_wage)
e.1 <- table(e.1)
addmargins(e.1)

e.2 <- data.frame(poc2$ethnicity, poc2$living_wage)
e.2 <- table(e.2)
addmargins(e.2)
