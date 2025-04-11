# import data 
library(readxl)
library(dplyr)
library(MASS)
library(jtools)
library(ggplot2)
library(stargazer)
library(lme4)

# read file
d <- read_excel("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\bigMart\\data\\BigMartSales-2.xlsx")

str(d)

# clean data
d$item_type <- factor(d$item_fat_content)
d$outlet_size <- factor(d$outlet_size)
d$city_type <- factor(d$city_type)
d$outlet_type <- factor(d$outlet_type)
d$outlet_age <- 2025 - d$outlet_year
d$item_fat_content <- recode_factor(d$item_fat_content, "low fat"="Low Fat")

# review data
summary(d)
View(d)
str(d)
hist(d$item_sales)
hist(log(d$item_sales))

boxplot(item_sales ~ interaction(outlet_type, city_type), data = d,
        main = "Boxplot of Sales by Outlet Type and City Type",
        xlab = "Outlet Type and City Type",
        ylab = "Item Sales",
        las = 2)

cor(d$item_sales, d$item_visibility)
cor(d$item_sales, d$item_mrp)


# If the questions are completely separate from each other, then the model ought
# to have only one independant variable.  Let's run through that quick

# only the outlet

otm1_ols <- lm(log(item_sales) ~ outlet_type, data=d)
summary(otm1_ols)
plot(otm1_ols)

otm2_glm <- glm(log(item_sales) ~ outlet_type, data=d)
summary(otm2_glm)
plot(otm2_glm)

otm3_plm <- lmer(log(item_sales) ~ outlet_type + (1 | outlet_ID), data=d)
summary(otm3_plm)
plot(otm3_plm)

stargazer(otm1_ols, otm2_glm, otm3_plm, type="text", single.row = TRUE)

# all 3 models show Supermarket Type 3 returning the highest differential

# only the city

ctm1_ols <- lm(log(item_sales) ~ city_type, data=d)
summary(ctm1_ols)
plot(ctm1_ols)

ctm2_glm <- glm(log(item_sales) ~ city_type, data=d)
summary(ctm2_glm)
plot(ctm2_glm)

ctm3_plm <- lmer(log(item_sales) ~ city_type + (1 | outlet_ID), data=d)
summary(ctm3_plm)
plot(ctm3_plm)

stargazer(ctm1_ols, ctm2_glm, ctm3_plm, type="text", single.row = TRUE)

# all 3 models show city type tier 3 to produce the highest differential

# only the store

oim1_ols <- lm(log(item_sales) ~ outlet_ID, data=d)
summary(oim1_ols)
plot(oim1_ols)

oim2_glm <- glm(log(item_sales) ~ outlet_ID, data=d)
summary(oim2_glm)
plot(oim2_glm)

oim3_plm <- lmer(log(item_sales) ~ outlet_ID + (1 | item_ID), data=d)
summary(oim3_plm)
plot(oim3_plm)

stargazer(oim1_ols, oim2_glm, oim3_plm, type="text", single.row = TRUE)

# Best performers
# #1 OUT027
# #2 OUT035
# #3 2 votes for OUT049 1 vote for OUT017

# Worst performers
# #1 OUT019
# #2 OUT018
# #3 OUT045

# Let's put it together

cm1 <- lmer(log(item_sales) ~ city_type + outlet_type + item_mrp + item_visibility + (1 | outlet_ID), data=d)
summary(cm1_ols)
plot(cm1_ols)

cm2 <- lmer(log(item_sales) ~ city_type + outlet_type + item_mrp + outlet_age + (1 | outlet_ID), data=d)
summary(cm2_glm)
plot(cm2_glm)

cm3 <- lmer(log(item_sales) ~  city_type + outlet_type + outlet_age + item_visibility + (1 | outlet_ID), data=d)
summary(cm3_plm)
plot(cm3_plm)

stargazer(cm1_ols, cm2_glm, cm3_plm, type="text", single.row = TRUE)
