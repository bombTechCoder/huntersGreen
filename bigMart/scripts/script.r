# import data 
library(readxl)
library(dplyr)
library(plm)
d <- read_excel("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\bigMart\\data\\BigMartSales-2.xlsx")

# View(d)
str(d)

# d$item_fat_content <- factor(d$item_fat_content)
d$item_type <- factor(d$item_fat_content)
d$outlet_size <- factor(d$outlet_size)
d$city_type <- factor(d$city_type)
d$outlet_type <- factor(d$outlet_type)
d$outlet_age <- 2025 - d$outlet_year

d$item_fat_content <- recode_factor(d$item_fat_content, "low fat"="Low Fat")

View(d)
str(d)
hist(log(d$item_sales))

summary(d)

cor(d$item_sales, d$item_visibility)
cor(d$item_sales, d$item_mrp)

otm1_ols <- lm(log(item_sales) ~ outlet_type, data=d)
summary(otm1_ols)
plot(otm1_ols)

otm2_glm <- glm(log(item_sales) ~ outlet_type, data=d)
summary(otm2_glm)
plot(otm2_glm)

otm3_plm <- plm(log(item_sales) ~ outlet_type, data=d, model="random")
summary(otm3_plm)
