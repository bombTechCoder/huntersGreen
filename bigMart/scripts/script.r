
# import data 
library(readxl)
raw_data <- read_excel("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\bigMart\\data\\BigMartSales-2.xlsx")

# View(raw_data)
str(raw_data)

raw_data$item_fat_content <- factor(raw_data$item_fat_content)
raw_data$item_type <- factor(raw_data$item_fat_content)
raw_data$outlet_size <- factor(raw_data$outlet_size)
raw_data$city_type <- factor(raw_data$city_type)
raw_data$outlet_type <- factor(raw_data$outlet_type)

View(raw_data)
str(raw_data)
hist(raw_data$item_sales)

summary(raw_data)

cor(raw_data$item_sales, raw_data$item_visibility)
cor(raw_data$item_sales, raw_data$item_mrp)
# cor(raw_data$item_sales, raw_data$outlet_type)

library(corrplot)
corrplot(raw_data, method="circle")
library(ggplot2)
