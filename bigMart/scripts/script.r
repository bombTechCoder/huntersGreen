
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

str(raw_data)