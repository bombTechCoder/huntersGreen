library(readxl)  
library(dplyr)  
library(lme4)  
library(ggplot2)  
library(stargazer)  

### Data Preparation ###

# Read the Excel sheets  
stores <- read_excel("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\statsFinal\\data\\SnackChain-2.xlsx", sheet = "stores")  
products <- read_excel("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\statsFinal\\data\\SnackChain-2.xlsx", sheet = "products")  
transactions <- read_excel("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\statsFinal\\data\\SnackChain-2.xlsx", sheet = "transactions")  

# Clean and prepare data  
clean_data <- transactions %>%  
  left_join(products, by = "UPC") %>%  
  left_join(stores, by = c("STORE_NUM" = "STORE_ID")) %>%  
  filter(CATEGORY != "ORAL HYGIENE PRODUCTS") %>%  
  mutate(  
    logUnits = log(UNITS),  
    logPrice = log(PRICE),  
    Promotion = ifelse(FEATURE > 0 | DISPLAY > 0, 1, 0)  
  ) %>%  
  filter(UNITS > 0, PRICE > 0)  

### Check Distributions and Visualizations ###

# Histogram for UNITS  
p1 <- ggplot(clean_data, aes(x = UNITS)) +  
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +  
  labs(title = "Distribution of UNITS", x = "UNITS", y = "Count") +  
  theme_minimal()  
print(p1)  

# Histogram for PRICE  
p2 <- ggplot(clean_data, aes(x = PRICE)) +  
  geom_histogram(bins = 50, fill = "lightgreen", color = "black") +  
  labs(title = "Distribution of PRICE", x = "PRICE", y = "Count") +  
  theme_minimal()  
print(p2)  

# Histogram for logUnits  
p3 <- ggplot(clean_data, aes(x = logUnits)) +  
  geom_histogram(bins = 50, fill = "lightpink", color = "black") +  
  labs(title = "Distribution of log(UNITS)", x = "log(UNITS)", y = "Count") +  
  theme_minimal()  
print(p3)  

# Histogram for logPrice  
p2b <- ggplot(clean_data, aes(x = logPrice)) +  
  geom_histogram(bins = 50, fill = "gold", color = "black") +  
  labs(title = "Distribution of log(PRICE)", x = "log(PRICE)", y = "Count") +  
  theme_minimal()  
print(p2b)

# Box plot of Units by Promotion  
p4 <- ggplot(clean_data, aes(x = factor(Promotion), y = UNITS)) +  
  geom_boxplot(fill = "lightblue") +  
  scale_y_log10() +  
  labs(title = "Distribution of UNITS by Promotion Status",  
       x = "Promotion (0 = No, 1 = Yes)",  
       y = "UNITS (log scale)") +  
  theme_minimal()  
print(p4)  

### Product-Level Elasticity ###

product_counts <- clean_data %>%
  group_by(UPC) %>%
  summarise(
    n = n(),
    mean_price = mean(PRICE, na.rm = TRUE),
    description = first(DESCRIPTION),
    .groups = "drop"
  ) %>%
  filter(n >= 30)

product_elasticity <- clean_data %>%
  filter(UNITS > 0, PRICE > 0) %>%
  group_by(UPC) %>%
  filter(n() >= 5) %>%
  group_modify(~ {
    model <- lm(log(UNITS) ~ log(PRICE), data = .x)
    tibble(elasticity = coef(model)["log(PRICE)"])
  }) %>%
  left_join(product_counts, by = "UPC") %>%
  ungroup()

most_elastic <- product_elasticity %>% arrange(elasticity) %>% slice_head(n = 5)
print("5 Most Elastic Products:")
print(most_elastic)

least_elastic <- product_elasticity %>% arrange(desc(elasticity)) %>% slice_head(n = 5)
print("5 Least Elastic Products:")
print(least_elastic)

print(ggplot(product_elasticity, aes(x = elasticity)) +
        geom_histogram(bins = 30, fill = "steelblue", color = "black") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Distribution of Price Elasticities", x = "Elasticity", y = "Frequency") +
        theme_minimal())

# Simulate 10% price reduction on units and revenue
scenario_df <- product_elasticity %>%
  filter(!is.na(elasticity)) %>%
  left_join(clean_data %>%
              group_by(UPC) %>%
              summarise(
                total_units = sum(UNITS, na.rm = TRUE),
                avg_price = mean(PRICE, na.rm = TRUE),
                .groups = "drop"
              ),
            by = "UPC") %>%
  mutate(
    Expected_Unit_Change = -0.10 * elasticity,
    Expected_Units_Gained = total_units * Expected_Unit_Change,
    Expected_Revenue_Gain = Expected_Units_Gained * avg_price
  )

# Top 5 Products to Maximize Revenue
top_revenue <- scenario_df %>%
  arrange(desc(Expected_Revenue_Gain)) %>%
  slice_head(n = 5)
print("Top 5 Products to Maximize Revenue with 10% Price Cut:")
print(top_revenue)

# Top 5 Products to Maximize Unit Volume
top_units <- scenario_df %>%
  arrange(desc(Expected_Units_Gained)) %>%
  slice_head(n = 5)
print("Top 5 Products to Maximize Unit Volume with 10% Price Cut:")
print(top_units)

### Models ###
  
# Model 1: Simple
model1 <- lm(logUnits ~ logPrice, data = clean_data)  
print(summary(model1))

# Model 2: Fixed Effects  
model2 <- lm(logUnits ~ logPrice + FEATURE + DISPLAY + CATEGORY + SEGMENT,   
             data = clean_data)  
print(summary(model2))

# Model 3: Mixed Effects  
model3 <- lmer(logUnits ~ logPrice + FEATURE + DISPLAY + (1|STORE_NUM),   
               data = clean_data)  
print(summary(model3))

### Stargazer ###

stargazer(model1, model2, model3,  
          title = "Regression Results",  
          type = "text",
          dep.var.labels = "log(Units)",  
          covariate.labels = c("log(Price)", "Feature", "Display", "Category", "Segment"),  
          model.names = FALSE,  
          column.labels = c("Model 1", "Model 2", "Mixed Effects"),  
          notes = "Note: Mixed Effects model includes store-level random effects",  
          digits = 3)  

### Additional Visualizations ###

# Actual vs. Predicted Plot for Model 2  
clean_data$predicted_units <- exp(predict(model2))  
p5 <- ggplot(clean_data, aes(x = UNITS, y = predicted_units)) +  
  geom_point(alpha = 0.1) +  
  geom_abline(intercept = 0, slope = 1, color = "red") +  
  scale_x_log10() +  
  scale_y_log10() +  
  labs(title = "Model 2: Actual vs Predicted Units",  
       x = "Actual Units",  
       y = "Predicted Units") +  
  theme_minimal()  
print(p5)  

# Price Elasticity by Category  
model_by_category <- clean_data %>%  
  group_by(CATEGORY) %>%  
  do(model = lm(logUnits ~ logPrice, data = .))  

elasticities <- model_by_category %>%  
  summarise(  
    category = first(CATEGORY),  
    elasticity = coef(model)[["logPrice"]]  
  )  

p6 <- ggplot(elasticities, aes(x = reorder(category, elasticity), y = elasticity)) +  
  geom_col(fill = "blue") +  
  coord_flip() +  
  labs(title = "Price Elasticity by Category",  
       x = "Category",  
       y = "Price Elasticity") +  
  theme_minimal()  
print(p6)  

# Model comparison using AIC  
aic1 <- AIC(model1)  
aic2 <- AIC(model2)  
aic3 <- AIC(model3)  

cat("Model AICs:\n")
cat("Model 1 (Log-Log):", round(aic1, 2), "\n")
cat("Model 2 (Fixed Effects):", round(aic2, 2), "\n")
cat("Model 3 (Mixed Effects):", round(aic3, 2), "\n")

### Residual plots for Model 2 ###
clean_data$residuals <- residuals(model2)  
clean_data$fitted <- fitted(model2)  

p7 <- ggplot(clean_data, aes(x = fitted, y = residuals)) +  
  geom_point(alpha = 0.1) +  
  geom_hline(yintercept = 0, color = "red") +  
  labs(title = "Residual Plot for Model 2",  
       x = "Fitted Values",  
       y = "Residuals") +  
  theme_minimal()  
print(p7)  

### QQ Plot for residuals ### 
p8 <- ggplot(clean_data, aes(sample = residuals)) +  
  stat_qq() +  
  stat_qq_line() +  
  labs(title = "Normal Q-Q Plot of Residuals",  
       x = "Theoretical Quantiles",  
       y = "Sample Quantiles") +  
  theme_minimal()  
print(p8)  
