library(survival)
library(survminer)

# read data
d <- read.table("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\lungCancer\\data\\LungCancer-2.txt", header = FALSE)
colnames(d) <- c("treatment", "cellType", "survival", "status", 
                    "karnofsky", "monthsfromdiag", "age", "priorchemo")

View(d)
attach(d)
hist(Survival)
hist(log(Survival))


# factor treatment variable
d$treatment <- factor(data$treatment, levels = c(1, 2), labels = c("standard", "test"))

surv_obj <- Surv(time = d$survival, event = d$status)

km_fit <- survfit(surv_obj ~ d$treatment, data = d)

# Kaplan-Meier curves
ggsurvplot(km_fit, data = d, 
           pval = TRUE,   
           conf.int = TRUE, s
           risk.table = TRUE, 
           xlab = "Time (days)",
           ylab = "Survival probability", 
           title = "Kaplan-Meier Survival Curves: Standard vs. Test Treatment")

survival_summary <- summary(km_fit, times = c(183, 365))
print(survival_summary)

# standard treatment group
km_standard <- survfit(surv_obj ~ 1, data = subset(d, treatment == "standard"))
summary_standard <- summary(km_standard)
print(summary_standard)
print(summary_standard$table)
mean_survival_standard <- summary_standard$table["rmean"]
print(mean_survival_standard)

# test treatment group:
km_test <- survfit(surv_obj ~ 1, data = subset(d, treatment == "test"))
summary_test <- summary(km_test)
print(summary_test)
print(summary_test$table)
mean_survival_test <- summary_test$table["rmean"]
print(mean_survival_test)
