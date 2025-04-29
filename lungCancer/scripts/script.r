rm(list=ls())
library(survival)
library(survminer)

# read data
d <- read.table("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\lungCancer\\data\\LungCancer-2.txt", header = FALSE)
colnames(d) <- c("treatment", "cellType", "survival", "status", 
                    "karnofsky", "monthsfromdiag", "age", "priorchemo")

View(d)
attach(d)
hist(survival)
hist(log(survival))


# factor treatment variable
d$treatment <- factor(d$treatment, levels = c(1, 2),
                      labels = c("standard", "test"))

View(d)

surv_obj <- Surv(time = d$survival, event = d$status)

km_fit <- survfit(surv_obj ~ d$treatment, data = d)

# Kaplan-Meier curves
ggsurvplot(km_fit, data = d, 
           pval = TRUE,   
           conf.int = TRUE, 
           risk.table = TRUE, 
           xlab = "Time (days)",
           ylab = "Survival probability", 
           title = "Kaplan-Meier Standard vs. Test Treatment")


survival_summary <- summary(km_fit, times = c(183, 365))
print(survival_summary)

# View(d)

#### This block I couldn't get to distinguish between treatments
# standard treatment group
# km_standard <- survfit(surv_obj ~ 1, data = subset(d, treatment == "standard"))
# print(km_standard)
# plot(km_standard)
# summary_standard <- summary(km_standard)
# print(summary_standard)
# print(summary_standard$table)
# mean_survival_standard <- summary_standard$table["rmean"]
# print(mean_survival_standard)

# test treatment group:
# km_test <- survfit(surv_obj ~ 1, data = subset(d, treatment == "test"))
# print(km_test)
# plot(km_test)
# summary_test <- summary(km_test)
# print(summary_test)
# print(summary_test$table)
# mean_survival_test <- summary_test$table["rmean"]
# print(mean_survival_test)


#### So I tried this approach
library(survRM2)

# needs a boolean input again now
arm <- ifelse(treatment == "test", 1, 0)

# maximum amount of time I care about
tau <- max(survival)

# run rmst2 on the full dataset
rmst_cmp <- rmst2(
  time   = survival,
  status = status,
  arm    = arm,
  tau    = tau
)

print(rmst_cmp)
plot(rmst_cmp)



cox_model <- coxph(surv_obj ~ age + monthsfromdiag + treatment, data = d)
summary(cox_model)

hr <- exp(coef(cox_model))
print("Hazard Ratios (Cox Model):")
print(hr)

weibull_model <- survreg(surv_obj ~ age + monthsfromdiag + treatment, data = d, dist = "weibull")
summary(weibull_model)

time_ratio_weibull <- exp(-coef(weibull_model))
print("Time Ratios (Weibull Model):")
print(time_ratio_weibull)

lognormal_model <- survreg(surv_obj ~ age + monthsfromdiag + treatment, data = d, dist = "lognormal")
summary(lognormal_model)

time_ratio_lognormal <- exp(-coef(lognormal_model))
print("Time Ratios (Log-Normal Model):")
print(time_ratio_lognormal)
