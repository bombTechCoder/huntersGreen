# Install and load required packages if not already installed.
# install.packages("survival")
# install.packages("survminer")
library(survival)
library(survminer)

# Read the data
# Note: The file "LungCancer-2.txt" has comment lines beginning with "#" which read.table will ignore.
# Since the data file does not include a header row, we assign column names manually.
d <- read.table("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\lungCancer\\data\\LungCancer-2.txt", header = FALSE)
colnames(d) <- c("treatment", "cellType", "survival", "status", 
                    "karnofsky", "monthsfromdiag", "age", "priorchemo")

View(d)
attach(d)
hist(Survival)
hist(log(Survival))


# Recode the Treatment variable into a factor
# According to the header: 1 = standard, 2 = test
d$treatment <- factor(data$treatment, levels = c(1, 2),
                         labels = c("standard", "test"))

# Create the Survival object.
# 'Survival' is the follow-up time (in days), and 'Status' indicates the event (1 = death, 0 = censored).
surv_obj <- Surv(time = d$survival, event = d$status)

# Fit the Kaplan-Meier survival curves by Treatment group.
km_fit <- survfit(surv_obj ~ treatment, data = d)

# Plot the Kaplan-Meier curves using ggsurvplot for a polished graph.
ggsurvplot(km_fit, data = d, 
           pval = TRUE,                # show p-value from log-rank test
           conf.int = TRUE,            # plot confidence intervals
           risk.table = TRUE,          # display the risk table below the graph
           xlab = "Time (days)",       # x-axis label
           ylab = "Survival probability",  # y-axis label
           title = "Kaplan-Meier Survival Curves: Standard vs. Test Treatment")

# Extract survival probabilities at 183 and 365 days.
# The summary function applied to the survfit object provides survival estimates at specified times.
summary <- summary(km_fit, times = c(183, 365))
print(summary)
# The printed output will provide, for each treatment group, the survival probability (and standard error/confidence interval)
# at 183 days and 365 days.

# Calculate the mean (restricted) survival times for Standard and Test treatments.
# Note: With censoring, the overall (unrestricted) mean survival may not be estimable; 
# here we use the estimated area under the survival curve (restricted mean survival time).

# For the Standard treatment group:
km_standard <- survfit(surv_obj ~ 1, data = subset(d, treatment == "standard"))
summary_standard <- summary(km_standard)
mean_survival_standard <- summary_standard$table["*rmean"]  # rmean is the estimated restricted mean survival time
cat("Mean survival (Standard treatment):", mean_survival_standard, "days\n")

# For the Test treatment group:
km_test <- survfit(surv_obj ~ 1, data = subset(d, treatment == "test"))
summary_test <- summary(km_test)
mean_survival_test <- summary_test$table["*rmean"]
cat("Mean survival (Test treatment):", mean_survival_test, "days\n")
