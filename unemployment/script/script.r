#' Survival Analysis000
#' Data Unemployment.csv (3343 obs x 20 variables)
#' Objective: To model the time to finding a job
#' Y: Time, Event (Time to Event)
#' X: LogWage, Age, UnempIns

setwd("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\unemployment\\data")
d <- read.csv("Unemployment-1.csv")
str(d)
View(d)
attach(d)

# Descriptive statistics
length(unique(d$Time))                 # Count of unique values in the Time Column
unique(d$Time)                         # 20 unique times in the sample: 1-28 
summary(d$Time)
table(d$Event)
hist(d$LogWage)
hist(d$Age)
table(d$UnempIns)

# Kaplan-Meier non-parametric analysis
# Group data based on Time and estimate KM survival function based on Event
# install.packages("survival")
library(survival)

km1 <- survfit(Surv(d$Time, d$Event) ~ 1)      
summary(km1)
plot(km1, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
km2 <- survfit(Surv(d$Time, d$Event) ~ d$UnempIns)
summary(km2)
plot(km2, xlab="Time", ylab="Survival Probability")

# Nelson-Aalen non-parametric analysis
na <- survfit(coxph(Surv(d$Time, d$Event) ~ 1), type="aalen")
summary(na)
plot(na, xlab="Time", ylab="Survival Probability")

# Cox proportional hazard model - coefficients and hazard rates
cox <- coxph(Surv(d$Time, d$Event) ~ d$LogWage + d$Age + d$UnempIns, method="breslow")
summary(cox)

# Exponential, Weibull, and log-logistic parametric model coefficients
exp <- survreg(Surv(d$Time, d$Event) ~ d$LogWage + d$Age + d$UnempIns, dist="exponential")
summary(exp)

weibull <- survreg(Surv(d$Time, d$Event) ~ d$LogWage + d$Age + d$UnempIns, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(d$Time, d$Event) ~ d$LogWage + d$Age + d$UnempIns, dist="loglogistic")
summary(loglogistic)

library(stargazer)
stargazer(cox, exp, weibull, loglogistic, type="text")
