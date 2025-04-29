# 1. Read & clean  (re-run from scratch in a fresh session if needed)
d <- read.table("C:\\Users\\cbloom\\Documents\\dev\\huntersGreen\\huntersGreen\\lungCancer\\data\\LungCancer-2.txt",
                header = FALSE, comment.char = "#",
                col.names = c("treatment","celltype","survival","status",
                              "karnofsky","monthsfromdiag","age","priorchemo"))

# (optional) make everything lower-case
names(d) <- tolower(names(d))

# Ensure numeric
d$survival <- as.numeric(d$survival)
d$status   <- as.numeric(d$status)

# Recode treatment 1/2 → factor
d$treatment <- factor(d$treatment, levels = c(1,2),
                      labels = c("standard","test"))

# 2. Arm indicator
arm <- ifelse(d$treatment == "test", 1, 0)

# 3. Choose a sensible tau: largest observed **event** time in either arm
tau <- min( max(d$survival[arm == 0 & d$status == 1]),
            max(d$survival[arm == 1 & d$status == 1]) )

# 4. Run rmst2()
library(survRM2)
rmst_cmp <- rmst2(time   = d$survival,
                  status = d$status,
                  arm    = arm,
                  tau    = tau)

print(rmst_cmp)
plot(rmst_cmp)
