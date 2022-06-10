eia <- read_excel("~/Downloads/eia.xlsx", col_types = c("text", "numeric", "numeric"))
View(eia)

a.trim <- eia$a.od[eia$panel=="A"] > max(eia$a.od[eia$panel=="D"])
c.trim <- eia$c.od[eia$panel=="C"] > max(eia$c.od[eia$panel=="D"])
trim <- c(a.trim, c.trim)
eia.trim <- eia[-c(which(trim %in% FALSE), which(trim %in% NA)),]
eia.trim$r <- with(eia.trim, a.od/c.od)

na <- sum(eia.trim$panel=="A")
nc <- sum(eia.trim$panel=="C")
nd <- sum(eia.trim$panel=="D")
a1c0 <- c(rep(1,na), rep(0,nc))
c0d0a1 <- c(rep(1,na), rep(0,nc+nd))
a0d0c1 <- c(rep(0,na), rep(1,nc), rep(0,nd))

#Cut-off optimisation
library(cutpointr)
cp.aod.aa <- cutpointr(eia.trim$a.od, c0d0a1, pos_class=1, method = maximize_metric, metric = sum_sens_spec)
aod.aa <- as.numeric(with(cp.aod.aa, cbind(sensitivity, specificity, AUC, optimal_cutpoint)))

cp.cod.cc <- cutpointr(eia.trim$c.od, a0d0c1, pos_class=1, method = maximize_metric, metric = sum_sens_spec)
cod.cc <- as.numeric(with(cp.cod.cc, cbind(sensitivity, specificity, AUC, optimal_cutpoint)))

cp.r.ac <- cutpointr(eia.trim$r[eia.trim$panel=="A" | eia.trim$panel=="C"], a1c0, pos_class=1, method = maximize_metric, metric = sum_sens_spec)
r.ac <- as.numeric(with(cp.r.ac, cbind(sensitivity, specificity, AUC, optimal_cutpoint)))

label <- c("Sensitivity", "Specificity", "AUC", "Optimal cutpoint")
df <- data.frame(label, aod.aa, cod.cc, r.ac)
print(df)

#Algorithm
aatest <- eia.trim$a.od #choose the index for the A/¬A test
cctest <- eia.trim$c.od #choose the index for the C/¬C test
actest <- eia.trim$r #choose the index for the A/C test

firstA <- cut(aatest,
              c(-Inf, cp.aod.aa$optimal_cutpoint, Inf), right = FALSE,
              labels=c("A-", "A+"))
firstC <- cut(cctest,
              c(-Inf, cp.cod.cc$optimal_cutpoint, Inf), right = FALSE,
              labels=c("C-", "C+"))
secondCA <- cut(actest,
                c(-Inf, cp.r.ac$optimal_cutpoint, Inf), right = FALSE,
                labels=c("-", "+"))
algorithm <- data.frame(firstA, firstC, secondCA)
algorithm$pred <- "D"
algorithm$pred[(algorithm$firstA=="A+" &
                  algorithm$firstC=="C-") |
                 (algorithm$firstA=="A+" &
                    algorithm$firstC=="C+" &
                    algorithm$secondCA=="+")] <- "A"
algorithm$pred[(algorithm$firstA=="A-" &
                  algorithm$firstC=="C+") |
                 (algorithm$firstA=="A+" &
                    algorithm$firstC=="C+" &
                    algorithm$secondCA=="-")] <- "C"

conf <- table(eia.trim$panel[1:(na+nc+nd)], algorithm$pred)
conf

