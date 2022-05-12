#elisa is a .xlsx file storing the vectors a.od (HEV-A p239 OD value) and c.od (HEV-C p241 OD value).

elisa$r <- with(elisa, a.od/c.od) #calculates ratio

anatest <- elisa$a.od
cnctest <- elisa$c.od
actest <- elisa$r

#cutpoint.ana is the optimal cut-off in the A/¬A test as determined by cutpointr(), cutpoint.cnc that in the C/¬C test, and cutpoint.ac that in the A/C test.

firstA <- cut(anatest, c(-Inf, cutpoint.ana, Inf), right = FALSE, labels=c("A-", "A+"))
firstC <- cut(cnctest, c(-Inf, cutpoint.cnc, Inf), right = FALSE, labels=c("C-", "C+"))
second <- cut(actest, c(-Inf, cutpoint.ac, Inf), right = FALSE, labels=c("-", "+"))
algorithm <- data.frame(firstA, firstC, second)

#Create elisa$pred to store predictions.

algorithm$pred <- "D"
algorithm$pred[(algorithm$firstA=="A+" & algorithm$firstC=="C-") | (algorithm$firstA=="A+" & algorithm$firstC=="C+" & algorithm$second=="+")] <- "A"
algorithm$pred[(algorithm$firstA=="A-" & algorithm$firstC=="C+") | (algorithm$firstA=="A+" & algorithm$firstC=="C+" & algorithm$second=="-")] <- "C"
elisa$pred <- algorithm$pred