# Descriptives tables for data collection


dat <- data.frame("cond"=cnrmturk::cond,cnrmturk::ymat)

tab1 <- round(t(aggregate(dat[,-1],list(dat$cond),mean)),2)[-1,]

tab2 <- round(t(aggregate(dat[,-1],list(dat$cond),sd)),2)[-1,]

colnames(tab1) <-  c("mean_c1","mean_c2","mean_c3","mean_c4")

colnames(tab2) <-  c("sd_c1","sd_c2","sd_c3","sd_c4")




dat <- data.frame(sprintf("%.2f",tab1),sprintf("%.2f",tab2))

tab3 <- paste0(dat[,1]," (",dat[,2],")")


tab4 <- data.frame("cond1" = tab3[1:100],
                   "cond2" = tab3[101:200],
                   "cond3" = tab3[201:300],
                   "cond4" = tab3[301:400])

row.names(tab4) <- row.names(tab1)

library(kableExtra)


kable(tab4,format = "latex",booktabs = TRUE)
