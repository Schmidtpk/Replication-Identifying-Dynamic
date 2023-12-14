library(ggplot2)
library(cnrmturk)
library(verification)
library(usethis)
library(rjags)
library(R2jags)


# BS items first for ordering.
d <- cnrmturk::pull_data(choice.cond = c(1,2,3,4),standardize = F)


# BS 1 == Correct != 1 is Wrong

# Paper for cutoffs
# https://www.tandfonline.com/doi/pdf/10.1207/S15324818AME1604_2?casa_token=Gm0Oh4cGiDQAAAAA:PVGzziDxdFSc7kzjDzA4K1FeTdP7Rv5oylc8agO2tu1aC1O9aG-4oMMdRdckOmmkh6CjthGxKMhiKw
# # # # # # # # # # # # # # # # # # # #


index <- c(rep(1,10),
           rep(2,10),
           rep(3,10),
           rep(4,10),
           rep(5,10),
           rep(6,10),
           rep(7,10),
           rep(8,10),
           rep(9,10),
           rep(10,10))

xx <- expand.grid(1:10,1:10)
index2 <- xx[ xx[,1] != xx[,2],]

# Omits marker variables
index3 <- seq(1,100,1)[-c(1,11,21,31,41,51,61,71,81,91)]

ymat <- d$ymat

cdX <- list()
for(i in 1:nrow(d$ymat)){
cdX[[i]] <-  as.numeric(d$cd[i,d$iord[i,]])
}
cd_sorted <- as.data.frame(do.call("rbind",cdX))


ggplot() +
  geom_density(aes(x = cd_sorted$V100)) +
  theme_minimal()

cut <- quantile(x = cd_sorted$V100,probs = c(0.20,
                                             0.40,
                                             0.60,
                                             0.80))
cd <- cd_sorted[,100]
for(i in length(cut)){

ymatx <- ymat[cd < cut[i], ]
cdx <- cd[cd < cut[i]]
ttx <- d$treatment.timing[cd < cut[i]]
condx <- d$cond[cd < cut[i]]

input <- list(y = ymatx,
              R0 = diag(10),
              N = dim(ymatx)[1],
              muy0 = 3.5,
              index = index,
              index2 = index2,
              index3 = index3)

params <- c("lySt",
            "ly",
            "Fcor",
            "sigmaxi",
            "sigmay",
            "ty1",
            "log_lik")

fit1 <- jags(data=input,
             parameters.to.save=params,
             n.iter=10000,
             n.chains=4,
             n.thin=2,
             n.burnin=5000,
             model.file="CFA_std2.R")

out <- list("fit" = fit1$BUGSoutput$summary,
            "full_fit" = fit1,
            "input" = input,
            "Treat" = ttx,
            "Cond" = condx,
            "cd" = cdx,
            "cd_cut" = cut[i])

dir <- "save/final_fits/"

name <- paste0(dir,"CFA_fit_Pfit_cutoff_",i,".RDS")
saveRDS(out,name)
}

dat <- pull_data(choice.id=cnrmturk::bs%>%filter(bs1==1,bs2==1)%>%pull(id), standardize = F)

ymatx <- dat$ymat
cdx <- dat$cd
ttx <- dat$treatment.timing
condx <- dat$cond

input <- list(y = ymatx,
              R0 = diag(10),
              N = dim(ymatx)[1],
              muy0 = 3.5,
              index = index,
              index2 = index2,
              index3 = index3)

params <- c("lySt",
            "ly",
            "Fcor",
            "sigmaxi",
            "sigmay",
            "ty1",
            "log_lik")


fit1 <- jags(data=input,
             parameters.to.save=params,
             n.iter=10000,
             n.chains=4,
             n.thin=2,
             n.burnin=5000,
             model.file="CFA_std2.R")

out <- list("fit" = fit1$BUGSoutput$summary,
            "full_fit" = fit1,
            "input" = input,
            "Treat" = ttx,
            "Cond" = condx,
            "cd" = cdx,
            "cd_cut" = "BS")

dir <- "save/final_fits/"

name <- paste0(dir,"CFA_BS_fit.RDS")
saveRDS(out,name)

# FULL CFA

dat <- cnrmturk::pull_data(choice.cond = c(1,2,3,4),standardize = F)

ymatx <- dat$ymat
cdx <- dat$cd
ttx <- dat$treatment.timing
condx <- dat$cond

input <- list(y = ymatx,
              R0 = diag(10),
              N = dim(ymatx)[1],
              muy0 = 3.5,
              index = index,
              index2 = index2,
              index3 = index3)

params <- c("lySt",
            "ly",
            "Fcor",
            "sigmaxi",
            "sigmay",
            "ty1",
            "log_lik")

fit1 <- jags(data=input,
             parameters.to.save=params,
             n.iter=10000,
             n.chains=4,
             n.thin=2,
             n.burnin=5000,
             model.file="CFA_std2.R")

ccut <- "100%"


out <- list(
  "fit" = fit1$BUGSoutput$summary,
  "log_lik" = fit1$BUGSoutput$sims.list$log_lik,
  "input" = input,
  "Treat" = ttx,
  "Cond" = condx,
  "cd" = cdx,
  "cd_cut" = ccut
)

dir <- "save/final_fits/"

name <- paste0(dir,"CFA_full_loglik.RDS")
saveRDS(out,name)


