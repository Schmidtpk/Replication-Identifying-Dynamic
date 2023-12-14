#
# runcfa <- function(r,datr){
#
#   ydatr <- datr[,1:(p*q)]
#   odatr <- datr[,1:(p*q)+(p*q)]
#   vydatr <- datr[,1:(p*q)+(p*q*2)]
#
#   # parameters to safe
#   params <- c("lySt","psiy_i","psiy",
#               "Fcor",
#               "sigmaxi",
#               "b0","b_return",
#               "p.att","p.inatt","p.diff",
#               "C",
#               "p0",
#               "log_lik",
#               "Cratio","Call")
#
#   ### Generate indexes for model
#   index <- c(rep(1,10),
#              rep(2,10),
#              rep(3,10),
#              rep(4,10),
#              rep(5,10),
#              rep(6,10))
#
#   xx <- expand.grid(1:6,1:6)
#   index2 <- xx[ xx[,1] != xx[,2],]
#     # Omits marker variables
#   index3 <- seq(1,60,1)[-c(1,11,21,31,41,51)]
#
#   # list of data
#   data.now <- list(N=N,y=ydatr,R0=diag(q),ord=odatr, #?? cd function
#                    nfactor=6,
#                    index = index,
#                    index2 = index2,
#                    index3 = index3,
#                    muy0 = mean(unlist(ydatr)),
#                    p.return = 0.01)
#
#
#   # run mcmc
#   fit1 <- jags.parallel(data=data.now,
#                         parameters.to.save=params,
#                         n.iter=10000, n.chains=2,n.thin=1,n.burnin=5000,
#                         model.file="funs/jags_cfa.R")
#
#   est <- fit1$BUGSoutput$summary
#   save(est,datr,file = paste0("resultssim/",cond,"_r",r,"_cfa.rdata"))
# }
#
#
# runcfa4 <- function(r,datr){
#
#   ydatr <- datr[,1:(p*q)]
#   odatr <- datr[,1:(p*q)+(p*q)]
#   vydatr <- datr[,1:(p*q)+(p*q*2)]
#
#   # parameters to safe
#   params <- c("lySt","psiy_i","psiy",
#               "Fcor",
#               "sigmaxi",
#               "b0","b_return",
#               "p.att","p.inatt","p.diff",
#               "C",
#               "p0",
#               "log_lik",
#               "Cratio","Call")
#
#   ### Generate indexes for model
#   index <- c(rep(1,10),
#              rep(2,10),
#              rep(3,10),
#              rep(4,10),
#              rep(5,10),
#              rep(6,10))
#
#   xx <- expand.grid(1:6,1:6)
#   index2 <- xx[ xx[,1] != xx[,2],]
#   # Omits marker variables
#   index3 <- seq(1,60,1)[-c(1,11,21,31,41,51)]
#
#   # list of data
#   data.now <- list(N=N,y=ydatr,R0=diag(q),ord=odatr, maxV=((max(ydatr)-min(ydatr))/2)^2,
#                    nfactor=6,
#                    index = index,
#                    index2 = index2,
#                    index3 = index3,
#                    muy0 = mean(unlist(ydatr)),
#                    p.return = 0.01)
#
#
#   # run mcmc
#   fit1 <- jags.parallel(data=data.now,
#                         parameters.to.save=params,
#                         n.iter=10000, n.chains=2,n.thin=1,n.burnin=5000,
#                         model.file="funs/jags_cfa4.R")
#
#   est <- fit1$BUGSoutput$summary
#   save(est,datr,file = paste0("resultssim/",cond,"_r",r,"_cfa4.rdata"))
# }


runcfa <- function(r,datr){

  ydatr <- datr[,1:(p*q)]
  odatr <- datr[,1:(p*q)+(p*q)]
  vydatr <- datr[,1:(p*q)+(p*q*2)]

  # parameters to safe
  params <- c("lySt","psiy_i","psiy",
              "Fcor",
              "sigmaxi",
              "b0","b_return",
              "p.att","p.inatt","p.diff",
              "C",
              "p0",
              "log_lik",
              "Cratio","Call")

  ### Generate indexes for model
  index <- c(rep(1,10),
             rep(2,10),
             rep(3,10),
             rep(4,10),
             rep(5,10),
             rep(6,10))

  xx <- expand.grid(1:6,1:6)
  index2 <- xx[ xx[,1] != xx[,2],]
  # Omits marker variables
  index3 <- seq(1,60,1)[-c(1,11,21,31,41,51)]

  maxV <- ((max(ydatr)-min(ydatr))/2)^2
  # list of data
  data.now <- list(N=N,y=ydatr,R0=diag(q),ord=odatr, minpsi=1/maxV,
                   nfactor=6,
                   index = index,
                   index2 = index2,
                   index3 = index3,
                   muy0 = mean(unlist(ydatr)),
                   p.return = 0.01)


  # run mcmc
  fit1 <- jags.parallel(data=data.now,
                        parameters.to.save=params,
                        n.iter=10000, n.chains=2,n.thin=1,n.burnin=5000,
                        model.file="funs/jags_cfa.R")

  est <- fit1$BUGSoutput$summary
  save(est,datr,file = paste0("resultssim/",cond,"_r",r,"_cfa.rdata"))
}

