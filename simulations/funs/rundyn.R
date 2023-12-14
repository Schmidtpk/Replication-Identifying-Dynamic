
# rundyn <- function(r,datr){
#
#   ydatr <- datr[,1:(p*q)]
#   odatr <- datr[,1:(p*q)+(p*q)]
#   vydatr <- datr[,1:(p*q)+(p*q*2)]
#   cddatr <- cdfun(ydatr,odatr,p,q)
#
#   # standardize markov model covariates
#   vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
#   cddatr <- (cddatr - mean(unlist(cddatr)))/sd(unlist(cddatr))
#
#   # parameters to safe
#   params <- c("lySt","psiy_i","psiy","ty1",
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
#   data.now <- list(N=N,y=ydatr,R0=diag(q),vy=vydatr,ord=odatr,
#                    cd =  cddatr,
#                    nfactor=6,
#                    index = index,
#                    index2 = index2,
#                    index3 = index3,
#                    muy0 = mean(unlist(ydatr)),
#                    p.return = 0.0001,
#                    p0min=.999,p0max=1)
#
#
#   # run mcmc
#   fit1 <- jags.parallel(data=data.now,
#                         parameters.to.save=params,
#                         n.iter=1000, n.chains=2,n.thin=1,n.burnin=500,
#                         model.file="funs/jags_dyn.R")
#
#   est <- fit1$BUGSoutput$summary
#   save(est,datr,fit1,file = paste0("resultssim/",cond,"_r",r,"_dyn.rdata"))
# }
#
# rundyn2 <- function(r,datr){
#
#   ydatr <- datr[,1:(p*q)]
#   odatr <- datr[,1:(p*q)+(p*q)]
#   vydatr <- datr[,1:(p*q)+(p*q*2)]
#   cddatr <- cdfun(ydatr,odatr,p,q)
#
#   # standardize markov model covariates
#   vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
#   cddatr <- (cddatr - mean(unlist(cddatr)))/sd(unlist(cddatr))
#
#   # parameters to safe
#   params <- c("lySt","psiy_i","psiy","ty1",
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
#   data.now <- list(N=N,y=ydatr,R0=diag(q),vy=vydatr,ord=odatr,
#                    cd =  cddatr,
#                    nfactor=6,
#                    index = index,
#                    index2 = index2,
#                    index3 = index3,
#                    muy0 = mean(unlist(ydatr)),
#                    p.return = 0.01,
#                    p0min=.5,p0max=1)
#
#
#   # run mcmc
#   fit1 <- jags.parallel(data=data.now,
#                         parameters.to.save=params,
#                         n.iter=10000, n.chains=2,n.thin=1,n.burnin=5000,
#                         model.file="funs/jags_dyn2.R")
#
#   est <- fit1$BUGSoutput$summary
#   save(est,datr,fit1,file = paste0("resultssim/",cond,"_r",r,"_dyn2.rdata"))
# }
#
#
# rundyn3 <- function(r,datr){
#
#   ydatr <- datr[,1:(p*q)]
#   odatr <- as.matrix(datr[,1:(p*q)+(p*q)])
#   vydatr <- as.matrix(datr[,1:(p*q)+(p*q*2)])
#   cddatr <- as.matrix(cdfun(ydatr,odatr,p,q))
#
#   # standardize markov model covariates
#   vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
#   cddatr <- (cddatr - mean(unlist(cddatr)))/sd(unlist(cddatr))
#
#   # order by item number
#   vy <- vydatr
#   cd <- cddatr
#   for(i in 1:N){
#     vydatr[i,]<-vy[i,odatr[i,]]
#     cddatr[i,]<-cd[i,odatr[i,]]
#   }
#
#   # parameters to safe
#   params <- c("lySt","psiy_i","psiy","ty1",
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
#
#   # markov model inputs (dimensions=individiual x item number(time) x covariate number)
#   x <- array(NA,dim=c(N,60,2))
#   x[,,1]<- as.matrix(vydatr)
#   x[,,2]<- as.matrix(cddatr)
#   nx <- dim(x)[3]+1
#
#
#   # list of data
#   data.now <- list(N=N,y=ydatr,R0=diag(q),
#                    x = x,nx=nx,
#                    ord=odatr,
#                    nfactor=6,
#                    index = index,
#                    index2 = index2,
#                    index3 = index3,
#                    muy0 = mean(unlist(ydatr)),
#                    p.return = 0.01,
#                    p0min=.999,p0max=1)
#
#   C0 <- array(1,dim = c(N,60))
#   for(i in 1:N){
#     C0[i,odatr[i,31:60]]<-2
#   }
#
#
#   # run mcmc
#   fit1 <- jags.parallel(data=data.now,inits = function() return(list(C=C0)),
#                         parameters.to.save=params,
#                         n.iter=5000, n.chains=2,n.thin=1,n.burnin=2500,
#                         model.file="funs/jags_dyn3.R")
#
#   est <- fit1$BUGSoutput$summary
#   save(est,datr,file = paste0("resultssim/",cond,"_r",r,"_dyn3.rdata"))
# }
#
#
# rundyn4 <- function(r,datr){
#
#   ydatr <- datr[,1:(p*q)]
#   odatr <- as.matrix(datr[,1:(p*q)+(p*q)])
#   vydatr <- as.matrix(datr[,1:(p*q)+(p*q*2)])
#
#   # standardize markov model covariates
#   vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
#
#   # order by item number
#   vy <- vydatr
#   for(i in 1:N){
#     vydatr[i,]<-vy[i,odatr[i,]]
#   }
#
#   # parameters to safe
#   params <- c("lySt",#"psiy_i","psiy","ty1",
#               "Fcor",
#               #"sigmaxi",
#               "b0",#"b_return",
#               #"p.att","p.inatt","p.diff",
#               "C",
#               "p0",
#               #"log_lik",
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
#
#   # markov model inputs (dimensions=individiual x item number(time) x covariate number)
#   x <- array(NA,dim=c(N,60,1))
#   x[,,1]<- as.matrix(vydatr)
#   nx <- dim(x)[3]+1
#
#
#   print(q)
#   maxV <- ((max(ydatr)-min(ydatr))/2)^2
#   print(maxV)
#   # list of data
#   data.now <- list(N=N,y=ydatr,R0=diag(q),maxV=maxV,
#                    x = x,nx=nx,
#                    ord=odatr,
#                    nfactor=6,
#                    index = index,
#                    index2 = index2,
#                    index3 = index3,
#                    muy0 = mean(unlist(ydatr)),
#                    p.return = 0.01,
#                    p0min=.5,p0max=1)
#
#   C0 <- array(1,dim = c(N,60))
#   for(i in 1:N){
#     C0[i,odatr[i,31:60]]<-2
#   }
#
#
#   message("START FIT",cond)
#   # run mcmc
#   fit1 <- jags.parallel(data=data.now,#inits = function() return(list(C=C0)),
#                         parameters.to.save=params,jags.seed = r,
#                         n.iter=2000, n.chains=2,n.thin=1,n.burnin=1000,
#                         model.file="funs/jags_dyn4.R")
#
#   message("finished FIT",cond)
#
#   est <- fit1$BUGSoutput$summary
#   save(est,datr,file = paste0("resultssim/",cond,"_r",r,"_dyn4.rdata"))
# }
#
#
# rundyn5 <- function(r,datr){
#
#   ydatr <- datr[,1:(p*q)]
#   odatr <- as.matrix(datr[,1:(p*q)+(p*q)])
#   vydatr <- as.matrix(datr[,1:(p*q)+(p*q*2)])
#
#   # standardize markov model covariates
#   vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
#
#   # order by item number
#   vy <- vydatr
#   for(i in 1:N){
#     vydatr[i,]<-vy[i,odatr[i,]]
#   }
#
#   # parameters to safe
#   params <- c("lySt",#"psiy_i","psiy","ty1",
#               "Fcor",
#               #"sigmaxi",
#               "b0",#"b_return",
#               #"p.att","p.inatt","p.diff",
#               "C",
#               "p0",
#               #"log_lik",
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
#
#   # markov model inputs (dimensions=individiual x item number(time) x covariate number)
#   x <- array(NA,dim=c(N,60,1))
#   x[,,1]<- as.matrix(vydatr)
#   nx <- dim(x)[3]+1
#
#
#
#   maxV <- ((max(ydatr)-min(ydatr))/2)^2
#
#   # list of data
#   data.now <- list(N=N,y=ydatr,R0=diag(q),minpsi=1/maxV,
#                    x = x,nx=nx,
#                    ord=odatr,
#                    nfactor=6,
#                    index = index,
#                    index2 = index2,
#                    index3 = index3,
#                    muy0 = mean(unlist(ydatr)),
#                    p.return = 0.01,
#                    p0min=.5,p0max=1)
#
#   C0 <- array(1,dim = c(N,60))
#   for(i in 1:N){
#     C0[i,odatr[i,31:60]]<-2
#   }
#
#
#   message("START FIT",cond)
#   # run mcmc
#   fit1 <- jags.parallel(data=data.now,inits = function() return(list(C=C0)),
#                         parameters.to.save=params,jags.seed = r,
#                         n.iter=10000, n.chains=2,n.thin=1,n.burnin=5000,
#                         model.file="funs/jags_dyn5.R")
#
#   message("finished FIT",cond)
#
#   est <- fit1$BUGSoutput$summary
#   save(est,datr,file = paste0("resultssim/",cond,"_r",r,"_dyn5.rdata"))
# }

rundyn <- function(r,datr){

  ydatr <- datr[,1:(p*q)]
  odatr <- as.matrix(datr[,1:(p*q)+(p*q)])
  vydatr <- as.matrix(datr[,1:(p*q)+(p*q*2)])
  # cddatr <- as.matrix(cdfun(ydatr,odatr,p,q))

  # standardize markov model covariates
  vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
  # cddatr <- (cddatr - mean(unlist(cddatr)))/sd(unlist(cddatr))

  # order by item number
  vy <- vydatr
  # cd <- cddatr
  for(i in 1:N){
    vydatr[i,]<-vy[i,odatr[i,]]
    # cddatr[i,]<-cd[i,odatr[i,]]
  }


  # parameters to safe
  params <- c("lySt","psiy_i","psiy","ty1",
              "Fcor",
              #"sigmaxi",
              "b0",#"b_return",
              #"p.att","p.inatt","p.diff",
              "C",
              "p0",
              #"log_lik",
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



  # markov model inputs (dimensions=individiual x item number(time) x covariate number)
  x <- array(NA,dim=c(N,60,1))
  x[,,1]<- as.matrix(vydatr)
  # x[,,2]<- as.matrix(cddatr)
  nx <- dim(x)[3]+1




  maxV <- ((max(ydatr)-min(ydatr))/2)^2

  # list of data
  data.now <- list(N=N,y=ydatr,R0=diag(q),minpsi=1/maxV,
                   x = x,nx=nx,
                   ord=odatr,
                   nfactor=6,
                   index = index,
                   index2 = index2,
                   index3 = index3,
                   muy0 = mean(unlist(ydatr)),
                   p.return = 0.01,
                   p0min=.5,p0max=1)

  C0 <- array(1,dim = c(N,60))
  for(i in 1:N){
    C0[i,odatr[i,31:60]]<-2
  }


  message("START FIT",cond)
  # run mcmc
  fit1 <- jags.parallel(data=data.now,inits = function() return(list(C=C0)),
                        parameters.to.save=params,jags.seed = r,
                        n.iter=2000, n.chains=3,n.thin=1,n.burnin=1000,
                        model.file="funs/jags_dyn.R")

  message("finished FIT",cond)

  est <- fit1$BUGSoutput$summary
  save(fit1,
       est,datr,file = paste0("resultssim/",cond,"_r",r,"_dyn.rdata"))
}


rundyn7 <- function(r,datr){

  ydatr <- datr[,1:(p*q)]
  odatr <- as.matrix(datr[,1:(p*q)+(p*q)])
  vydatr <- as.matrix(datr[,1:(p*q)+(p*q*2)])
  # cddatr <- as.matrix(cdfun(ydatr,odatr,p,q))

  # standardize markov model covariates
  vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
  # cddatr <- (cddatr - mean(unlist(cddatr)))/sd(unlist(cddatr))

  # order by item number
  vy <- vydatr
  # cd <- cddatr
  for(i in 1:N){
    vydatr[i,]<-vy[i,odatr[i,]]
    # cddatr[i,]<-cd[i,odatr[i,]]
  }


  # parameters to safe
  params <- c("lySt","psiy_i","psiy","ty1",
              "Fcor",
              #"sigmaxi",
              "b0",#"b_return",
              #"p.att","p.inatt","p.diff",
              "C",
              "p0",
              #"log_lik",
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



  # markov model inputs (dimensions=individiual x item number(time) x covariate number)
  x <- array(NA,dim=c(N,60,1))
  x[,,1]<- as.matrix(vydatr)
  # x[,,2]<- as.matrix(cddatr)
  nx <- dim(x)[3]+1

  maxV <- ((max(ydatr)-min(ydatr))/2)^2
  minV <- (0.5)^2

  # list of data
  data.now <- list(N=N,y=ydatr,R0=diag(q),minpsi=1/maxV,maxpsi=1/minV,
                   x = x,nx=nx,
                   ord=odatr,
                   nfactor=6,
                   index = index,
                   index2 = index2,
                   index3 = index3,
                   muy0 = mean(unlist(ydatr)),
                   p.return = 0.01,
                   p0min=.5,p0max=1)

  C0 <- array(1,dim = c(N,60))
  for(i in 1:N){
    C0[i,odatr[i,31:60]]<-2
  }


  message("START FIT",cond)
  # run mcmc
  fit1 <- jags.parallel(data=data.now,inits = function() return(list(C=C0)),
                        parameters.to.save=params,jags.seed = r,
                        n.iter=2000, n.chains=4,n.thin=1,n.burnin=0,
                        model.file="funs/jags_dyn7.R")

  message("finished FIT",cond)

  est <- fit1$BUGSoutput$summary
  save(fit1,est,datr,file = paste0("resultssim/",cond,"_r",r,"_dyn7.rdata"))
}


rundyn8 <- function(r,datr){

  ydatr <- datr[,1:(p*q)]
  odatr <- as.matrix(datr[,1:(p*q)+(p*q)])
  vydatr <- as.matrix(datr[,1:(p*q)+(p*q*2)])
  # cddatr <- as.matrix(cdfun(ydatr,odatr,p,q))

  # standardize markov model covariates
  vydatr <- (vydatr - mean(unlist(vydatr)))/sd(unlist(vydatr))
  # cddatr <- (cddatr - mean(unlist(cddatr)))/sd(unlist(cddatr))

  # order by item number
  vy <- vydatr
  # cd <- cddatr
  for(i in 1:N){
    vydatr[i,]<-vy[i,odatr[i,]]
    # cddatr[i,]<-cd[i,odatr[i,]]
  }


  # parameters to safe
  params <- c("lySt","psiy_i","psiy","ty1","ty2",
              "Fcor",
              #"sigmaxi",
              "b0",#"b_return",
              #"p.att","p.inatt","p.diff",
              "C",
              "p0",
              #"log_lik",
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



  # markov model inputs (dimensions=individiual x item number(time) x covariate number)
  x <- array(NA,dim=c(N,60,1))
  x[,,1]<- as.matrix(vydatr)
  # x[,,2]<- as.matrix(cddatr)
  nx <- dim(x)[3]+1

  maxV <- ((max(ydatr)-min(ydatr))/2)^2
  minV <- (0.5)^2

  # list of data
  data.now <- list(N=N,y=ydatr,R0=diag(q),minpsi=1/maxV,maxpsi=1/minV,
                   x = x,nx=nx,
                   ord=odatr,
                   nfactor=6,
                   index = index,
                   index2 = index2,
                   index3 = index3,
                   muy0 = mean(unlist(ydatr)),
                   p.return = 0.01,
                   p0min=.5,p0max=1)

  C0 <- array(1,dim = c(N,60))
  for(i in 1:N){
    C0[i,odatr[i,31:60]]<-2
  }


  message("START FIT",cond)
  # run mcmc
  fit1 <- jags.parallel(data=data.now,inits = function() return(list(C=C0)),
                        parameters.to.save=params,jags.seed = r,
                        n.iter=2000, n.chains=4,n.thin=1,n.burnin=0,
                        model.file="funs/jags_dyn8.R")

  message("finished FIT",cond)

  est <- fit1$BUGSoutput$summary
  save(fit1,est,datr,file = paste0("resultssim/",cond,"_r",r,"_dyn8.rdata"))
}

