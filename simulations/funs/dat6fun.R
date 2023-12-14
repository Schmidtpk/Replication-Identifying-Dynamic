
################################################
# this randomly selects mechanisms for CNR
# missingness depends on the first 3 factors
# data is generated in continuous form
################################################

#tru.par <- c(rep(sqrt(.5),30),rep(.5,30),rep(.3,3))

dat1a <- function(N=1000,p=10,q=6,prop0=.5,rel0="low",qp0=0,ran0=1,cat0=1,miss="MNAR"){

  # order of item presentation is random
  if(ran0==1){
    ord <- matrix(NA,N,q*p)
    for(i in 1:N){ord[i,] <-  sample(1:(q*p),(q*p))}
  }else if(ran0==2){
    ord <- matrix(sample(1:(q*p),(q*p)),N,q*p,byrow=T)
  }

  #type of cnr
  type <- sample(1:4,N,replace=T)

  # generate first data for all participants
  #N<-10000
  rho  <- .4
  phix <- diag(q)*(1-rho)+rho
  mux  <- rep(0,q)

  # generate the data such that v(y)=1
  ly <- matrix(0,p*q,q)

  if(rel0=="low"){
    td0 <- .75
    ly0 <- sqrt(.25)
  }else if(rel0=="high"){
    td0 <- .36
    ly0 <- sqrt(.64)
  }

  td <- diag(p*q)*td0

  for(j in 1:q){#j<-1
    ly[((j-1)*p+1):(j*p),j] <- ly0
  }

  xi <- rmvnorm(N,mux,phix)
  delta <- rmvnorm(N,rep(0,p*q),td)
  y1 <- xi%*%t(ly)+ delta

  ############################
  # no threshold
  ############################
  y <- y1

  if(cat0==1){
    ############################
    # threshold model with 6 categories
    ############################
    #same prop in each cat
    #th0 <- qnorm(1:5/6)
    #same dist betw cats
    th0 <- seq(-3,3,length.out = 7)[2:6]
    for(j in 1:(q*p)){
      y[y1[,j]< th0[1],j] <- 1
      y[y1[,j]>=th0[1]&y1[,j]<th0[2],j] <- 2
      y[y1[,j]>=th0[2]&y1[,j]<th0[3],j] <- 3
      y[y1[,j]>=th0[3]&y1[,j]<th0[4],j] <- 4
      y[y1[,j]>=th0[4]&y1[,j]<th0[5],j] <- 5
      y[y1[,j]>=th0[5],j] <- 6
    }
  }


  ##############################
  # contaminations
  ##############################

  y2 <- list()
  ##############################
  # [1] persons choose middle categories
  # [2] extreme but random left & right
  # [3] all on right side independent of xi
  # [4] persons choose completely random
  ##############################
  if(cat0==1){
    y2[[1]] <- matrix(sample(1:6,q*p*N,prob=c(0,.05,.45,.45,.05,0),replace=T),N,q*p)
    y2[[2]] <- matrix(sample(1:6,q*p*N,prob=c(.45,.05,0,0,.05,.45),replace=T),N,q*p)
    y2[[3]] <- matrix(sample(1:6,q*p*N,prob=c(.0,.0,0,.0,.15,.85),replace=T),N,q*p)
    y2[[4]] <- matrix(sample(1:6,q*p*N,replace=T),N,q*p)
  }

  ##############################
  if(cat0==2){
    y2[[1]] <- matrix(rnorm(N*q*p,0,.1),N,q*p)
    y2a <- matrix(rnorm(N*q*p,2,.1),N,q*p)
    y2b <- matrix(rnorm(N*q*p,-2,.1),N,q*p)
    samp <- sample(c(0,1),N*q*p,replace=T)
    y2[[2]] <- y2a*samp+(1-samp)*y2b
    y2[[3]] <- matrix(rnorm(N*q*p,2,.1),N,q*p)
    y2[[4]] <- matrix(runif(N*q*p,-3,3),N,q*p)
  }

  ##############################
  # invert every second item
  ##############################
  for(j in 1:4){
    if(cat0==1){y2[[j]][,1:(p*q/2)*2] <- 7-y2[[j]][,1:(p*q/2)*2]}
    if(cat0==2){y2[[j]][,1:(p*q/2)*2] <- -y2[[j]][,1:(p*q/2)*2]}
  }

  ##############################
  # create missing pattern
  # based on xi
  ##############################
  if(prop0<1){
    xi2 <- ampute(xi[,1:3],prop=prop0,mech=miss)$amp
    #xi2$weights

    c0 <- rep(1,N)
    c0[is.na(apply(xi2,1,mean))] <- 0
  }else{c0 <- rep(0,N)}
  #mean(c0)


  #colo <- c("red","blue")
  #plot(xi[,1],xi[,2],col=colo[c0+1])
  #round(cov(xi)-cov(xi[c0==0,]),2)
  #mean(abs(cor(xi)-cor(xi[c0==0,])))
  #apply(xi,2,mean)
  #apply(xi[c0==0,],2,mean)

  ##############################
  # actual switch
  ##############################
  #qp0 <- 1
  when <- sample((q*p*2/3*qp0+1):(q*p),N,replace = T)
  c1 <- matrix(1,N,q*p)
  for(i in 1:N){#i <- 1
    if(c0[i]==0){
      c1[i,(when[i]:(q*p))] <- 2
      y[i,ord[i,(when[i]:(q*p))]] <- y2[[type[i]]][i,ord[i,(when[i]:(q*p))]]
    }
  }

  #c0[1]
  #y[1,ord]

  #c1[,ord]
  #colnames(y) <- paste0("y",1:30)
  nomy <- nomo <- c()
  for(k in 1:q){
    nomy <- c(nomy, paste0("y",k,"_",1:p))
    nomo <- c(nomo, paste0("o",k,"_",1:p))
  }
  colnames(y) <- nomy
  colnames(ord) <- nomo

  tswitch <- when
  tswitch[c0==1] <- NA
  bad <- c0 #0 means bad, 1 is a good guy

  type[c0==1] <- NA

  ########################
  # recode cat0 data
  ########################
  if(cat0==1){
    y <- y-3.5
  }

  return(list(y,ord,bad,tswitch,type))
}

