# dynamic response simulation
# inattention depends on xis[,1:3]

# setwd("./simulations")

##################################
# 1.  conditions
##################################
R <- 100

Nt <- c(100,200)
prop <- c(.1,.2,.4,1)
Nfac <- c(6)
rel <- c("low","high")
qp <- c(0) #switch from start or after 2/3
rand <- c(1,2)

p <- Nitems <- 10

library(foreach)
library(parallel)
library(doParallel)

source("funs/dat6fun.R")
source("funs/vyfun2.R")

###############################################
# 2. loop function data generation
###############################################


cores <- 8

cl<-makeCluster(cores, outfile="")
registerDoParallel(cl)

foreach(k = 1:length(Nt)) %:%
  foreach(m = 1:length(rel)) %:%
  foreach(l = 1:length(prop),
          .packages = c('mice','mvtnorm'))%dopar% {

            set.seed(10242019)

            for(n in 1:length(qp)){#n<-1
              for(o in 1:length(rand)){#o<-1
                for(q in Nfac){#q<-3

                  cond <- paste0("N",Nt[k],"_prop",l,"_rel",m,"_qp",n,"_rand",o,"_fac",q)
                  repdat <- matrix(0,R*Nt[k],p*q*3+4)

                  for(r in 1:R){#r<-1
                    y1 <- dat1a(N=Nt[k],p=p,q=q,prop0=prop[l],rel0=rel[m],qp0=qp[n],ran0=rand[o])

                    ydat    <- data.frame(y1[[1]])
                    odat    <- data.frame(y1[[2]])
                    badguys <- y1[[3]]
                    whenbad <- y1[[4]]
                    cnrbad  <- y1[[5]]

                    vydat <- vyfun(ydat,odat,p,q)
                    ydat  <- cbind(ydat,odat,vydat)
                    ydat$bad  <- badguys
                    ydat$when <- whenbad
                    ydat$cnr  <- cnrbad
                    ydat$r    <-rep(r,Nt[k])

                    repdat[((r-1)*Nt[k]+1):(r*Nt[k]),] <- as.matrix(ydat)
                  }

                  colnames(repdat) <- colnames(ydat)

                  write.table(repdat,paste0("./datasim/",cond,".dat"),col.names = T,row.names = F)
                }
              }
            }
            return(TRUE)
          }



