
# dynamic response simulation
# inattention depends on xis
# this tests now when the nondynamic class model breaks down

#library(mvtnorm)
library(foreach)
library(parallel)
library(doParallel)
# library(R2jags)
# library(coda)

# setwd("./simulations")

##################################
# 1.  conditions
##################################
R<-100

Nt <- c(100,200)
prop <- c(.1,.2,.4,1)
Nfac <- c(6)
rel <- c("low","high")
qp <- c(0) #switch from start or after 2/3
rand <- c(1,2)

p <- Nitems <- 10

expit <- function(x){1/(1+exp(-x))}

source("funs/runcfa.R")
source("funs/runstat.R")
source("funs/rundyn.R")
source("funs/vyfun2.R")

###############################################
#analysis functions
###############################################
#params <- c("ly","sigmaxi","sigmay","ty1","b0","Ct","Cend","Cmax")

cores <- 30

cl<-makeCluster(cores, outfile="")
registerDoParallel(cl)

parallel.output <-
  foreach(r = 1:R,.errorhandling = "remove") %:%
    foreach(k = 1:length(Nt),.errorhandling = "remove") %:%
    foreach(o = 1:length(rand),.errorhandling = "remove") %:%
    foreach(l = 1:length(prop),.errorhandling = "remove",
            .packages = c('R2jags','coda'))%dopar% {
        for(m in 1:length(rel)){#m<-1
          for(n in 1:length(qp)){#n<-1

              for(q in Nfac){#q<-3

                cond <- paste0("N",Nt[k],"_prop",l,"_rel",m,"_qp",n,"_rand",o,"_fac",q)

                datr <- read.table(paste0("datasim/",cond,".dat"),header=T)

                N <- Nt[k]

                datr.now <- datr[datr$r==r,]
                rundyn(r,datr.now)
                runstat(r,datr.now)
                runcfa(r,datr.now)
              }
            }
          }
        }
              return(TRUE)
      }
cat('---------- FINISHED ------------')




