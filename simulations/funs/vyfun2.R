# variance function

#ordat <- ydat[,61:120]
#ydat <- ydat[,1:60]

#library(stringr)

#vyfun2 <- function(ydat,ordat,p=p,q=q){
#  vy <- ydat
#  N  <- dim(ydat)[1]
#
#  for(i in 1:N){#i <- 1
#    ydati <- ydat[i,]
#    ordi  <- ordat[i,]
#    #names(vyi) <- colnames(ydat)
#    nom.ydati<- qp <- c()
#
#    for(k0 in 1:(q*p)){#k0<-7
#      #counts through all items
#      nom.ydatik <- names(ydati)[ordi==k0]
#      qp.temp <- as.numeric(str_extract_all(nom.ydatik,"\\(?[0-9,.]+\\)?")[[1]])
#      qp <- rbind(qp,qp.temp)
#
#      nom.ydati <- c(nom.ydati,nom.ydatik)
#
#      vyq <- rep(NA,q)
#      for(m0 in 1:q){#m0 <- 1
#
#        if(sum(qp[,1]==m0)==1){
#          vyq[m0] <- 0
#        }else if(sum(qp[,1]==m0)>1)
#          vyq[m0] <- sd(ydati[paste0("y",m0,"_",qp[qp[,1]==m0,2])])^2
#      }
#
#      vy[i,ordi==k0] <- mean(vyq,na.rm=T)
#    }
#
#
#  }
#
#  nomvy <- c()
#  for(k0 in 1:q){
#    nomvy <- c(nomvy, paste0("vy",k0,"_",1:p))
#  }
#
#  colnames(vy) <- nomvy
#  return(vy)
#
#}



#vy.1 <- vyfun(ydat[,1:60],ydat[,1:60+60])
#vy.2 <- vy.1[ydat$bad==0,]
#vy.3 <- vy.1[ydat$bad!=0,]

#ord.2 <- ydat[ydat$bad==0,1:60+60]
#ord.3 <- ydat[ydat$bad!=0,1:60+60]

#par(mfrow=c(1,1),new=F)
#for(i in 1:dim(vy.2)[1]){
#  plot(1:60,vy.2[i,unlist(ord.2[i,])],ylim=c(0,10),col=1,type="l")
#  par(new=T)
#}

#for(i in 1:dim(vy.3)[1]){
#  plot(1:60,vy.3[i,unlist(ord.3[i,])],ylim=c(0,10),col=2,type="l")
#  par(new=T)
#}

model0 <- list()

model0[[3]] <- '
xi1 =~ y1_1+y1_2+y1_3+y1_4+y1_5+y1_6+y1_7+y1_8+y1_9+y1_10
xi2 =~ y2_1+y2_2+y2_3+y2_4+y2_5+y2_6+y2_7+y2_8+y2_9+y2_10
xi3 =~ y3_1+y3_2+y3_3+y3_4+y3_5+y3_6+y3_7+y3_8+y3_9+y3_10
'

model0[[6]] <- '
xi1 =~ y1_1+y1_2+y1_3+y1_4+y1_5+y1_6+y1_7+y1_8+y1_9+y1_10
xi2 =~ y2_1+y2_2+y2_3+y2_4+y2_5+y2_6+y2_7+y2_8+y2_9+y2_10
xi3 =~ y3_1+y3_2+y3_3+y3_4+y3_5+y3_6+y3_7+y3_8+y3_9+y3_10
xi4 =~ y4_1+y4_2+y4_3+y4_4+y4_5+y4_6+y4_7+y4_8+y4_9+y4_10
xi5 =~ y5_1+y5_2+y5_3+y5_4+y5_5+y5_6+y5_7+y5_8+y5_9+y5_10
xi6 =~ y6_1+y6_2+y6_3+y6_4+y6_5+y6_6+y6_7+y6_8+y6_9+y6_10
'

model0[[12]] <- '
xi1 =~ y1_1+y1_2+y1_3+y1_4+y1_5+y1_6+y1_7+y1_8+y1_9+y1_10
xi2 =~ y2_1+y2_2+y2_3+y2_4+y2_5+y2_6+y2_7+y2_8+y2_9+y2_10
xi3 =~ y3_1+y3_2+y3_3+y3_4+y3_5+y3_6+y3_7+y3_8+y3_9+y3_10
xi4 =~ y4_1+y4_2+y4_3+y4_4+y4_5+y4_6+y4_7+y4_8+y4_9+y4_10
xi5 =~ y5_1+y5_2+y5_3+y5_4+y5_5+y5_6+y5_7+y5_8+y5_9+y5_10
xi6 =~ y6_1+y6_2+y6_3+y6_4+y6_5+y6_6+y6_7+y6_8+y6_9+y6_10
xi7 =~ y7_1+y7_2+y7_3+y7_4+y7_5+y7_6+y7_7+y7_8+y7_9+y7_10
xi8 =~ y8_1+y8_2+y8_3+y8_4+y8_5+y8_6+y8_7+y8_8+y8_9+y8_10
xi9 =~ y9_1+y9_2+y9_3+y9_4+y9_5+y9_6+y9_7+y9_8+y9_9+y9_10
xi10 =~ y10_1+y10_2+y10_3+y10_4+y10_5+y10_6+y10_7+y10_8+y10_9+y10_10
xi11 =~ y11_1+y11_2+y11_3+y11_4+y11_5+y11_6+y11_7+y11_8+y11_9+y11_10
xi12 =~ y12_1+y12_2+y12_3+y12_4+y12_5+y12_6+y12_7+y12_8+y12_9+y12_10
'

cdfun <- function(ydat,odat,p=p,q=q){
  require(lavaan)
  #ydat <-ydatr;odat <- odatr
  upsilon <- ydat
  N  <- dim(ydat)[1]

  sem0 <- sem(model0[[q]],ydat,meanstructure =T)
  sigma0 <- fitted(sem0)


  for(i in 1:N){#i <- 1
    ydati <- ydat[i,]
    ordi  <- odat[i,]
    #names(vyi) <- colnames(ydat)
    nom.ydati<- qp <- c()

    for(k0 in 1:(q*p)){#k0<-1
      #counts through all items
      nom.ydatik <- names(ydati)[ordi==k0]
      nom.ydati <- c(nom.ydati,nom.ydatik)
      #qp.temp <- as.numeric(str_extract_all(nom.ydatik,"\\(?[0-9,.]+\\)?")[[1]])
      #qp <- rbind(qp,qp.temp)

      # m-distances
      if(length(nom.ydati)==1){
        mu0 <- mean(ydat[,nom.ydati])
        sig0 <- sd(ydat[,nom.ydati])^2
      }else{
        mu0 <- apply(ydat[,nom.ydati],2,mean)
        sig0 <- cov(ydat[,nom.ydati])
      }

      md1 <- mahalanobis(ydati[,nom.ydati],center=mu0,cov=sig0)
      md2 <- mahalanobis(ydati[,nom.ydati],center=sigma0$mean[nom.ydati],cov=sigma0$cov[nom.ydati,nom.ydati])

      if(length(nom.ydati)==1){
        cd1 <- -0.5*(k0*log(2*pi)+log(sig0)+md1)
        cd2 <- -0.5*(k0*log(2*pi)+log(sigma0$cov[nom.ydati,nom.ydati])+md2)
      }else{
        cd1 <- -0.5*(k0*log(2*pi)+log(det(sig0))+md1)
        cd2 <- -0.5*(k0*log(2*pi)+log(det(sigma0$cov[nom.ydati,nom.ydati]))+md2)
      }


      #neu: adjusment "/length(nom.ydati)"
      upsilon[i,ordi==k0] <- -2*(cd2-cd1)
    }
  }

  nomvy <- c()
  for(k0 in 1:q){
    nomvy <- c(nomvy, paste0("cd",k0,"_",1:p))
  }

  colnames(upsilon) <- nomvy
  return(upsilon)

}



add.na.matrix <- function(A,B){
  dim1 <- dim(B)[1]
  dim2 <- dim(B)[2]

  for(i0 in 1:dim1){
    for(i1 in 1:dim2){
      if(is.na(B[i0,i1])==F){
        A[i0,i1] <- B[i0,i1]
      }


    }
  }

  A
}


vyfun <- function(ydat,odat,p=p,q=q){
  vy <- as.matrix(ydat)
  N  <- dim(ydat)[1]

  ydat0 <- matrix(NA,p,q)

  for(i in 1:N){#i <- 1
    ydati <- ydat[i,]
    ordi  <- odat[i,]
    #names(vyi) <- colnames(ydat)

    y02 <- matrix(NA,p,q)
    for(k0 in 1:(q*p)){#k0<-1
      wo <- which(ordi==k0)
      y01 <- rep(NA,q*p)
      y01[wo] <- unlist(ydati[wo])

      y02 <- add.na.matrix(y02,matrix(y01,p,q,byrow=F))

      vy[i,ordi==k0] <- mean(apply(y02,2,var,na.rm=T),na.rm=T)
    }


  }

  nomvy <- c()
  for(k0 in 1:q){
    nomvy <- c(nomvy, paste0("vy",k0,"_",1:p))
  }

  vy[is.nan(vy)] <- 0
  vy[is.na(vy)]  <- 0

  colnames(vy) <- nomvy

  vy

}

vyfun.kernel <- function(ydat,odat,p=p,q=q){
  vy <- as.matrix(ydat)
  N  <- dim(ydat)[1]

  p0 <- dnorm(1:6,mean=6)

  ydat0 <- matrix(NA,p,q)

  for(i in 1:N){#i <- 1
    ydati <- ydat[i,]
    ordi  <- odat[i,]
    #names(vyi) <- colnames(ydat)

    y02 <- matrix(NA,p,q)
    for(k0 in 1:(q*p)){#k0<-1
      wo <- which(ordi==k0)
      y01 <- rep(NA,q*p)
      y01[wo] <- unlist(ydati[wo])

      y02 <- add.na.matrix(y02,matrix(y01,p,q,byrow=F))

      vy[i,ordi==k0] <- mean(apply(y02*p0,2,var,na.rm=T),na.rm=T)
    }


  }

  nomvy <- c()
  for(k0 in 1:q){
    nomvy <- c(nomvy, paste0("vy",k0,"_",1:p))
  }

  vy[is.nan(vy)] <- 0
  vy[is.na(vy)]  <- 0

  colnames(vy) <- nomvy

  vy

}

stfun <- function(ydat,odat,p=p,q=q){
  N  <- dim(ydat)[1]
  vy <- matrix(0,N,p*q)

  for(i in 1:N){#i <- 1
    ydati <- ydat[i,]
    ordi  <- odat[i,]

    y01 <- c()
    for(k0 in 1:(q*p)){#k0<-1
      wo <- which(ordi==k0)
      y01 <- c(y01,unlist(ydati[wo]))

      if(k0>5){
        #if(all(abs(y01[1:6+k0-6])==0.5)){
        #  vy[i,ordi==k0] <- 1
        #}
        vy[i,ordi==k0] <- sum(abs(y01[1:6+k0-6])==0.5)

      }



    }


  }

  nomvy <- c()
  for(k0 in 1:q){
    nomvy <- c(nomvy, paste0("st",k0,"_",1:p))
  }

  vy[is.nan(vy)] <- NA

  colnames(vy) <- nomvy

  vy

}





















