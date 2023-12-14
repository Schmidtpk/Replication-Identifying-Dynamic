# from submission/jags_dyn3
# adapt number of factors
# adapt to static
model{

  # constants
  for(k in 1:nfactor){
    ly[(k-1)*10+1] <- 1
    ty1[(k-1)*10+1] <- muy0}

  for(i in 1:N){
  # i = person

  ##############################################
  # within level
  ##############################################

    for(j in 1:(nfactor*10)){
    # j = item
      y[i,j] ~ dnorm(
        ifelse(C[i]==1, ty1[j] + ly[j]*xi[i,index[j]], ty2),
        ifelse(C[i]==1, psiy[j],psiy_i[i]))
    }


    # factors
    # psixi is precision matrix
    # muxi is mean vector
    xi[i,1:nfactor] ~ dmnorm(muxi,psixi)


    # switching probabilities in attention state
    PCa[i,1] <- 1/(exp(-(b0[1]+b0[2]*vy[i]
    ))+1)
    PCa[i,2] <- 1-PCa[i,1]

    # markov states
    C[i] ~ dcat(PCa[i,1:2])

  }

  ##############################################
  # priors
  ##############################################


  for(j in 1:(nfactor*10)){
    psiy[j] ~ dgamma(.1,.1)T(minpsi,) }

  for(i in 1:N){
    psiy_i[i] ~ dgamma(.1,.1)T(minpsi,) }

  ty2 <- muy0 # Dropped person index, so it's just a single parameter

  for(j in 1:(nfactor*9)){
    ly[index3[j]]  ~ dnorm(1,1)T(0,) # T instead of I for clarity
    ty1[index3[j]] ~ dnorm(muy0,1) }

  for(j in 1:nfactor){muxi[j] = 0}
  psixi[1:nfactor,1:nfactor] ~ dwish(R0[1:nfactor,1:nfactor],nfactor)

  b0[1]~dnorm(5,1/25) # increasing standard deviation of intercept.
  b0[2]~dnorm(0,1)

  ##############################################
  # Log-likelihood (for model comparison)
  ##############################################
  # for(i in 1:N){
  #   for(j in 1:(nfactor*10)){
  #     log_lik0[i,j] <- logdensity.norm(y[i,j],
  #                                      ifelse(C[i,j]==1, ty1[j] + ly[j]*xi[i,index[j]], ty2),
  #                                      ifelse(C[i,j]==1, psiy[j],psiy_i[i]))
  #   }
  #   log_lik[i] <- sum(log_lik0[i,])
  # }


  ##############################################
  # STD loadings and covariances
  ##############################################

  for(j in 1:(nfactor*10)){
    sigmay[j] <- 1/psiy[j] } #zac: right?
  sigmaxi[1:nfactor,1:nfactor] <- inverse(psixi[1:nfactor,1:nfactor])

  for(j in 1:(nfactor*10)){
    vx[j] <- ly[j]^2*sigmaxi[index[j],index[j]] + sigmay[j]
    lySt[j] <- ly[j]*sqrt(sigmaxi[index[j],index[j]] / vx[j]) }

  for(q in 1:(nfactor*(nfactor-1))){
    Fcor[index2[q,1],index2[q,2]] <- sigmaxi[index2[q,1],index2[q,2]] / sqrt(sigmaxi[index2[q,1],index2[q,1]]*sigmaxi[index2[q,2],index2[q,2]]) }


}
