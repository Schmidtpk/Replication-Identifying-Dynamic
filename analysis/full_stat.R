seed.cur <- 42

set.seed(seed.cur)

# + server ----------------------------------------------------------------
### check if file is executed on server
if(grepl("quamet",getwd())){
  message("Working directory looks like server. Different options are now valid")
  server <- TRUE
} else {
  # set working directory to project (needed if run in RStudio "jobs")
  setwd(gsub("/cnrmturk.*","/cnrmturk",getwd()))
  server <- FALSE
}
library(R2jags)
library(ReplicationIdentifyingDynamic)
library(DynExpData)

library(tidyverse)

### Load data
d <- DynExpData::pull_data(choice.cond = 1:4,
                         standardize = F)


### model
mod <-  "analysis/submission/jags_static.R"
if(server)
  mod <-  "jags_static.R"

# + indices -----------------------------------------------------------------
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

vy_24 <- makevy2(d$ymat,d$iord,d$tord)
cd_24 <- ReplicationIdentifyingDynamic::cd_mat_full

input <- list(y = d$ymat,
              ord = d$iord,
              R0 = diag(10), # number of factors = 10
              N = dim(d$ymat)[1],
              muy0 = 3.5,
              cd = cd_24[rownames(d$ymat),100],
              vy = vy_24[,100],
              index = index,
              index2 = index2,
              index3 = index3)


params <- c("lySt",
            "Fcor",
            "sigmaxi",
            "C",
            "b0",
            "log_lik")

message("start sampling.")
time <- Sys.time()
fit <-
  jags.parallel(
    jags.seed = seed.cur, # set seed
    data=input,
    parameters.to.save=params,
    n.iter=40000, n.chains=4,n.thin=2,n.burnin=20000,
    model.file=mod)
input$time.passed <- Sys.time()-time
message(paste("Sampling finished after", input$time.passed,units(input$time.passed)))

name <- paste0("analysis/submission/save/",format(Sys.time(),"%d_%m_%y"),"static.RDS")

if(server)
  name <- paste0("save/",format(Sys.time(),"%d_%m_%y"),"static.RDS")

out <- list("fit" = fit,
  "summary" = fit$BUGSoutput$summary,
  "log_lik"=fit$BUGSoutput$sims.list$log_lik,
  "input" = input,
  "Treat" = d$treatment.timing,
  "Cond" = d$cond,
  "sum" = MCMCvis::MCMCsummary(fit))

message("Saving.")
saveRDS(out,name)

stop("FInished")
