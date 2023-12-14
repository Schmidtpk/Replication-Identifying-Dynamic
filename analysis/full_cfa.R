seed.cur <- 42
n.iter <- 10000


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
library(DynExpData)
library(tidyverse)

### Load data
# choice.treatment <- df.instr%>%
#   filter(cond==4,confidence==1)%>%
#   pull(id)
# choice.control <- names(cond)[cond==2]%>%sample(length(choice.treatment))
# choice.id <- c(choice.treatment,choice.control)

d <- pull_data(standardize = F,choice.cond = 1:4)


### model
mod <-  "analysis/submission/jags_cfa.R"
if(server)
  mod <-  "jags_cfa.R"

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

# add rownmaes to vy_24 for subsetting

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
            "log_lik")

message("start sampling.")
time <- Sys.time()
fit <-
  jags.parallel(
    jags.seed = seed.cur, # set seed
    export_obj_names = 'n.iter', # user n.iter for sample size
    data=input,
    parameters.to.save=params,
    n.iter=n.iter, n.chains=4,n.thin=ceiling(n.iter/100),n.burnin=n.iter/2,
    model.file=mod)
input$time.passed <- Sys.time()-time
message(paste("Sampling finished after", input$time.passed,units(input$time.passed)))

name <- paste0("analysis/submission/save/",format(Sys.time(),"%d_%m_%y"),"cfa.RDS")

if(server)
  name <- paste0("save/",format(Sys.time(),"%d_%m_%y"),"cfa.RDS")

out <- list("fit" = fit,
  "summary" = fit$BUGSoutput$summary,
  "log_lik"=fit$BUGSoutput$sims.list$log_lik,
  "input" = input,
  "Treat" = d$treatment.timing,
  "Cond" = d$cond,
  "sum" = MCMCvis::MCMCsummary(fit))

message("Saving.")
saveRDS(out,name)

stop("Finished")

