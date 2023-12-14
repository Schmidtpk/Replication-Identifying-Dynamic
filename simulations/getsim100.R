library(latex2exp)
library(ReplicationIdentifyingDynamic)
library(tidyverse)

add_sum_names <- function(res){
  res$name <- gsub("\\[.*","",rownames(res))
  res$variable <- rownames(res)
  return(res)
}


theme_set(theme_bw())


Nt <- c(100,200)
prop <- c(.1,.2,.4,1)
Nfac <- c(6)
rel <- c("low","high")
qp <- c(0) #switch from start or after 2/3
rand <- c(1,2)


# load data ---------------------------------------------------------------


R <- 100
df <- df.s <- data.frame()
df.person <- data.frame()
for(r in 1:R){
  cat('---- ', r, ' -----')
  for(k in 1:length(Nt)){#k<-1
    for(l in 1:length(prop)){#l<-4
      for(m in 1:length(rel)){#m<-1
        for(n in 1:length(qp)){#n<-1
          for(o in 1:length(rand)){#o<-1
            for(q in Nfac){#q<-3

              cond <- paste0("N",Nt[k],"_prop",l,"_rel",m,"_qp",n,"_rand",o,"_fac",q)

              for(model in c('dyn','cfa','stat')){

                file.now <- paste0("./simulations/resultssim/",cond,"_r",r,"_",model,".rdata")
                if(file.exists(file.now)){
                  load(file = file.now)

                  est <- add_sum_names(as.data.frame(est))
                  estl <- subset_summary(est, 'lySt',c("i"))



                  ord <- data.frame()
                  for(j in 1:nrow(datr)){
                    ord <- rbind(ord,
                                 data.frame(i=as.numeric(datr[j,1:60+60]),t=1:60,j=j)
                    )
                  }

                  # if same order, add t (item number) to loads
                  if(o==2){
                    estl <- estl%>%left_join(ord%>%filter(j==1)%>%select(i,t))
                  }else if(o==1){
                    estl$t <- NA
                  }

                  k.now = k
                  df.person <- rbind(df.person,datr%>%
                                       select(bad,when,cnr)%>%
                                       mutate(
                                         j=1:n(),
                                         r = r,
                                         nqp = qp[n],
                                         n = Nt[k.now],
                                         prop = prop[l],
                                         rel = rel[m],
                                         o = rand[o],
                                         q = q,
                                         model = model)
                  )


                  df <- rbind(df,estl%>%mutate(
                    r = r,
                    nqp = qp[n],
                    n = Nt[k.now],
                    prop = prop[l],
                    rel = rel[m],
                    o = rand[o],
                    q = q,
                    model = model
                  ))


                  if(grepl("dyn",model)){
                    estC <- subset_summary(est,"C",c("j","i"))%>%
                      mutate(mean=mean-1)%>%
                      left_join(ord)

                    df.s <- rbind(df.s,estC%>%mutate(
                      r = r,
                      nqp = qp[n],
                      n = Nt[k.now],
                      prop = prop[l],
                      rel = rel[m],
                      o = rand[o],
                      q = q,
                      model = model
                    ))
                  }
                } else {
                  message("not found ",file.now)
                }
              }}}}}}}}

  stop("loading finished")


# saveRDS(df,"simulations/resultssim/loads.rds")
# saveRDS(df.person,"simulations/resultssim/persons.rds")
# saveRDS(df.s,"simulations/resultssim/states.rds")


# analysis ----------------------------------------------------------------

library(tidyverse)
library(latex2exp)
library(ReplicationIdentifyingDynamic)
theme_set(theme_bw())

df <- readRDS("simulations/resultssim/loads.rds")
df.person <- readRDS("simulations/resultssim/persons.rds")
df.s <- readRDS("simulations/resultssim/states.rds")




# + mixing ------------------------------------------------------------------


df %>%
  filter(o==1)%>%
  group_by(r,n,prop,rel,nqp,q,model)%>%
  summarise(maxRhat = max(Rhat))%>%
  group_by(prop,rel,nqp,q,model)%>%
  summarise(converge = 1-mean(maxRhat >1.2))%>%
  mutate(model = factor(model,levels = c('cfa','stat','dyn')))%>%
  ggplot(aes(x=model,y=converge,fill=model))+
  geom_bar(stat = "identity")+
  facet_grid(rel~prop,labeller = as_labeller(c('high' = 'high reliability',
                                                'low' = 'low reliability',
                                                '0.1' = '10%',
                                                '0.2' = '20%',
                                                '0.4' = '40%',
                                                '1' = '100%')))+
  geom_hline(yintercept = 1,alpha=.7)+
  ylab('ratio of converged model runs')
ggsave("./simulations/plots/mixing_ran1.pdf",width = 6.5,height=3)


df %>%
  filter(o==2)%>%
  group_by(r,n,prop,rel,nqp,q,model)%>%
  summarise(maxRhat = max(Rhat))%>%
  group_by(prop,rel,nqp,q,model)%>%
  summarise(converge = 1-mean(maxRhat >1.2))%>%
  mutate(model = factor(model,levels = c('cfa','stat','dyn')))%>%
  ggplot(aes(x=model,y=converge,fill=model))+
  geom_bar(stat = "identity")+
  facet_grid(rel~prop,labeller = as_labeller(c('high' = 'high reliability',
                                                'low' = 'low reliability',
                                                '0.1' = '10%',
                                                '0.2' = '20%',
                                                '0.4' = '40%',
                                                '1' = '100%')))+
  geom_hline(yintercept = 1,alpha=.7)+
  ylab('ratio of converged model runs')
ggsave("./simulations/plots/mixing_ran2.pdf",width = 6.5,height=3)



# loads -------------------------------------------------------------------



df%>%
  group_by(n,prop,o,rel,model)%>%
  summarise(load = mean(mean))%>%
  mutate(load0 = ifelse(rel=='high',0.8,0.5))%>%
  ggplot(aes(x=factor(scales::percent(prop),levels = c('10%','20%','40%','100%')),linetype=factor(n),color=model,y=load,group = interaction(n,model)))+
  geom_hline(aes(yintercept=load0))+
  geom_line()+
  geom_point()+
  facet_grid(o~rel,labeller = as_labeller(c('high' = 'high reliability',
                                            'low' = 'low reliability',
                                            '1'='individual sequence',
                                            '2'='same sequence')))+
  scale_y_continuous(limits=c(0,1),breaks = c(0,.2,.4,.6,.8,1))+
  scale_linetype_discrete(name='sample size')+
  xlab('dropout probability')+
  ylab(TeX('load $\\lambda$'))
ggsave("./simulations/plots/loads_ran2_overp.pdf",width = 6.5,height=4)


df%>%
  filter(o==2)%>%
  group_by(n,prop,rel,t,model)%>%
  summarise(load = mean(mean))%>%
  mutate(load0 = ifelse(rel=='high',0.8,0.5))%>%
  ggplot(aes(x=t,linetype=factor(n),color=model,y=load,group = interaction(n,model)))+
  geom_line()+
  geom_hline(aes(yintercept = load0))+
  facet_grid(rel~prop,labeller = as_labeller(c('0.1' = '10%',
                                               '0.2' = '20%',
                                               '0.4' = '40%',
                                               '1' = '100%',
                                               'high' = 'high reliability',
                                               'low' = 'low reliability')))+
  scale_y_continuous(limits=c(0,1),breaks = c(0,.2,.4,.6,.8,1))+
  scale_linetype_discrete(name='sample size')+
  xlab('item number')+
  ylab(TeX('load $\\lambda$'))
ggsave("./simulations/plots/loads_ran2.pdf",width = 6.5,height=3)



# attention ---------------------------------------------------------------



df.s%>%
  filter(o==1)%>%
  left_join(df.person)%>%
  mutate(trel = ifelse(is.na(when),t-60,t-when),
         cnr = is.na(cnr))%>%
  group_by(prop,rel,cnr,trel)%>%
  summarise(mean = 1-mean(mean))%>%
  ggplot(aes(x=trel,y=mean, color = factor(cnr), group=interaction(prop,rel,cnr)))+
  geom_vline(xintercept = 0,col='red',linetype=2,size=1)+
  geom_hline(yintercept = c(0,1),alpha=.5)+
  geom_line(size=1,alpha=1)+
  facet_grid(rel~prop,labeller = as_labeller(c('high' = 'high reliability',
                                               'low' = 'low reliability',
                                               '0.1' = '10%',
                                               '0.2' = '20%',
                                               '0.4' = '40%',
                                               '1' = '100%')))+
  scale_color_discrete(name='attention')+
  ylab('Prob. of attention')+
  xlab('item number relative to treatment')+
  theme_bw()
ggsave("./simulations/plots/sim_att_ran1.pdf",width = 6.5,height=3)

df.s%>%
  filter(o==2)%>%
  left_join(df.person)%>%
  mutate(trel = ifelse(is.na(when),t-60,t-when),
         cnr = is.na(cnr))%>%
  group_by(prop,rel,cnr,trel)%>%
  summarise(mean = 1-mean(mean))%>%
  ggplot(aes(x=trel,y=mean, color = factor(cnr), group=interaction(prop,rel,cnr)))+
  geom_vline(xintercept = 0,col='red',linetype=2,size=1)+
  geom_hline(yintercept = c(0,1),alpha=.5)+
  geom_line(size=1,alpha=1)+
  facet_grid(rel~prop,labeller = as_labeller(c('high' = 'high reliability',
                                               'low' = 'low reliability',
                                               '0.1' = '10%',
                                               '0.2' = '20%',
                                               '0.4' = '40%',
                                               '1' = '100%')))+
  scale_color_discrete(name='attention')+
  ylab('Prob. of attention')+
  xlab('item number relative to treatment')+
  theme_bw()
ggsave("./simulations/plots/sim_att_ran2.png",width = 6.5,height=3)



