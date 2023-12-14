### preparation
library(ReplicationIdentifyingDynamic)

library(DynExpData)

library(tidyverse)
library(kableExtra)


theme_set(theme_bw())

add_sum_names <- function(res){
  res$sum$name <- gsub("\\[.*","",rownames(res$sum))
  res$sum$variable <- rownames(res$sum)
  return(res)
  }


# load data ---------------------------------------------------------------------


res.dyn <- readRDS("./analysis/save/06_07_22dyn4.RDS")
res.stat <-  readRDS("./analysis/save/13_07_22static4beta.RDS")
res.cfa <-  readRDS("./analysis/save/final_fits/CFA_full_loglik.RDS")
res.cfa$sum <- as.data.frame(res.cfa$fit)



# estimates ----------------------------------------------------------------


# + read data ------------------------------------------------------------------

df.p.switch <- df.var <- df.cor <- df.call<- df.beta <-df.breturn_i <-df.beta_i <- df.load <- df.p0 <- df.s <- df.s.static <- df.rest <- data.frame()
res.dyn <- add_sum_names(res.dyn)
res.cfa <- add_sum_names(res.cfa)
res.stat <- add_sum_names(res.stat)
df.p0.cur <-subset_summary(res.dyn$sum,"p0",c("k"))%>%mutate(model = 'dyn')%>%filter(k==1)
df.p0 <- rbind(df.p0.cur,df.p0)

dat <- pull_data(choice.id = rownames(res.dyn$input$y),standardize = F)
df <- gen_df(dat)

df.s.cur <- subset_summary(res.dyn$sum,"C",c("j","i"))%>%
  mutate(mean=mean-1)%>%
  left_join(df)
df.s <- rbind(df.s.cur,df.s)%>%
  left_join(df.instr%>%select(id,confidence))

df.s.static.cur <- subset_summary(res.stat$sum,"C",c("j"))%>%
  mutate(mean=mean-1)%>%
  left_join(df%>%select(j,id,treatment.timing,instr,cond)%>%unique())
df.s.static <- rbind(df.s.static.cur,df.s.static)

   df.beta.cur <-subset_summary(res.dyn$sum,"b0",c("k"))%>%mutate(model = 'dyn')%>%rownames_to_column('coef')
  df.beta <- rbind(df.beta.cur,df.beta)
  df.beta.cur <-subset_summary(res.stat$sum,"b0",c("k"))%>%mutate(model = 'stat')%>%rownames_to_column('coef')
  df.beta.all <- rbind(df.beta.cur,df.beta)

  df.load.cur <-subset_summary(res.dyn$sum,"lySt",c("k"))%>%mutate(model = 'dyn')
  df.load <- rbind(df.load.cur,df.load)
  df.load.cur <-subset_summary(res.stat$sum,"lySt",c("k"))%>%mutate(model = 'stat')
  df.load <- rbind(df.load.cur,df.load)
  df.load.cur <-subset_summary(res.cfa$sum,"lySt",c("k"))%>%mutate(model = 'cfa')
  df.load <- plyr::rbind.fill(df.load.cur,df.load)

  df.cor.cur <-subset_summary(res.dyn$sum,"Fcor",c("k1","k2"))%>%mutate(model = 'dyn')
  df.cor <- rbind(df.cor.cur,df.cor)
  df.cor.cur <-subset_summary(res.stat$sum,"Fcor",c("k1","k2"))%>%mutate(model = 'stat')
  df.cor <- rbind(df.cor.cur,df.cor)
  df.cor.cur <-subset_summary(res.cfa$sum,"Fcor",c("k1","k2"))%>%mutate(model = 'cfa')
  df.cor <- plyr::rbind.fill(df.cor.cur,df.cor)

  df.var.cur <-subset_summary(res.dyn$sum,"sigmaxi",c("k1","k2"))%>%mutate(model = 'dyn')
  df.var <- rbind(df.var.cur,df.var)
  df.var.cur <-subset_summary(res.stat$sum,"sigmaxi",c("k1","k2"))%>%mutate(model = 'stat')
  df.var <- rbind(df.var.cur,df.var)
  df.var.cur <-subset_summary(res.cfa$sum,"sigmaxi",c("k1","k2"))%>%mutate(model = 'cfa')
  df.var <- plyr::rbind.fill(df.var.cur,df.var)



  df.call <- subset_summary(res.dyn$sum,"Call",c("j"))%>%mutate(model = 'dyn',)%>%
    left_join(df%>%select(j,id,treatment.timing,instr,cond)%>%unique())
  df.cratio <- subset_summary(res.dyn$sum,"Cratio",c("j"))%>%mutate(model = 'dyn')%>%
    left_join(df%>%select(j,id,treatment.timing,instr,cond)%>%unique())


# + states ----------------------------------------------------------------

# original plot
set.seed(42)
df.s%>%
  filter(#id %in% sample(df.s%>%filter(confidence==1)%>%pull(id),3) |
         id %in% c(sample(df.s%>%filter(cond %in% c(1))%>%pull(id),1),
                   sample(df.s%>%filter(cond %in% c(2))%>%pull(id),1),
                   sample(df.s%>%filter(cond %in% c(3))%>%pull(id),1),
                   sample(df.s%>%filter(cond %in% c(4))%>%pull(id),1)))%>%
  mutate(cond = paste0("Individual ",as.character(as.numeric(cond))))%>%
  ggplot(aes(x=t,y=1-mean))+
  geom_line()+
  # geom_line(aes(y=timerank),color="blue",alpha=.5)+
  # geom_point(aes(y=value.org/7),alpha=.3)+
  geom_vline(aes(xintercept=treatment.timing),color="red")+
  # geom_hline(aes(yintercept=absscore/max(absscore)),color="blue")+
  facet_wrap(vars(cond),nrow=1)+
  ylab("Probability of Non-C/IER")+
  xlab("Item number")
ggsave("./analysis/plots/states_example2.pdf",width = 6.5,height=2)


# dynamic plot 2x2
p.states2x2 <- df.s %>%
  mutate(treatment.timing = ifelse(is.na(treatment.timing),100,treatment.timing),
       t.transform=t-treatment.timing)%>%
  group_by(t.transform,cond)%>%
  mutate(
    allmean = mean(mean)
  )%>%
  ggplot(aes(x=t.transform,y=1-mean,group=id))+
  geom_line(alpha=.05,size=.25)+
  geom_vline(xintercept = 0,color="red",linetype=2,size=.75)+
  geom_line(aes(y=1-allmean),color="black",linewidth=.75)+
  facet_wrap(vars(cond),scales = "free_x",nrow = 2,labeller = as_labeller(function(x) paste0('Condition ',as.character(x) )))+
  ylab("Probability of Non-C/IER")+xlab("Item number relative to treatment")+
  theme(panel.spacing.x = unit(4, "mm"))
p.states2x2
ggsave("./analysis/plots/statesC.pdf",width = 6.5,height=6.5)

# ++ sensivity ---------------------------------------------------------------------
library(caret)

confusionMatrix(factor(df.call$mean<0.5), factor(df.call$cond %in% c(3,4)))

sensitivity(factor(df.call$mean<0.5), factor(df.call$cond %in% c(3,4)))
specificity(factor(df.call$mean<0.5), factor(df.call$cond %in% c(3,4)))

addmargins(table(df.call$mean<0.5, df.call$cond %in% c(3,4),dnn = c("Prob < 0.5","Treatment")))


# + roc ---------------------------------------------------------------------

# load static probabilities
df.s.static <- subset_summary(res.stat$sum,"C",c("j"))%>%
  mutate(mean=mean-1)%>%
  left_join(df%>%select(j,id,treatment.timing,instr,cond)%>%unique())

# load bullshit items results
bs.pred <- bs.long%>%
  group_by(id)%>%
  summarise(inattention = mean(response!=1))

# join with dynamic states
df.pred <- df.s%>%
  mutate(treatment.timing= ifelse(is.na(treatment.timing),101,treatment.timing)) %>%
  left_join(bs.pred)

#static
df.static.temp <- df %>%
  mutate(treatment.timing = ifelse(is.na(treatment.timing),101,treatment.timing))%>%
  left_join(df.s.static%>%select(mean,j))


### multiple hurlde approach
# All the data
d <- pull_data(choice.cond = c(1,2,3,4),standardize = F)
ymat <- d$ymat
cd <- d$cd

# Select those who don't fail BS items
filtered_data <- dplyr::filter(DynExpData::bs, bs1 == 1, bs2 == 1)
NotBS <- dplyr::pull(filtered_data, id)

# Sort CD matrix
cdX <- list()
for(i in 1:nrow(d$ymat)){
  cdX[[i]] <-  as.numeric(d$cd[i,d$iord[i,]])
}
cd_sorted <- as.data.frame(do.call("rbind",cdX))

# Create cuttoff for CD
cut <- qchisq(p = 0.05,df = mean(cd_sorted$V100),lower.tail = F)

# Select those who pass both:
# First CD cutoff
ymat2 <- ymat[which(cd_sorted$V100 < cut),]

# Then BS
ymatKeep <- ymat2[row.names(ymat2) %in% NotBS,]

# Row names of participants who passed both "Hurdles" (Flagged NOT CNR)
df.hurdle <- df.pred |> select(id,treatment.timing,t) |>
  left_join(data.frame(id=row.names(ymatKeep), pass.hurdles = TRUE)) |>
  mutate(pass.hurdles = ifelse(is.na(pass.hurdles), FALSE,TRUE))





### CD function based
# get all data
datall <- pull_data(choice.cond = 1:4)

# get cutoffs
cutoff.data <- readRDS("./analysis/save/final_fits/CFA_cutoffmod_14_12_22.RDS")
cutoff <- cutoff.data$cd_cut
ids.cutoff<- cutoff.data$input$y |> rownames()


df.cd <- df %>%
  mutate(treatment.timing = ifelse(is.na(treatment.timing),101,treatment.timing))%>%
  left_join(
    as.data.frame(datall$cd)%>%rownames_to_column("id")%>%
      pivot_longer(cols = -c(id))%>%
      rename(iname=name,cd=value)%>%
      left_join(df%>%select(id,iname,t))%>%
      filter(t==max(t))%>%
      select(id,cd))



### roc with ggplot
roc.list <- list(pROC::roc(df.pred$t > df.pred$treatment.timing,df.pred$mean,direction="<"),
                 pROC::roc(df.static.temp$t > df.static.temp$treatment.timing,df.static.temp$mean,direction="<"),
                 pROC::roc(df.pred$t > df.pred$treatment.timing,as.numeric(df.pred$inattention>0),direction="<"),
                 pROC::roc(df.hurdle$t > df.hurdle$treatment.timing,as.numeric(!df.hurdle$pass.hurdles),direction="<"),
                  pROC::roc(df.cd$t > df.cd$treatment.timing,as.numeric(!df.cd$id %in% ids.cutoff),direction="<")
                 )


data.auc <- data.frame(AUC=roc.list %>%
  map(~tibble(AUC = .x$auc)) %>% unlist(),
  name = 1:5)


# generate labels labels
data.auc %>%
  mutate(model=case_when(name==1~'Dynamic Model',
                         name==2~'Static Model',
                         name==4~'Multiple Hurdle Approach',
                         name==3~'Bogus Items Approach',
                         name==5~'Person-fit Cutoff Approach'
                         ),
         label_long=paste0(model," , AUC = ",paste(round(AUC,2))),
         label_AUC=paste0("AUC = ",paste(round(AUC,2)))) -> data.labels

# plot on a single plot with AUC in labels
p.rocI <- pROC::ggroc(roc.list) +
  scale_color_discrete(labels=data.labels$label_long)+
  geom_abline(aes(slope = 1, intercept=1), color="darkgrey",linetype=2)+
  theme(legend.position = c(0.65, 0.2),
        legend.title = element_blank(),
        legend.text=element_text(size=9),
        legend.background = element_blank())+
  xlab("Specificity")+
  ylab("Sensitivity")
p.rocI
ggsave("./analysis/plots/ggrocA5.pdf",width = 6.5,height=6.5)


