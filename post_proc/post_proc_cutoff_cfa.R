
library(ggplot2)
library(cnrmturk)
library(verification)
library(usethis)
library(rjags)
library(loo)


# Import fits
# # # # # # # # # # # #
path <-  "./analysis/save/final_fits/"
list.files(path)

# CFA cutoff model
CFC <- readRDS("./analysis/save/final_fits/CFA_cutoffmod_14_12_22.RDS")

# BS model
BS <- readRDS("./analysis/save/final_fits/CFA_BS_fit.RDS")

# CFA model
CFA <- readRDS("./analysis/save/final_fits/CFA_full_loglik.RDS")

# Adding dynamic model
DYN <- readRDS("./analysis/save/06_07_22dyn4.RDS")

# Static model
STA <- readRDS("./analysis/save/13_07_22static4beta.RDS")

# Multiple Hurdle model
MH <- readRDS("./analysis/save/final_fits/CFA_MH_08_23_23.RDS")

# Indexes and name lists
# # # # # # # # # # # #

# Factor correlation/covariance index
xx <- expand.grid(1:10,1:10)
index2 <- xx[ xx[,1] != xx[,2],]

# Factor Names
Fnames <- c("ANX","ANG","FRE","GREG","IMG","ART","TRST","ALT","ORD","SED")

yy <- expand.grid(Fnames,Fnames)
index2n <- yy[ yy[,1] != yy[,2],]

# Item names
# Using factor here because of the item10 before item2 issue
Inames <- factor(colnames(STA$input$y),
                 levels = colnames(STA$input$y))
index <- 1:100

# Factor Loadings
# # # # # # # # # # #
# Tidy Loadings plot DF for plots

Namez <- names(CFC$fit[paste0("lySt[",index,"]"),"mean"])

n = length(CFC$fit[paste0("lySt[",index,"]"),"mean"])
CFCLY <- data.frame(Inames,
                    LYS = CFC$fit[paste0("lySt[",index,"]"),"mean"],
                    SD = CFC$fit[paste0("lySt[",index,"]"),"sd"],
                    LYS5 = CFC$fit[paste0("lySt[",index,"]"),"2.5%"],
                    LYS9 = CFC$fit[paste0("lySt[",index,"]"),"97.5%"],
                    LYS_Rhat = CFC$fit[paste0("lySt[",index,"]"),"Rhat"],
                    LYS_ESS = CFC$fit[paste0("lySt[",index,"]"),"n.eff"],
                    Inames,
                    rep("Cutoff",n),
                    rep("Cutoff",n))
colnames(CFCLY) <- c("Item","Ly","SD",
                   "p2.5","p97.5","Rhat",
                   "ESS","Iname","CDcut","percentile")


# Make Dynamic Dataframe

Dlyz <- data.frame(Namez,
                   DYN$sum[paste0("lySt[",index,"]"),"mean"],
                   DYN$sum[paste0("lySt[",index,"]"),"sd"],
                   DYN$sum[paste0("lySt[",index,"]"),"2.5%"],
                   DYN$sum[paste0("lySt[",index,"]"),"97.5%"],
                   DYN$sum[paste0("lySt[",index,"]"),"Rhat"],
                   DYN$sum[paste0("lySt[",index,"]"),"n.eff"],
                   Inames,
                   rep(200,100),
                   rep("DYN",100))

colnames(Dlyz) <- c("Item","Ly","SD",
                     "p2.5","p97.5","Rhat",
                     "ESS","Iname","CDcut","percentile")

# Static model
Slyz <- data.frame(Namez,
                    STA$sum[paste0("lySt[",index,"]"),"mean"],
                    STA$sum[paste0("lySt[",index,"]"),"sd"],
                    STA$sum[paste0("lySt[",index,"]"),"2.5%"],
                    STA$sum[paste0("lySt[",index,"]"),"97.5%"],
                    STA$sum[paste0("lySt[",index,"]"),"Rhat"],
                    STA$sum[paste0("lySt[",index,"]"),"n.eff"],
                    Inames,
                    rep(200,100),
                    rep("STA",100))

colnames(Slyz)<-colnames(Dlyz) <- c("Item",
                                    "Ly",
                                    "SD",
                                    "p2.5",
                                    "p97.5",
                                    "Rhat",
                                    "ESS",
                                    "Iname",
                                    "CDcut",
                                    "percentile")

BSLY <- data.frame(Inames,
                    LYS = BS$fit[paste0("lySt[",index,"]"),"mean"],
                    SD = BS$fit[paste0("lySt[",index,"]"),"sd"],
                    LYS5 = BS$fit[paste0("lySt[",index,"]"),"2.5%"],
                    LYS9 = BS$fit[paste0("lySt[",index,"]"),"97.5%"],
                    LYS_Rhat = BS$fit[paste0("lySt[",index,"]"),"Rhat"],
                    LYS_ESS = BS$fit[paste0("lySt[",index,"]"),"n.eff"],
                    Inames,
                    rep("BS",n),
                    rep("BS",n))

colnames(BSLY) <- c("Item","Ly","SD",
                     "p2.5","p97.5","Rhat",
                     "ESS","Iname","CDcut","percentile")


CFALY <- data.frame(Inames,
                   LYS = CFA$fit[paste0("lySt[",index,"]"),"mean"],
                   SD = CFA$fit[paste0("lySt[",index,"]"),"sd"],
                   LYS5 = CFA$fit[paste0("lySt[",index,"]"),"2.5%"],
                   LYS9 = CFA$fit[paste0("lySt[",index,"]"),"97.5%"],
                   LYS_Rhat = CFA$fit[paste0("lySt[",index,"]"),"Rhat"],
                   LYS_ESS = CFA$fit[paste0("lySt[",index,"]"),"n.eff"],
                   Inames,
                   rep("CFA",n),
                   rep("CFA",n))

colnames(CFALY) <- c("Item","Ly","SD",
                    "p2.5","p97.5","Rhat",
                    "ESS","Iname","CDcut","percentile")

MHALY <- data.frame(Inames,
                    LYS = MH$fit[paste0("lySt[",index,"]"),"mean"],
                    SD = MH$fit[paste0("lySt[",index,"]"),"sd"],
                    LYS5 = MH$fit[paste0("lySt[",index,"]"),"2.5%"],
                    LYS9 = MH$fit[paste0("lySt[",index,"]"),"97.5%"],
                    LYS_Rhat = MH$fit[paste0("lySt[",index,"]"),"Rhat"],
                    LYS_ESS = MH$fit[paste0("lySt[",index,"]"),"n.eff"],
                    Inames,
                    rep("MH",n),
                    rep("MH",n))

colnames(MHALY) <- c("Item","Ly","SD",
                     "p2.5","p97.5","Rhat",
                     "ESS","Iname","CDcut","percentile")






Pdat <- rbind(Slyz,Dlyz,CFALY,BSLY,CFCLY,MHALY)





# Average Load, Rhat, and ESS

Range <-function(x){ return(range(x)[2] - range(x)[1])}

tab1 <- data.frame(
aggregate(x = Pdat$Ly,list(Pdat$percentile),mean)$Group.1,
paste(
round(aggregate(x = Pdat$Ly,list(Pdat$percentile),mean)$x,2),
paste0("(",as.character(round(aggregate(x = Pdat$Ly,list(Pdat$percentile),sd)$x,2)),")")
),
paste(
  round(aggregate(x = Pdat$p2.5,list(Pdat$percentile),mean)$x,2),
  paste0("(",as.character(round(aggregate(x = Pdat$p2.5,list(Pdat$percentile),sd)$x,2)),")")
),
paste(
  round(aggregate(x = Pdat$p97.5,list(Pdat$percentile),mean)$x,2),
  paste0("(",as.character(round(aggregate(x = Pdat$p97.5,list(Pdat$percentile),sd)$x,2)),")")
),
paste(
  round(aggregate(x = Pdat$Rhat,list(Pdat$percentile),mean)$x,3),
  paste0("(",as.character(round(aggregate(x = Pdat$Rhat,list(Pdat$percentile),Range)$x,2)),")")
),
paste(
  round(aggregate(x = Pdat$ESS,list(Pdat$percentile),mean)$x,0),
  paste0("(",as.character(round(aggregate(x = Pdat$ESS,list(Pdat$percentile),Range)$x,0)),")")
))

library(kableExtra)
library(ggplot2)

colnames(tab1) <-  c("Model",
                     "Mean LY (SD)",
                     "Mean 2.5% (SD)",
                     "Mean 97.5% (SD)",
                     "Mean Rhat (Range)",
                     "Mean ESS (Range)")

# Changing to range for ESS and Rhat

ggplot() +
  geom_histogram(aes(x=Pdat$Rhat))

# Average loading and diagnostics by model
kable(x = tab1,
      format = "latex",
      booktabs = TRUE,
      align = c("l","c","c","c","c","c"))


library(reshape2)

#
# Loading plot for paper
# # # # # # #


Pdat2 <- data.frame("Ly.Dyn" = Pdat$Ly[Pdat$percentile == "DYN"],
                   "Ly.Sta" = Pdat$Ly[Pdat$percentile == "STA"],
                   "Ly.Bogus" = Pdat$Ly[Pdat$percentile == "BS"],
                   "Ly.CFA" = Pdat$Ly[Pdat$percentile == "CFA"],
                   "Ly.Cut" = Pdat$Ly[Pdat$percentile == "Cutoff"],
                   "Ly.MH" = Pdat$Ly[Pdat$percentile == "MH"])

p1 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Sta,y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0.25,y = 0.25, yend = 1, xend = 1))+ theme_bw() +
  xlab("Static Model Loadings") + ylab("Dynamic Model Loadings")

p2 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.CFA,y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0.25,y = 0.25, yend = 1, xend = 1))+ theme_bw() +
  xlab("CFA Model Loadings") + ylab("Dynamic Model Loadings")

p3 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Bogus, y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0.25,y = 0.25, yend = 1, xend = 1))+ theme_bw() +
  xlab("Bogus Strategy Loadings") + ylab("Dynamic Model Loadings")

p4 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Cut, y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0.25,y = 0.25, yend = 1, xend = 1))+ theme_bw() +
  xlab("Cutoff Strategy Loadings") + ylab("Dynamic Model Loadings")

p5 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Cut, y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0,y = 0, yend = 1, xend = 1))+ theme_bw() +
  xlab("Multiple Hurdle Strategy Loadings") + ylab("Dynamic Model Loadings")


pc <- gridExtra::grid.arrange(p1,p2,p3,p4,p5, nrow = 3)

ggsave("./analysis/full/plots/load_plot.pdf",pc,width = 6,height = 8,units = "in")

# Again for SDS
Pdat2 <- data.frame("Ly.Dyn" = Pdat$SD[Pdat$percentile == "DYN"],
                    "Ly.Sta" = Pdat$SD[Pdat$percentile == "STA"],
                    "Ly.Bogus" = Pdat$SD[Pdat$percentile == "BS"],
                    "Ly.CFA" = Pdat$SD[Pdat$percentile == "CFA"],
                    "Ly.Cut" = Pdat$SD[Pdat$percentile == "Cutoff"],
                    "Ly.MH" = Pdat$SD[Pdat$percentile == "MH"])

p1 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Sta,y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0,y = 0, yend = 0.1, xend = 0.1))+ theme_bw() +
  xlab("Static Model Loading SD") + ylab("Dynamic Model Loadings SD")

p2 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.CFA,y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0,y = 0, yend = 0.1, xend = 0.1))+ theme_bw() +
  xlab("CFA Model Loadings SD") + ylab("Dynamic Model Loadings SD")

p3 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Bogus, y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0,y = 0, yend = 0.1, xend = 0.1))+ theme_bw() +
  xlab("Bogus Strategy Loadings SD") + ylab("Dynamic Model Loadings SD")

p4 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Cut, y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0,y = 0, yend = 0.1, xend = 0.1))+ theme_bw() +
  xlab("Cutoff Strategy Loadings SD") + ylab("Dynamic Model Loadings SD")

p5 <- ggplot(data = Pdat2) +
  geom_point(aes(x = Ly.Cut, y = Ly.Dyn), alpha = 0.45) +
  geom_segment(aes(x = 0,y = 0, yend = 0.1, xend = 0.1))+ theme_bw() +
  xlab("Multiple Hurdle Strategy Loadings SD") + ylab("Dynamic Model Loadings SD")


pc <- gridExtra::grid.arrange(p1,p2,p3,p4,p5,nrow = 3)

ggsave("./analysis/full/plots/load_SD_plot.pdf",pc,width = 6,height = 8,units = "in")

