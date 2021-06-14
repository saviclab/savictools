# ---------------------------------------------------------------------
# Clinical relevance plot using ggplot2
# Full covariate models
# ---------------------------------------------------------------------
#library(MIfuns)
library(lattice)
library(MASS)
library(reshape)
library(ggplot2)

### MIfun installation
setwd("~/Dropbox/MDR-TB-KOR/Share/MIfuns_5.1.tar/MIfuns/R")
source("accept.R")
source("as.comment.R")
source("as.data.frame.block.R")
source("as.filename.R")
source("as.flag.R")
source("as.keyed.R")
source("as.nm.R")
source("as.nmcontrol.R")
source("as.xml.R")
source("attribute.R")
source("bin.R")
source("bracket.R")
source("check.subjects.R")
source("CLNR.R")
source("colname.R")
source("compute.cwres.R")
source("constant.R")
source("contains.R")
source("covariatePlots.R")
source("css.R")
source("diagnosticPlots.R")
source("diagnosticPlots.R")
source("zzz.R")
source("electronicAppendix.R")
source("first.R")
source("fixedwidth.R")
source("ftable2data.frame.R")
source("glue.R")
source("hash.R")
source("ibw.R")
source("inner.data.frame.R")
source("iterations.R")
source("latest.R")
source("latex.R")
source("locf.R")
source("lookup.R")
source("map.R")
source("metaSub.character.R")
source("miTemporal.R")
source("nest.R")
source("NONR.R")
source("omegacor.R")
source("ops.keyed.R")
source("panel.densitystrip.R")
source("panel.hist.R")
source("panel.stratify.R")
source("params.R")
source("parens.R")
source("partab.R")
source("plot.nm.R")
source("PLOTR.R")
source("prev.R")
source("read.cwres.data.R")
source("reapply.R")
source("resample.data.frame.R")
source("rig.R")
source("rlog.R")
source("runCommand.R")
source("runlog.R")
source("runNonmem.R")
source("safeQuote.R")
source("shuffle.R")
source("simpar.R")
source("snap.R")
source("stableMerge.R")
source("svnProp.R")
source("Tmax.R")
source("variants.R")
source("wikimath.R")
source("xyplot.ext.R")

######## Working directory ###############
setwd("~/Documents/R/CR plot")

######## Example: run4012 information 
######## Structural parameter: BA, MT(MTT), SL(Slope), PO (Power coefficient) #######
######## Covariate: AGE, AAG as continuous #############
######## Covariate: SEX, PC (Post-chemotherapy), PERF (Performance) as categorical #######
######## Covariate: SEX, PC (Post-chemotherapy), PERF (Performance) as categorical #######
######## parameter-covariate relationship: exponentioal equation (e.g.,  Par_ind = Par_pop*exp(individual information - median covariates)) #######
######## Load *.cov file for covariance information ###############
######## Load *.est file for final estimation parameters ###############
varCov <- read.table("run4012.cov",skip=1,as.is=T,header=T)
theta.cov <- varCov[1:25,c(2:26)]
est.theta <- read.table("run4012.ext",skip=1,as.i=T,header=T)
est.theta <- est.theta[12,,drop=FALSE]
head(est.theta) ##select value of iteration -1000000000
est.theta <- est.theta[,-1]
est.theta <- est.theta[,c(1:25)]

######## BA-Replace the mu vector with the results of full model (#Thetas)
boot <- mvrnorm(n=1000, mu=t(est.theta),Sigma=theta.cov)
boot <- data.frame(boot[,c(1:4,6:25)])
names(boot) <- c("BA","MT","SL","PO",
                 "BAAGE","BASEX","BAAAG","BAPC","BAPERF",
                 "MTAGE","MTSEX","MTAAG","MTPC","MTPERF",
                 "SLAGE","SLSEX","SLAAG","SLPC","SLPERF",
                 "POAGE","POSEX","POAAG","POPC","POPERF")
write.csv(boot,"bootstrap_using_AGE_SEX_AAG_PC_PERF.csv",row.names=FALSE,quote=FALSE)

######## Read boot file ###############
boot <- read.csv("bootstrap_using_AGE_SEX_AAG_PC_PERF.csv",head=TRUE)

#boot <- within(boot, BA <- BA/median(BA))
######## Move distribution based on 5th and 95th percentile of each covariates ###############
boot <- within(boot, BAAAG_5 <- exp(BAAAG*(0.533-1.34)))
boot <- within(boot, BAAAG_95 <- exp(BAAAG*(2.353-1.34)))
boot <- within(boot, BAAGE_5 <- exp(BAAGE*(38.5-56)))
boot <- within(boot, BAAGE_95 <- exp(BAAGE*(72.4-56)))
boot <- within(boot, BAPERF_0 <- exp(BAPERF*(0-0.67)))
boot <- within(boot, BAPERF_1 <- exp(BAPERF*(1-0.67)))
boot <- within(boot, BAPC_0 <- exp(BAPC*(0-0.45)))
boot <- within(boot, BAPC_1 <- exp(BAPC*(1-0.45)))
boot <- within(boot, BASEX_1 <- exp(BASEX*(1-1.58)))
boot <- within(boot, BASEX_2 <- exp(BASEX*(2-1.58)))

boot$BA <- NULL
boot$MT <- NULL
boot$SL <- NULL
boot$PO <- NULL

boot$BAAGE <- NULL
boot$BASEX <- NULL
boot$BAPC <- NULL
boot$BAPERF <- NULL
boot$BAAAG <- NULL

boot$MTAGE <- NULL
boot$MTSEX <- NULL
boot$MTPC <- NULL
boot$MTPERF <- NULL
boot$MTAAG <- NULL

boot$SLAGE <- NULL
boot$SLSEX <- NULL
boot$SLPC <- NULL
boot$SLPERF <- NULL
boot$SLAAG <- NULL

boot$POAGE <- NULL
boot$POSEX <- NULL
boot$POPC <- NULL
boot$POPERF <- NULL
boot$POAAG <- NULL


#boot$BAAGE_5 <- NULL
#boot$BAAGE_95 <- NULL

#boot$BAPERF_0 <- NULL
#boot$BAPERF_1 <- NULL
#boot$BAPC_0 <- NULL
#boot$BAPC_1 <- NULL
#boot$BASEX_1 <- NULL
#boot$BASEX_2 <- NULL

boot <- melt(rev(boot))

######## Add group to facet when we draw the figure ###############
boot$group <- c(1)
boot[boot$variable == "BAAAG_5","group"] <- c(1)
boot[boot$variable == "BAAAG_95","group"] <- c(1)
boot[boot$variable == "BAAGE_5","group"] <- c(2)
boot[boot$variable == "BAAGE_95","group"] <- c(2)
boot[boot$variable == "BAPERF_0","group"] <- c(3)
boot[boot$variable == "BAPERF_1","group"] <- c(3)
boot[boot$variable == "BAPC_0","group"] <- c(4)
boot[boot$variable == "BAPC_1","group"] <- c(4)
boot[boot$variable == "BASEX_1","group"] <- c(5)
boot[boot$variable == "BASEX_2","group"] <- c(5)

boot <- boot[
  with(
    boot,
    value >= reapply(value,variable,quantile,0.05) &
      value <= reapply(value,variable,quantile,0.95)
  ),
  ]

######## Group lable for plot label ###############
group_names <- list(
  '1'="BA-AAG",
  '2'="BA-AGE",
  '3'="BA-PERF",
  '4'="BA-PC",
  '5'="BA-SEX")

group_labeller <- function(variable,value){
  return(group_names[value])
}

######## Generate median line and add group  ###############
mu <- boot %>% group_by(variable) %>% summarize(grp.mean = mean(value))
mu[mu$variable == "BAAAG_5","group"] <- c(1)
mu[mu$variable == "BAAAG_95","group"] <- c(1)
mu[mu$variable == "BAAGE_5","group"] <- c(2)
mu[mu$variable == "BAAGE_95","group"] <- c(2)
mu[mu$variable == "BAPERF_0","group"] <- c(3)
mu[mu$variable == "BAPERF_1","group"] <- c(3)
mu[mu$variable == "BAPC_0","group"] <- c(4)
mu[mu$variable == "BAPC_1","group"] <- c(4)
mu[mu$variable == "BASEX_1","group"] <- c(5)
mu[mu$variable == "BASEX_2","group"] <- c(5)

######## Plot using ggplot  ###############
ggplot(boot, aes(x=value, fill=variable, color=variable)) +
  geom_density(alpha=0.1, adjust = 5) + 
  facet_grid(as.factor(group) ~ .,labeller=group_labeller) +
  geom_vline(data=mu, aes(xintercept=grp.mean, colour = variable), linetype ="dashed", alpha = 1) + #Draw median line
  geom_hline(yintercept=0, colour="white", size=1) +                                                #Remove baseline of density plot
  annotate("rect", xmin=0.8, xmax=1.2, ymin=0, ymax=Inf, alpha=0.05, fill="blue") +                 #Shade the range 0.8~1.2
  geom_jitter(data = boot, aes(x=value, y=0), alpha = 0.02, height = 1) +                           #Make dots of value
  ylab("Density plot") +
  xlab("Clinical relevance plot") +
  theme(legend.title = element_blank())
