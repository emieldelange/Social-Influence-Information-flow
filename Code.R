### Linear modelling of raw data ###

library(car)
library(lme4)
library(ggplot2)
library(tidyverse)

totaldata <- read.csv("Raw behavior data for linear modelling.csv")

totaldata <- as_tibble(totaldata, rownames = "ID") %>%
                pivot_longer(cols = matches("attitudes|control|dnorms|innorms|intention|Knowledge|pledge|story|hotline"),
                names_to = "names", values_to = "values") %>%
                separate(col = names, into = c("period", "variable"), sep = "\\.",
                fill = "left") %>%    
                pivot_wider(names_from = variable, values_from = values) %>%
                mutate(period = factor(period, levels = c("base", "follow", "final")))

#normalise some variables
totaldata$agemod <- totaldata$prelim.age/sd(totaldata$prelim.age, na.rm=TRUE)
totaldata$knowledgemod <- totaldata$Knowledge/sd(totaldata$Knowledge, na.rm=TRUE)

#linear models
##### Intention lmer
lmintention1 <- lmer(intention ~ period * follow.event_attendance + prelim.gender +agemod +
                       hh.SMP + hh.wealth1 +Base+ hh.Pesticide + (1|ID), data=totaldata)
summary(lmintention1)

linearHypothesis(lmintention1, "periodfollow+periodfollow:follow.event_attendance=0")
linearHypothesis(lmintention1, "periodfinal+periodfinal:follow.event_attendance=0")

lmintention2 <- lmer(intention ~ period + knowledgemod + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmintention2)
lmintention3 <- lmer(intention ~ period + pledge + story + hotline + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmintention3)

##### attitudes lmer
lmattitudes1 <- lmer(attitudes ~ period * follow.event_attendance + prelim.gender +agemod +
                       hh.SMP + hh.wealth1 +Base+ hh.Pesticide + (1|ID), data=totaldata)
summary(lmattitudes1)

linearHypothesis(lmattitudes1, "periodfollow+periodfollow:follow.event_attendance=0")
linearHypothesis(lmattitudes1, "periodfinal+periodfinal:follow.event_attendance=0")

lmattitudes2 <- lmer(attitudes ~ period + knowledgemod + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmattitudes2)
lmattitudes3 <- lmer(attitudes ~ period + pledge + story + hotline + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmattitudes3)

##### control lmer
lmcontrol1 <- lmer(control ~ period * follow.event_attendance + prelim.gender +agemod +
                       hh.SMP + hh.wealth1 +Base+ hh.Pesticide + (1|ID), data=totaldata)
summary(lmcontrol1)

linearHypothesis(lmcontrol1, "periodfollow+periodfollow:follow.event_attendance=0")
linearHypothesis(lmcontrol1, "periodfinal+periodfinal:follow.event_attendance=0")

lmcontrol2 <- lmer(control ~ period + knowledgemod + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmcontrol2)
lmcontrol3 <- lmer(control ~ period + pledge + story + hotline + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmcontrol3)

##### dnorms lmer
lmdnorms1 <- lmer(dnorms ~ period * follow.event_attendance + prelim.gender +agemod +
                       hh.SMP + hh.wealth1 +Base+ hh.Pesticide + (1|ID), data=totaldata)
summary(lmdnorms1)

linearHypothesis(lmdnorms1, "periodfollow+periodfollow:follow.event_attendance=0")
linearHypothesis(lmdnorms1, "periodfinal+periodfinal:follow.event_attendance=0")

lmdnorms2 <- lmer(dnorms ~ period + knowledgemod + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmdnorms2)
lmdnorms3 <- lmer(dnorms ~ period + pledge + story + hotline + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lmdnorms3)

##### innorms lmer
lminnorms1 <- lmer(innorms ~ period * follow.event_attendance + prelim.gender +agemod +
                       hh.SMP + hh.wealth1 +Base+ hh.Pesticide + (1|ID), data=totaldata)
summary(lminnorms1)

linearHypothesis(lminnorms1, "periodfollow+periodfollow:follow.event_attendance=0")
linearHypothesis(lminnorms1, "periodfinal+periodfinal:follow.event_attendance=0")

lminnorms2 <- lmer(innorms ~ period + knowledgemod + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lminnorms2)
lminnorms3 <- lmer(innorms ~ period + pledge + story + hotline + prelim.gender +agemod + hh.SMP +
                       hh.wealth1 +Base+ hh.Pesticide+ (1|ID), data=totaldata)
summary(lminnorms3)

##### Knowledge lmer
lmknow <- lmer(knowledge ~ period * follow.event_attendance + prelim.gender +agemod + hh.SMP + hh.wealth1+Base+ hh.Pesticide + (1|ID), data=TPBPlots2)
summary(lmknow)

linearHypothesis(lmknow, "periodfollow+periodfollow:follow.event_attendance=0")
linearHypothesis(lmknow, "periodfinal+periodfinal:follow.event_attendance=0")
linearHypothesis(lmknow, "periodfinal+periodfinal:follow.event_attendance=periodfollow+periodfollow:follow.event_attendance")
linearHypothesis(lmknow, "periodfinal=periodfollow")

#Visualising changes
distplots <- TPBPlots2
distplots$intention<-distplots$intention/10
distplots$attitudes<-distplots$attitudes/20
distplots$control<-distplots$control/15
distplots$dnorms<-distplots$dnorms/10
distplots$innorms<-distplots$innorms/20

distplots <- distplots %>%
  as_tibble() %>%
  pivot_longer(cols = matches("intention|attitudes|dnorms|innorms|control"),names_to = "names", values_to = "values")

distplots$names <- as.factor(distplots$names)
distplots$names <- fct_relevel(distplots$names,"intention", "attitudes","control","dnorms","innorms")
levels(distplots$names) <- c("Intention", "Attitudes", "Perceived control", "Perceived descriptive norm", "Perceived injunctive norm")
levels(distplots$period) <- c("Wave 1", "Wave 2", "Wave 3")
ggplot(distplots, aes(x=names, y=values, fill=period))+
  geom_boxplot() +
  labs(x = "TPB Construct", y="Scaled value") +
  theme_grey(base_size=12)


#TPB GLMs
# Period 1
BaselineIntentionGLM <- glm(intention ~ control + attitudes + dnorms + innorms, data=totaldata[which(totaldata$period=="base"),])
summary(BaselineIntentionGLM)
# Period 2
FollowIntentionGLM <- glm(intention ~ control + attitudes + dnorms + innorms, data=totaldata[which(totaldata$period=="base"),])
summary(FollowIntentionGLM)
# Period 3
FinalIntentionGLM <- glm(intention ~ control + attitudes + dnorms + innorms, data=totaldata[which(totaldata$period=="base"),])
summary(FinalIntentionGLM)

TPBGLM <- rbind(as.data.frame(summary(BaselineIntentionGLM)$coefficients[2:5,1:2]),
                as.data.frame(summary(FollowIntentionGLM)$coefficients[2:5,1:2]),
                as.data.frame(summary(FinalIntentionGLM)$coefficients[2:5,1:2]))
TPBGLM$period <- as.factor(c(rep("base",4), rep("follow", 4), rep("final",4)))
TPBGLM$period <- as.factor(TPBGLM$period) 
TPBGLM$SE <- TPBGLM$`Std. Error` 
TPBGLM$CI <- TPBGLM$SE*1.96
TPBGLM$min <- TPBGLM$Estimate-TPBGLM$CI
TPBGLM$max <- TPBGLM$Estimate+TPBGLM$CI

ggplot(TPBGLM, aes(y=variable, x=Estimate, xmin=min, xmax=max))+
  geom_dotplot(binaxis='y',stackdir='center',dotsize=0.4)+
  geom_errorbarh(height=0) +
  facet_grid(.~period) +
  geom_vline(xintercept=0, linetype="dotted")

#############################################################################
######### Imputation using Mice and subsequent linear modelling ##########

library(ggplot2)
library(tidyverse)
library(mice)
library(reshape2)
library(car)
library(carEx)
library(lme4)
library(broom.mixed)

micedata <- read.csv("Raw behavior data for SNA.csv")
D <- 20 #Number of imputations 

#Conduct multiple imputation
miceImp <- mice(micedata, m=D, meth='pmm', seed=503, maxit=20)
plot(miceImp)

#diagnostics
mice::bwplot(miceImp, story2)
densityplot(miceImp)
xyplot(miceImp, hotline1 ~ base.intention, pch=18,cex=1)

#lengthen temporarily to calculate a total knowledge score 
long.data <- complete(miceImp, action="long", include=TRUE)
long.data$follow.Knowledge <- apply(long.data[,24:26], 1, sum)
long.data$final.Knowledge <- apply(long.data[,27:29], 1, sum)
long.data$base.hotline <- 0 #before the intervention all knowledge is 0
long.data$base.story <- 0
long.data$base.pledge <- 0
long.data$base.Knowledge <- 0
wide.data <- as.mids(long.data)

id <- list() # Create a list for storing completed imputed data sets
intention.m1 <- list() # Create a list for storing fitted models
intention.m2 <- list()
attitudes.m1 <- list()
attitudes.m2 <- list()
control.m1 <- list()
control.m2 <- list()
dnorms.m1 <- list()
dnorms.m2 <- list()
innorms.m1 <- list()
innorms.m2 <- list()
knowledge.m <- list()

for(i in 1:D){
  # Complete the data
  id[[i]] <- complete(wide.data, action=i) %>%
    # Reshape
    as_tibble(rownames = "id") %>%
    pivot_longer(cols = matches("attitudes|control|dnorms|innorms|intention|knowledge|pledge|story|hotline"),
                 names_to = "names", values_to = "values") %>%
    separate(col = names, into = c("period", "variable"), sep = "\\.",
             fill = "left") %>%
    pivot_wider(names_from = variable, values_from = values) %>%
    mutate(period = factor(period, levels = c("base", "follow", "final")))
  
  id[[i]]$Knowledgemod <- id[[i]]$Knowledge/sd(id[[i]]$Knowledge)
  
  # Fit models (in this case, a model for intention as a function of event attendance)
  intention.m1[[i]] <- lmer(intention ~ period*follow.event_attendance + prelim.gender +
                              hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  intention.m2[[i]] <- lmer(intention ~ period + pledge + hotline +story + prelim.gender +
                              hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  #attitudes
  attitudes.m1[[i]] <- lmer(attitudes ~ period*follow.event_attendance + prelim.gender +
                              hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  attitudes.m2[[i]] <- lmer(attitudes ~ period + pledge + hotline +story + prelim.gender +
                              hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  #control
  control.m1[[i]] <- lmer(control ~ period*follow.event_attendance + prelim.gender +
                            hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  control.m2[[i]] <- lmer(control ~ period + pledge + hotline +story + prelim.gender +
                            hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  #dnorms
  dnorms.m1[[i]] <- lmer(dnorms ~ period*follow.event_attendance + prelim.gender +
                           hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  dnorms.m2[[i]] <- lmer(dnorms ~ period + pledge + hotline +story + prelim.gender +
                           hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  #innorms
  innorms.m1[[i]] <- lmer(innorms ~ period*follow.event_attendance + prelim.gender +
                            hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  innorms.m2[[i]] <- lmer(innorms ~ period +pledge + hotline +story + prelim.gender +
                            hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide + (1|id), data=id[[i]])
  #knowledge
  knowledge.m[[i]] <- lmer(Knowledge ~ period*follow.event_attendance + prelim.gender +
                             hh.SMP + agemod + hh.wealth1 + Base + hh.Pesticide  + (1|id), data=id[[i]])
  
}

#intention
intention.rep1 <- as.mira(intention.m1) # Convert model list to a mira object so that it works with pool()
intention.pooled1 <- pool(intention.rep1) # Pool results across model list (e.g. pooled effect sizes and variances)
intention.rep2 <- as.mira(intention.m2)
intention.pooled2 <- pool(intention.rep2)

summary(intention.pooled1)
summary(intention.pooled2)

linearHypothesis(intention.rep1, "periodfollow+periodfollow:follow.event_attendance = 0") #Attendees increase short term?
linearHypothesis(intention.rep1, "periodfinal+periodfinal:follow.event_attendance = 0") #Attendees increase long term?
linearHypothesis(intention.rep1, "periodfollow = 0") #Others increase short term?
linearHypothesis(intention.rep1, "periodfinal = 0") #Others increase long term?

#attitudes
attitudes.rep1 <- as.mira(attitudes.m1) # Convert model list to a mira object so that it works with pool()
attitudes.pooled1 <- pool(attitudes.rep1) # Pool results across model list (e.g. pooled effect sizes and variances)
attitudes.rep2 <- as.mira(attitudes.m2)
attitudes.pooled2 <- pool(attitudes.rep2)

summary(attitudes.pooled1)
summary(attitudes.pooled2)

linearHypothesis(attitudes.rep1, "periodfollow+periodfollow:follow.event_attendance = 0") #Attendees increase short term?
linearHypothesis(attitudes.rep1, "periodfinal+periodfinal:follow.event_attendance = 0") #Attendees increase long term?
linearHypothesis(attitudes.rep1, "periodfollow = 0") #Others increase short term?
linearHypothesis(attitudes.rep1, "periodfinal = 0") #Others increase long term?

#control
control.rep1 <- as.mira(control.m1) # Convert model list to a mira object so that it works with pool()
control.pooled1 <- pool(control.rep1) # Pool results across model list (e.g. pooled effect sizes and variances)
control.rep2 <- as.mira(control.m2)
control.pooled2 <- pool(control.rep2)

summary(control.pooled1)
summary(control.pooled2)

linearHypothesis(control.rep1, "periodfollow+periodfollow:follow.event_attendance = 0") #Attendees increase short term?
linearHypothesis(control.rep1, "periodfinal+periodfinal:follow.event_attendance = 0") #Attendees increase long term?
linearHypothesis(control.rep1, "periodfollow = 0") #Others increase short term?
linearHypothesis(control.rep1, "periodfinal = 0") #Others increase long term?

#dnorms
dnorms.rep1 <- as.mira(dnorms.m1) # Convert model list to a mira object so that it works with pool()
dnorms.pooled1 <- pool(dnorms.rep1) # Pool results across model list (e.g. pooled effect sizes and variances)
dnorms.rep2 <- as.mira(dnorms.m2)
dnorms.pooled2 <- pool(dnorms.rep2)

summary(dnorms.pooled1)
summary(dnorms.pooled2)

linearHypothesis(dnorms.rep1, "periodfollow+periodfollow:follow.event_attendance = 0") #Attendees increase short term?
linearHypothesis(dnorms.rep1, "periodfinal+periodfinal:follow.event_attendance = 0") #Attendees increase long term?
linearHypothesis(dnorms.rep1, "periodfollow = 0") #Others increase short term?
linearHypothesis(dnorms.rep1, "periodfinal = 0") #Others increase long term?

#innorms
innorms.rep1 <- as.mira(innorms.m1) # Convert model list to a mira object so that it works with pool()
innorms.pooled1 <- pool(innorms.rep1) # Pool results across model list (e.g. pooled effect sizes and variances)
innorms.rep2 <- as.mira(innorms.m2)
innorms.pooled2 <- pool(innorms.rep2)

summary(innorms.pooled1)
summary(innorms.pooled2)

linearHypothesis(innorms.rep1, "periodfollow+periodfollow:follow.event_attendance = 0") #Attendees increase short term?
linearHypothesis(innorms.rep1, "periodfinal+periodfinal:follow.event_attendance = 0") #Attendees increase long term?
linearHypothesis(innorms.rep1, "periodfollow = 0") #Others increase short term?
linearHypothesis(innorms.rep1, "periodfinal = 0") #Others increase long term?

#knowledge
knowledge.rep <- as.mira(knowledge.m) # Convert model list to a mira object so that it works with pool()
knowledge.pooled <- pool(knowledge.rep) # Pool results across model list (e.g. pooled effect sizes and variances)

summary(knowledge.pooled)

linearHypothesis(knowledge.rep, "periodfollow+periodfollow:follow.event_attendance = 0") #Attendees increase short term?
linearHypothesis(knowledge.rep, "periodfinal+periodfinal:follow.event_attendance = 0") #Attendees increase long term?
linearHypothesis(knowledge.rep, "periodfinal+periodfinal:follow.event_attendance = periodfollow+ periodfollow:follow.event_attendance")
linearHypothesis(knowledge.rep, "periodfinal = periodfollow")

#TPB GLMs
Baseline = list()
Follow = list()
Final = list()

for(i in 1:D){
  glm.data <- complete(wide.data, action=i)
  
  Baseline[[i]] <- glm(base.intention ~ base.control + base.attitudes + base.dnorms + base.innorms, data=glm.data)
  Follow[[i]] <- glm(follow.intention ~ follow.control + follow.attitudes + follow.dnorms + follow.innorms, data=glm.data)
  Final[[i]] <- glm(final.intention ~ final.control + final.attitudes + final.dnorms + final.innorms, data=glm.data)
  
}

base.rep <- as.mira(Baseline) # Convert model list to a mira object so that it works with pool()
follow.rep <- as.mira(Follow)
final.rep <- as.mira(Final)
base.pooled <- pool(base.rep) # Pool results across model list (e.g. pooled effect sizes and variances)
follow.pooled <- pool(follow.rep)
final.pooled <- pool(final.rep)

summary(base.pooled)
summary(follow.pooled)
summary(final.pooled)

saveRDS(wide.data, file="miceImp.rds")


###############################################################################################
############ Preparing to impute and estimate the SAOMs ################################

#This code is adapted from Krause et.al. 
#https://www.stats.ox.ac.uk/~snijders/siena/MultipleImputationNetworkAndBehavior.html#imputing-the-behavior-with-mice

library(RSiena)
library(mice)
library(igraph)

D <- 20 #set number of imputations
N <- 365 #set number of actors

network <- as.matrix(read.csv("Network matrix.csv")[-1,-1])
#For robustness check using the updated network use:
# network <- as.matrix(read.csv("Updated matrix.csv")[-1,-1])

miceImp <- readRDS("miceImp.rds")
networkdata <- read.csv("Raw behavior data for SNA.csv")

#Define some functions
siena07ToConvergence <- function(alg, dat, eff, ans0=NULL, threshold, nodes=3,
                                 cluster = TRUE, n2startPrev = 1000, ...) {
  # parameters are:
  # alg, dat, eff: Arguments for siena07: algorithm, data, effects object.
  # ans0: previous answer, if available; used as prevAns in siena07.
  # threshold: largest satisfactory value
  #            for overall maximum convergence ratio (indicating convergence).
  # nodes: number of processes for parallel processing.
  numr <- 0
  if (is.null(ans0)) {
    ans <- siena07(alg, data = dat, effects = eff, prevAns = ans0,nbrNodes = nodes,
                   returnDeps = TRUE, useCluster = cluster, ...) # the first run
  } else {
    alg$nsub <- 1
    alg$n2start <- n2startPrev
    ans <- siena07(alg, data = dat, effects = eff, prevAns = ans0,nbrNodes = nodes,
                   returnDeps = TRUE, useCluster = cluster, ...)
  }
  repeat {
    #save(ans, file = paste("ans",numr,".RData",sep = "")) # to be safe
    numr <- numr + 1           # count number of repeated runs
    tm <- ans$tconv.max      # convergence indicator
    cat(numr,"tconv  max:", round(tm,3),"\n")       # report how far we are
    if (tm < threshold) {break}   # success
    if (tm > 10) {stop()}     # divergence without much hope
    # of returning to good parameter values
    if (numr > 100) {stop()}  # now it has lasted too long
    alg$nsub <- 1
    alg$n2start <- 1000 + numr * 1000
    alg$n3 <- 2000 + numr * 1000
    ans <- siena07(alg, data = dat,effects = eff,prevAns = ans, nbrNodes = nodes,
                   returnDeps = TRUE, useCluster = cluster, ...)
  }
  if (tm > threshold) {
    stop("Warning: convergence inadequate.\n")
  }
  ans
}

#define some general covariates
Age <- coCovar(networkdata$prelim.age)
Wealth <- coCovar(networkdata$hh.wealth1)
SMP <- coCovar(networkdata$hh.SMP)
pesticide <- coCovar(networkdata$hh.Pesticide)
Gender <- coCovar(networkdata$prelim.gender)
Dummy <-varCovar(cbind(rep(0,365), rep(1,365)))

###################################################################################################
########### Imputation and estimation of the SAOM for intention #################################

network1 <- network 

#####Stationary SAOM
visits <- sienaDependent(array(c(network1, network1), dim = c(N,N, 2)) ,
                         allowOnly = FALSE)
a2 <- coCovar(networkdata$follow.intention) # the 2nd wave incomplete behavior as covariate

stationaryDataList <- list()

for (d in 1:D) {
  intention <- sienaDependent(cbind(complete(miceImp,d)$base.intention,
                                    complete(miceImp,d)$base.intention), 
                              type = "behavior", allowOnly = FALSE)
  
  stationaryDataList[[d]] <- sienaDataCreate(visits,intention,a2, Atd, Age, Wealth, Gender, pesticide, SMP)
}

Data.stationary <- sienaGroupCreate(stationaryDataList)

effects.stationary <- getEffects(Data.stationary)
effects.stationary[effects.stationary$shortName == 'recip',]$include <- FALSE

# 2nd wave as covariate
effects.stationary <- includeEffects(effects.stationary, effFrom, 
                                     name = "intention", interaction1 = "a2")
#beh control
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     indeg, interaction1 = "visits")
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     outdeg, interaction1 = "visits")
# influence
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     avSim, interaction1 = "visits")

#Control
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     effFrom, interaction1 = "SMP")
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     effFrom, interaction1 = "Gender")
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     effFrom, interaction1 = "Wealth")
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     effFrom, interaction1 = "Age")
effects.stationary <- includeEffects(effects.stationary, name = "intention",
                                     effFrom, interaction1 = "pesticide")


for (d in 1:D) { #fix the rate function
  effects.stationary <- setEffect(effects.stationary, Rate, initialValue = 0.01,
                                  name = "visits",fix = TRUE, 
                                  group = d,type = "rate",test = FALSE)
  
  effects.stationary <- setEffect(effects.stationary, Rate, initialValue = 3,
                                  name = "intention",fix = TRUE,
                                  group = d,type = "rate",test = FALSE)
}

estimation.options.st <- sienaAlgorithmCreate(useStdInits = FALSE,
                                              seed = 214,
                                              n3 = 3000, maxlike = FALSE,
                                              cond = FALSE, diagonalize = 0.6,
                                              firstg = 0.02,
                                              behModelType = c(intention = 2),
                                              lessMem = TRUE)
#estimate the SAOM
period0saom <- siena07ToConvergence(alg = estimation.options.st,
                                    dat = Data.stationary, nodes=15,
                                    eff = effects.stationary, threshold = 0.2)
save.image('conmi.RData') 

imputation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 214,
                                           cond = FALSE, 
                                           behModelType = c(intention = 2),
                                           maxlike = TRUE,
                                           nsub = 0,
                                           simOnly = TRUE,
                                           n3 = 10)
set.seed(142)

stationaryImpDataList <- list()

for (d in 1:D) {
  n1 <- network1
  n1 <- n1 + 10
  n1 <- ifelse(n1>11, 11, n1)
  diag(n1) <- 0
  n2 <- n1
  tieList <- c(1:(nrow(n1)**2))[c(n1 == 11)]
  tieList <- tieList[!is.na(tieList)]
  
  changedTie <- sample(tieList,1)
  
  n1[changedTie] <- 0
  n2[changedTie] <- 1
  
  visits <- sienaDependent(array(c(n1,n2), dim = c(N,N, 2)),
                           allowOnly = FALSE )
  
  
  i1 <- networkdata$base.intention
  i1.3s <- c(1:N)[i1 == 8 & !is.na(i1)]
  int <- sample(i1.3s,1)
  i1change <- complete(miceImp,d)$base.intention
  i1change[int] <- sample(c(7,9),1)
  
  intention <- sienaDependent(cbind(i1change,i1), type = "behavior",
                              allowOnly = FALSE)
  
  stationaryImpDataList[[d]] <- sienaDataCreate(visits, intention,a2,Atd, Age,Gender, SMP, pesticide, Wealth)
}

Data.stationary.imp <- sienaGroupCreate(stationaryImpDataList)

#impute first wave
sims <- siena07(imputation.options, data = Data.stationary.imp,
                effects = effects.stationary,
                prevAns = period0saom,
                returnDeps = TRUE)$sims[[10]]


int1imp <- matrix(NA,N,D)

for (d in 1:D) {
  int1imp[,d] = sims[[d]][[1]][[2]]
}
save.image('conmi.RData') 
##########################################################################
################### Imputing Later Waves #################################
##########################################################################
int2imp <- matrix(NA,N,D)
int3imp <- matrix(NA,N,D)


n1 <- network1
diag(n1) <- 0
n2 <- n1
tieList <- c(1:(nrow(n1)**2))[c(n1 == 1)]
tieList <- tieList[!is.na(tieList)]

changedTie <- sample(tieList,1)

n1[changedTie] <- 1
n2[changedTie] <- 0


set.seed(1402)

estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 189,
                                           n3 = 3000, maxlike = FALSE,
                                           cond = FALSE, diagonalize = 0.3,
                                           firstg = 0.02,
                                           behModelType = c(intention = 2),
                                           lessMem = TRUE)


for (d in 1:D) {
  
  cat('imputation',d,'\n')
  
  # now impute wave2
  
  visits <- sienaDependent(array(c(n1,n2),
                                 dim = c(N,N,2)))
  intention <- sienaDependent(cbind(int1imp[,d], networkdata$follow.intention), type = "behavior")
  Know <- coCovar(complete(miceImp, d)$follow.Knowledge/sd(complete(miceImp, d)$follow.Knowledge,na.rm=T))
  a3 <- coCovar(networkdata$final.intention)
  
  Data.w2  <- sienaDataCreate(visits, intention, Age, Wealth, Know, a3, Gender, SMP, pesticide)
  
  effects.twoWaves <- getEffects(Data.w2)
  effects.twoWaves[effects.twoWaves$shortName == 'recip',]$include <- FALSE
  
  #influence
  effects.twoWaves <- includeEffects(effects.twoWaves, avSim, 
                                     name = 'intention',
                                     interaction1 =  "visits")
  
  #Knowledge
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Know")
  
  #Control
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Gender")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Age")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Wealth")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="a3")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",effFrom, interaction1 = "SMP")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",effFrom, interaction1 = "pesticide")
  #beh control
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",
                                     indeg, interaction1 = "visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",
                                     outdeg, interaction1 = "visits")
  
  #fix the rate function 
  effects.twoWaves <- setEffect(effects.twoWaves, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  
  if (d == 1) {
    period1saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w2,nodes=15,
                                        eff = effects.twoWaves,
                                        threshold = 0.2)
  } else {
    period1saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w2, nodes=15,
                                        eff = effects.twoWaves,
                                        threshold = 0.2,
                                        ans0 = period1saom)
  }
  
  sims <- siena07(imputation.options, data = Data.w2,
                  effects = effects.twoWaves,
                  prevAns = period1saom,
                  returnDeps = TRUE)$sims[[10]]
  
  int2imp[,d] <- sims[[2]]
  
  # impute wave 3
  visits <- sienaDependent(array( c(n1,n2),
                                  dim = c(N,N, 2)))
  intention <- sienaDependent(cbind(int2imp[,d],networkdata$final.intention), type = "behavior")
  Know <- coCovar(complete(miceImp, d)$final.Knowledge/sd(complete(miceImp, d)$final.Knowledge,na.rm=T))
  Data.w3  <- sienaDataCreate(visits, intention, Age, Wealth, Atd, Know, Gender, SMP, pesticide)
  
  effects.twoWaves <- getEffects(Data.w3)
  effects.twoWaves[effects.twoWaves$shortName == 'recip',]$include <- FALSE
  
  #influence
  effects.twoWaves <- includeEffects(effects.twoWaves, avSim, 
                                     name = 'intention',
                                     interaction1 =  "visits")
  
  
  #Knowledge
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Know")
  
  #Control
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Gender")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Age")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="intention", interaction1="Wealth")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",effFrom, interaction1 = "SMP")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",effFrom, interaction1 = "pesticide")
  
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",
                                     indeg, interaction1 = "visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "intention",
                                     outdeg, interaction1 = "visits")

  
  #fix the rate function 
  effects.twoWaves <- setEffect(effects.twoWaves, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  
  if (d == 1) {
    period2saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w3,nodes=15,
                                        eff = effects.twoWaves,
                                        threshold = 0.2)
  } else {
    period2saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w3,nodes=15,
                                        eff = effects.twoWaves,
                                        threshold = 0.2,
                                        ans0 = period2saom)
  }
  
  
  sims <- siena07(imputation.options, data = Data.w3,
                  effects = effects.twoWaves,
                  prevAns = period2saom,
                  returnDeps = TRUE)$sims[[10]]
  
  int3imp[,d] <- sims[[2]]
  save.image('conmi.RData') 
}

##############################################################################
############################# 4. Estimating the models ######################
##############################################################################

#modify the network slightly in each wave
n1 <- network1
tieList <- c(1:(nrow(n1)**2))[c(n1 == 0)]
tieList <- tieList[!is.na(tieList)]

changedTie <- sample(tieList,1)

n2 <- n1
n2[changedTie] <- NA
n3 <- n2
changedTie <- sample(tieList,1)
n3[changedTie] <- NA


consaomResults <- list()
constantDataList <- list()


for (d in 1:D) {
  cat('Imputation',d,'\n')
  
  visits <- sienaDependent(array(c(n1, n2, n3), dim = c(N,N, 3)) ,
                           allowOnly = FALSE)
  
  intention <- sienaDependent(cbind(int1imp[,d], int2imp[,d], int3imp[,d]), 
                              type = "behavior", allowOnly = FALSE)
  Know <- varCovar(cbind((complete(miceImp, d)$follow.Knowledge/sd(complete(miceImp, d)$follow.Knowledge,na.rm=T)),
                         (complete(miceImp, d)$final.Knowledge/sd(complete(miceImp, d)$final.Knowledge,na.rm=T))))
  
  Data <- sienaDataCreate(visits,intention,Dummy,Age,Wealth,Know, Atd, Gender, SMP, pesticide)
  
  
  effects.constant <- getEffects(Data)
  effects.constant[effects.constant$shortName == 'recip',]$include <- FALSE
  
  #Dummy
  effects.constant <- includeEffects(effects.constant, effFrom, name="intention", interaction1="Dummy")  
  #Controls
  effects.constant <- includeEffects(effects.constant, name = "intention",effFrom, interaction1 = "SMP")
  effects.constant <- includeEffects(effects.constant, effFrom, name="intention", interaction1="Age")
  effects.constant <- includeEffects(effects.constant, name = "intention",effFrom, interaction1 = "Wealth")
  effects.constant <- includeEffects(effects.constant, effFrom, name="intention", interaction1="pesticide")
  effects.constant <- includeEffects(effects.constant, name = "intention",effFrom, interaction1 = "Gender")
  
  effects.constant <- includeEffects(effects.constant, name = "intention",
                                     indeg, interaction1 = "visits")
  effects.constant <- includeEffects(effects.constant, name = "intention",
                                     outdeg, interaction1 = "visits")
  
  #Knowledge
  effects.constant <- includeEffects(effects.constant, effFrom, name="intention", interaction1="Know")
  # influence
  effects.constant <- includeEffects(effects.constant, name = "intention",
                                     avSim, interaction1 = "visits")
  
  effects.constant <- includeInteraction(effects.constant, avSim, effFrom,
                                         name="intention", interaction1=c("visits","Know"))
  effects.constant <- includeInteraction(effects.constant, avSim, effFrom,
                                         name="intention", interaction1=c("visits","Dummy"))
  

  
  #fix the rate function 
  effects.constant <- setEffect(effects.constant, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  effects.constant <- setEffect(effects.constant, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE, period=2)
  
  
  estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                             seed = 290,
                                             n3 = 3000, maxlike = FALSE,
                                             behModelType = c(intention = 2),
                                             lessMem = FALSE, cond=F)
  
  if (d == 1) {
    consaomResults[[d]] <- siena07ToConvergence(alg = estimation.options,nodes=15,
                                                dat = Data, eff = effects.constant,
                                                threshold = 0.2)
  } else {
    consaomResults[[d]] <- siena07ToConvergence(alg = estimation.options,nodes=15,
                                                dat = Data, eff = effects.constant,
                                                threshold = 0.2,
                                                ans0 = consaomResults[[d - 1]])
  }
  
  save.image('conmi.RData') 
}

saveRDS(consaomResults, file="Constant network fit final=5.rds")
write.csv(int1imp, "Constant network-Int1Imp-f20.csv")
write.csv(int2imp, "Constant network-Int2Imp-f20.csv")
write.csv(int3imp, "Constant network-Int3Imp-f20.csv")
write.csv(n1, "Constant network-net1.csv")
write.csv(n2, "Constant network-net2.csv")
write.csv(n3, "Constant network-net3.csv")


##Combining results

rowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

npar <- sum(effects.constant$include)



conMIResults <- as.data.frame(matrix(,npar,(2 * D)))

for (d in 1:D) {
  names(conMIResults)[d * 2 - 1] <- paste("imp" , "mean", sep = as.character(d))
  names(conMIResults)[d * 2] <- paste("imp" , "se", sep = as.character(d))
  conMIResults[,d * 2 - 1] <- consaomResults[[d]]$theta
  conMIResults[,d * 2] <-  sqrt(diag(consaomResults[[d]]$covtheta))
}

WDMIs <- matrix(0,npar,npar)

for (d in 1:D) {
  WDMIs <- WDMIs + consaomResults[[d]]$covtheta
}

WDMIs <- (1/D) * WDMIs

confinalResults <- as.data.frame(matrix(,npar,2))
names(confinalResults) <- c("combinedEstimate", "combinedSE")
rownames(confinalResults) <- consaomResults[[1]]$effects$effectName
confinalResults$combinedEstimate <- rowMeans(conMIResults[,seq(1,2*D,2)])
confinalResults$combinedSE <- sqrt(diag(WDMIs) + ((D + 1)/D) *
                                     rowVar(conMIResults[,seq(1,2*D,2)]))
table(round(confinalResults, 3))

write.csv(confinalResults, "Constant network results final D=20.csv")

###################################################################################################
########### Imputation and estimation of the SAOM for descriptive norms ###########################

network1 <- network 

#####Stationary SAOM
visits <- sienaDependent(array(c(network1, network1), dim = c(N,N, 2)) ,
                         allowOnly = FALSE)
a2 <- coCovar(networkdata$follow.dnorms) # the 2nd wave incomplete behavior as covariate

stationaryDataList <- list()

for (d in 1:D) {
  dnorms <- sienaDependent(cbind(complete(miceImp,d)$base.dnorms,
                                 complete(miceImp,d)$base.dnorms), 
                           type = "behavior", allowOnly = FALSE)
  
  intention <- coCovar(complete(miceImp,d)$base.intention)
  
  stationaryDataList[[d]] <- sienaDataCreate(visits,dnorms,a2, Atd, Age, Gender, Wealth, pesticide, SMP, intention)
}

Data.stationary <- sienaGroupCreate(stationaryDataList)

effects.stationary <- getEffects(Data.stationary)
effects.stationary[effects.stationary$shortName == 'recip',]$include <- FALSE
# 2nd wave as covariate
effects.stationary <- includeEffects(effects.stationary, effFrom, 
                                     name = "dnorms", interaction1 = "a2")
# influence
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     avXAlt, interaction1 = "intention", interaction2="visits")
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     indeg, interaction1 = "visits")
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     outdeg, interaction1 = "visits")

#effect from attendance
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     effFrom, interaction1 = "SMP")
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     effFrom, interaction1 = "pesticide")
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     effFrom, interaction1 = "Age")
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     effFrom, interaction1 = "Wealth")
effects.stationary <- includeEffects(effects.stationary, name = "dnorms",
                                     effFrom, interaction1 = "Gender")

for (d in 1:D) { #fix the rate function
  effects.stationary <- setEffect(effects.stationary, Rate, initialValue = 0.01,
                                  name = "visits",fix = TRUE, 
                                  group = d,type = "rate",test = FALSE)
  
  effects.stationary <- setEffect(effects.stationary, Rate, initialValue = 3,
                                  name = "dnorms",fix = TRUE,
                                  group = d,type = "rate",test = FALSE)
}

estimation.options.st <- sienaAlgorithmCreate(useStdInits = FALSE,
                                              seed = 214,
                                              n3 = 3000, maxlike = FALSE,
                                              cond = FALSE, diagonalize = 0.6,
                                              firstg = 0.02,
                                              behModelType = c(dnorms = 2),
                                              lessMem = TRUE)
#estimate the SAOM
period0saom <- siena07ToConvergence(alg = estimation.options.st,
                                    dat = Data.stationary, nodes=10,
                                    eff = effects.stationary, threshold = 0.2)

imputation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 214,
                                           cond = FALSE, 
                                           behModelType = c(dnorms = 2),
                                           maxlike = TRUE,
                                           nsub = 0,
                                           simOnly = TRUE,
                                           n3 = 10)
set.seed(142)

stationaryImpDataList <- list()

for (d in 1:D) {
  n1 <- network1
  n1 <- n1 + 10
  n1 <- ifelse(n1>11, 11, n1)
  diag(n1) <- 0
  n2 <- n1
  tieList <- c(1:(nrow(n1)**2))[c(n1 == 11)]
  tieList <- tieList[!is.na(tieList)]
  
  changedTie <- sample(tieList,1)
  
  n1[changedTie] <- 0
  n2[changedTie] <- 1
  
  visits <- sienaDependent(array(c(n1,n2), dim = c(N,N, 2)),
                           allowOnly = FALSE )
  
  
  i1 <- networkdata$base.dnorms
  i1.3s <- c(1:N)[i1 == 8 & !is.na(i1)]
  int <- sample(i1.3s,1)
  i1change <- complete(miceImp,d)$base.dnorms
  i1change[int] <- sample(c(7,9),1)
  
  dnorms <- sienaDependent(cbind(i1change,i1), type = "behavior",
                           allowOnly = FALSE)
  
  stationaryImpDataList[[d]] <- sienaDataCreate(visits,dnorms,a2,Atd,Age,Gender,Wealth,SMP,pesticide,intention)
}

Data.stationary.imp <- sienaGroupCreate(stationaryImpDataList)

#impute first wave
sims <- siena07(imputation.options, data = Data.stationary.imp,
                effects = effects.stationary,
                prevAns = period0saom,
                returnDeps = TRUE)$sims[[10]]


int1imp <- matrix(NA,N,D)

for (d in 1:D) {
  int1imp[,d] = sims[[d]][[1]][[2]]
}
save.image('conmi.RData') 
##########################################################################
################### Imputing Later Waves #################################
##########################################################################
int2imp <- matrix(NA,N,D)
int3imp <- matrix(NA,N,D)


n1 <- network1
diag(n1) <- 0
n2 <- n1
tieList <- c(1:(nrow(n1)**2))[c(n1 == 1)]
tieList <- tieList[!is.na(tieList)]

changedTie <- sample(tieList,1)

n1[changedTie] <- 0
n2[changedTie] <- 1


set.seed(1402)

estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 214,
                                           n3 = 3000, maxlike = FALSE,
                                           cond = FALSE, diagonalize = 0.3,
                                           firstg = 0.02,
                                           behModelType = c(dnorms = 2),
                                           lessMem = TRUE)


for (d in 1:D) {
  
  cat('imputation',d,'\n')
  
  # now impute wave2
  
  visits <- sienaDependent(array(c(n1,n2),
                                 dim = c(N,N,2)))
  dnorms <- sienaDependent(cbind(int1imp[,d], networkdata$follow.dnorms), type = "behavior")
  Know <- coCovar(complete(miceImp, d)$follow.Knowledge/sd(complete(miceImp, d)$follow.Knowledge,na.rm=T))
  a3 <- coCovar(networkdata$final.dnorms)
  intention <- coCovar(complete(miceImp,d)$follow.intention)
  Data.w2  <- sienaDataCreate(visits, dnorms, Age, Gender, Wealth, Atd, Know, pesticide, SMP,
                              intention, a3)
  
  effects.twoWaves <- getEffects(Data.w2)
  effects.twoWaves[effects.twoWaves$shortName == 'recip',]$include <- FALSE
  
  effects.twoWaves <- includeEffects(effects.twoWaves, avXAlt, 
                                     name = 'dnorms',
                                     interaction1 =  "intention", interaction2="visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Know")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="a3")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Wealth")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Gender")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Age")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="SMP")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="pesticide")
  
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "dnorms",
                                     indeg, interaction1 = "visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "dnorms",
                                     outdeg, interaction1 = "visits")
  
  
  #fix the rate function 
  effects.twoWaves <- setEffect(effects.twoWaves, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  
  if (d == 1) {
    period1saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w2,nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2)
  } else {
    period1saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w2, nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2,
                                        ans0 = period1saom)
  }
  
  sims <- siena07(imputation.options, data = Data.w2,
                  effects = effects.twoWaves,
                  prevAns = period1saom,
                  returnDeps = TRUE)$sims[[10]]
  
  int2imp[,d] <- sims[[2]]
  
  # impute wave 3
  visits <- sienaDependent(array( c(n1,n2),
                                  dim = c(N,N, 2)))
  dnorms <- sienaDependent(cbind(int2imp[,d],networkdata$final.dnorms), type = "behavior")
  Know <- coCovar(complete(miceImp, d)$final.Knowledge/sd(complete(miceImp, d)$final.Knowledge,na.rm=T))
  intention <- coCovar(complete(miceImp,d)$final.intention)
  Data.w3  <- sienaDataCreate(visits, dnorms, Age, Wealth, Atd, Know, Gender, SMP, intention, pesticide)
  
  effects.twoWaves <- getEffects(Data.w3)
  effects.twoWaves[effects.twoWaves$shortName == 'recip',]$include <- FALSE
  effects.twoWaves <- includeEffects(effects.twoWaves, avXAlt, 
                                     name = 'dnorms',
                                     interaction1 =  "intention", interaction2="visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Know")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Wealth")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Gender")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="Age")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="SMP")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="dnorms", interaction1="pesticide")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "dnorms",
                                     indeg, interaction1 = "visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "dnorms",
                                     outdeg, interaction1 = "visits")
  

  #fix the rate function 
  effects.twoWaves <- setEffect(effects.twoWaves, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  
  if (d == 1) {
    period2saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w3,nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2)
  } else {
    period2saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w3,nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2,
                                        ans0 = period2saom)
  }
  
  
  sims <- siena07(imputation.options, data = Data.w3,
                  effects = effects.twoWaves,
                  prevAns = period2saom,
                  returnDeps = TRUE)$sims[[10]]
  
  int3imp[,d] <- sims[[2]]
  save.image('conmi.RData') 
}

##############################################################################
############################# 4. Estimating the models ######################
##############################################################################

#modify the network slightly in each wave
n1 <- network1
tieList <- c(1:(nrow(n1)**2))[c(n1 == 0)]
tieList <- tieList[!is.na(tieList)]

changedTie <- sample(tieList,1)

n2 <- n1
n2[changedTie] <- NA
n3 <- n2
changedTie <- sample(tieList,1)
n3[changedTie] <- NA


consaomResults <- list()
constantDataList <- list()


for (d in 1:D) {
  cat('Imputation',d,'\n')
  
  visits <- sienaDependent(array(c(n1, n2, n3), dim = c(N,N, 3)) ,
                           allowOnly = FALSE)
  
  dnorms <- sienaDependent(cbind(int1imp[,d], int2imp[,d], int3imp[,d]), 
                           type = "behavior", allowOnly = FALSE)
  Know <- varCovar(cbind((complete(miceImp, d)$follow.Knowledge/sd(complete(miceImp, d)$follow.Knowledge,na.rm=T)),
                         (complete(miceImp, d)$final.Knowledge/sd(complete(miceImp, d)$final.Knowledge,na.rm=T))))
  intention <-varCovar(cbind(complete(miceImp,d)$base.intention, 
                             complete(miceImp,d)$follow.intention, complete(miceImp,d)$final.intention))
  Data <- sienaDataCreate(visits,dnorms,Dummy, Age,Wealth,Gender,SMP,pesticide,intention,Know, Atd)
  
  
  effects.constant <- getEffects(Data)
  effects.constant[effects.constant$shortName == 'recip',]$include <- FALSE
  
  effects.constant <- includeEffects(effects.constant, effFrom, name="dnorms", interaction1="Know")
  effects.constant <- includeEffects(effects.constant, name = "dnorms",effFrom, interaction1 = "SMP")
  effects.constant <- includeEffects(effects.constant, effFrom, name="dnorms", interaction1="Gender")
  effects.constant <- includeEffects(effects.constant, effFrom, name="dnorms", interaction1="Age")
  effects.constant <- includeEffects(effects.constant, name = "dnorms",effFrom, interaction1 = "Wealth")
  effects.constant <- includeEffects(effects.constant, name = "dnorms",effFrom, interaction1 = "pesticide")
  effects.constant <- includeEffects(effects.constant, effFrom, name="dnorms", interaction1="Dummy")  
  
  effects.constant <- includeEffects(effects.constant, name = "dnorms",
                                     indeg, interaction1 = "visits")
  effects.constant <- includeEffects(effects.constant, name = "dnorms",
                                     outdeg, interaction1 = "visits")
  
  effects.constant <- includeEffects(effects.constant, name = "dnorms",
                                     avXAlt, interaction1 = "intention", interaction2="visits")

  effects.constant <- includeInteraction(effects.constant, avXAlt, effFrom,
                                         name="dnorms", interaction1=c("intention","Know"),interaction2=c("visits",""))
  effects.constant <- includeInteraction(effects.constant, avXAlt, effFrom,
                                         name="dnorms", interaction1=c("intention","Dummy"), interaction2=c("visits",""))
  

  #fix the rate function 
  effects.constant <- setEffect(effects.constant, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  effects.constant <- setEffect(effects.constant, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE, period=2)
  
  
  estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                             seed = 214,
                                             n3 = 3000, maxlike = FALSE,
                                             behModelType = c(dnorms = 2),
                                             lessMem = FALSE, cond=F)
  
  if (d == 1) {
    consaomResults[[d]] <- siena07ToConvergence(alg = estimation.options,nodes=10,
                                                dat = Data, eff = effects.constant,
                                                threshold = 0.2)
  } else {
    consaomResults[[d]] <- siena07ToConvergence(alg = estimation.options,nodes=10,
                                                dat = Data, eff = effects.constant,
                                                threshold = 0.2,
                                                ans0 = consaomResults[[d - 1]])
  }
  
  save.image('conmi.RData') 
}

saveRDS(consaomResults, file="Constant network dnorms.rds")
write.csv(int1imp, "Constant network-Int1Imp-3.csv")
write.csv(int2imp, "Constant network-Int2Imp-3.csv")
write.csv(int3imp, "Constant network-Int3Imp-3.csv")
write.csv(n1, "Constant network-net1.csv")
write.csv(n2, "Constant network-net2.csv")
write.csv(n3, "Constant network-net3.csv")


##Combining results

rowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

npar <- sum(effects.constant$include)

conMIResults <- as.data.frame(matrix(,npar,(2 * D)))

for (d in 1:D) {
  names(conMIResults)[d * 2 - 1] <- paste("imp" , "mean", sep = as.character(d))
  names(conMIResults)[d * 2] <- paste("imp" , "se", sep = as.character(d))
  conMIResults[,d * 2 - 1] <- consaomResults[[d]]$theta
  conMIResults[,d * 2] <-  sqrt(diag(consaomResults[[d]]$covtheta))
}

WDMIs <- matrix(0,npar,npar)

for (d in 1:D) {
  WDMIs <- WDMIs + consaomResults[[d]]$covtheta
}

WDMIs <- (1/D) * WDMIs

confinalResults <- as.data.frame(matrix(,npar,2))
names(confinalResults) <- c("combinedEstimate", "combinedSE")
rownames(confinalResults) <- consaomResults[[1]]$effects$effectName
confinalResults$combinedEstimate <- rowMeans(conMIResults[,seq(1,2*D,2)])
confinalResults$combinedSE <- sqrt(diag(WDMIs) + ((D + 1)/D) *
                                     rowVar(conMIResults[,seq(1,2*D,2)]))
table(round(confinalResults, 3))

write.csv(confinalResults, "Constant network results dnorms D=20.csv")


###################################################################################################
########### Imputation and estimation of the SAOM for injunctive norms #################################

network1 <- network 

#####Stationary SAOM
visits <- sienaDependent(array(c(network1, network1), dim = c(N,N, 2)) ,
                         allowOnly = FALSE)
a2 <- coCovar(networkdata$follow.innorms) # the 2nd wave incomplete behavior as covariate

stationaryDataList <- list()

for (d in 1:D) {
  innorms <- sienaDependent(cbind(complete(miceImp,d)$base.innorms,
                                  complete(miceImp,d)$base.innorms), 
                            type = "behavior", allowOnly = FALSE)
  
  attitudes <- coCovar(complete(miceImp,d)$base.attitudes)
  
  stationaryDataList[[d]] <- sienaDataCreate(visits,innorms,a2, Atd, Age, Gender, Wealth, pesticide, SMP, attitudes)
}

Data.stationary <- sienaGroupCreate(stationaryDataList)

effects.stationary <- getEffects(Data.stationary)
effects.stationary[effects.stationary$shortName == 'recip',]$include <- FALSE
# 2nd wave as covariate
effects.stationary <- includeEffects(effects.stationary, effFrom, 
                                     name = "innorms", interaction1 = "a2")
# influence
effects.stationary <- includeEffects(effects.stationary, name = "innorms",
                                     avXAlt, interaction1 = "attitudes", interaction2="visits")
effects.stationary <- includeEffects(effects.stationary, name = "innorms",
                                     indeg, interaction1 = "visits")
effects.stationary <- includeEffects(effects.stationary, name = "innorms",
                                     outdeg, interaction1 = "visits")

#effect from attendance
effects.stationary <- includeEffects(effects.stationary, name = "innorms",
                                     effFrom, interaction1 = "SMP")


for (d in 1:D) { #fix the rate function
  effects.stationary <- setEffect(effects.stationary, Rate, initialValue = 0.01,
                                  name = "visits",fix = TRUE, 
                                  group = d,type = "rate",test = FALSE)
  
  effects.stationary <- setEffect(effects.stationary, Rate, initialValue = 3,
                                  name = "innorms",fix = TRUE,
                                  group = d,type = "rate",test = FALSE)
}

estimation.options.st <- sienaAlgorithmCreate(useStdInits = FALSE,
                                              seed = 214,
                                              n3 = 3000, maxlike = FALSE,
                                              cond = FALSE, diagonalize = 0.6,
                                              firstg = 0.02,
                                              behModelType = c(innorms = 2),
                                              lessMem = TRUE)
#estimate the SAOM
period0saom <- siena07ToConvergence(alg = estimation.options.st,
                                    dat = Data.stationary, nodes=10,
                                    eff = effects.stationary, threshold = 0.2)

imputation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 214,
                                           cond = FALSE, 
                                           behModelType = c(innorms = 2),
                                           maxlike = TRUE,
                                           nsub = 0,
                                           simOnly = TRUE,
                                           n3 = 10)
set.seed(142)

stationaryImpDataList <- list()

for (d in 1:D) {
  n1 <- network1
  n1 <- n1 + 10
  n1 <- ifelse(n1>11, 11, n1)
  diag(n1) <- 0
  n2 <- n1
  tieList <- c(1:(nrow(n1)**2))[c(n1 == 11)]
  tieList <- tieList[!is.na(tieList)]
  
  changedTie <- sample(tieList,1)
  
  n1[changedTie] <- 0
  n2[changedTie] <- 1
  
  visits <- sienaDependent(array(c(n1,n2), dim = c(N,N, 2)),
                           allowOnly = FALSE )
  
  
  i1 <- networkdata$base.innorms
  i1.3s <- c(1:N)[i1 == 8 & !is.na(i1)]
  int <- sample(i1.3s,1)
  i1change <- complete(miceImp,d)$base.innorms
  i1change[int] <- sample(c(7,9),1)
  
  innorms <- sienaDependent(cbind(i1change,i1), type = "behavior",
                            allowOnly = FALSE)
  
  stationaryImpDataList[[d]] <- sienaDataCreate(visits,innorms,a2,Atd,Age,Gender,Wealth,SMP,pesticide,attitudes)
}

Data.stationary.imp <- sienaGroupCreate(stationaryImpDataList)

#impute first wave
sims <- siena07(imputation.options, data = Data.stationary.imp,
                effects = effects.stationary,
                prevAns = period0saom,
                returnDeps = TRUE)$sims[[10]]


int1imp <- matrix(NA,N,D)

for (d in 1:D) {
  int1imp[,d] = sims[[d]][[1]][[2]]
}
save.image('conmi.RData') 
##########################################################################
################### Imputing Later Waves #################################
##########################################################################

n1 <- network1
diag(n1) <- 0
n2 <- n1
tieList <- c(1:(nrow(n1)**2))[c(n1 == 1)]
tieList <- tieList[!is.na(tieList)]

changedTie <- sample(tieList,1)

n1[changedTie] <- 0
n2[changedTie] <- 1


int2imp <- matrix(NA,N,D)
int3imp <- matrix(NA,N,D)

set.seed(1402)

estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 214,
                                           n3 = 3000, maxlike = FALSE,
                                           cond = FALSE, diagonalize = 0.3,
                                           firstg = 0.02,
                                           behModelType = c(innorms = 2),
                                           lessMem = TRUE)


for (d in 1:D) {
  
  cat('imputation',d,'\n')
  
  # now impute wave2
  
  visits <- sienaDependent(array(c(n1,n2),
                                 dim = c(N,N,2)))
  innorms <- sienaDependent(cbind(int1imp[,d], networkdata$follow.innorms), type = "behavior")
  Know <- coCovar(complete(miceImp, d)$follow.Knowledge/sd(complete(miceImp, d)$follow.Knowledge,na.rm=T))
  a3 <- coCovar(networkdata$final.innorms)
  attitudes <- coCovar(complete(miceImp,d)$follow.attitudes)
  Data.w2  <- sienaDataCreate(visits, innorms, Age, Gender, Wealth, Atd, Know, pesticide, SMP,
                              attitudes, a3)
  
  effects.twoWaves <- getEffects(Data.w2)
  effects.twoWaves[effects.twoWaves$shortName == 'recip',]$include <- FALSE
  
  effects.twoWaves <- includeEffects(effects.twoWaves, avXAlt, 
                                     name = 'innorms',
                                     interaction1 =  "attitudes", interaction2="visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="innorms", interaction1="Know")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="innorms", interaction1="a3")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="innorms", interaction1="SMP")
  
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "innorms",
                                     indeg, interaction1 = "visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "innorms",
                                     outdeg, interaction1 = "visits")
  
  #fix the rate function 
  effects.twoWaves <- setEffect(effects.twoWaves, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  
  if (d == 1) {
    period1saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w2,nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2)
  } else {
    period1saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w2, nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2,
                                        ans0 = period1saom)
  }
  
  sims <- siena07(imputation.options, data = Data.w2,
                  effects = effects.twoWaves,
                  prevAns = period1saom,
                  returnDeps = TRUE)$sims[[10]]
  
  int2imp[,d] <- sims[[2]]
  
  # impute wave 3
  visits <- sienaDependent(array( c(n1,n2),
                                  dim = c(N,N, 2)))
  innorms <- sienaDependent(cbind(int2imp[,d],networkdata$final.innorms), type = "behavior")
  Know <- coCovar(complete(miceImp, d)$final.Knowledge/sd(complete(miceImp, d)$final.Knowledge,na.rm=T))
  attitudes <- coCovar(complete(miceImp,d)$final.attitudes)
  Data.w3  <- sienaDataCreate(visits, innorms, Age, Wealth, Atd, Know, Gender, SMP, attitudes, pesticide)
  
  effects.twoWaves <- getEffects(Data.w3)
  effects.twoWaves[effects.twoWaves$shortName == 'recip',]$include <- FALSE
  
  effects.twoWaves <- includeEffects(effects.twoWaves, avXAlt, 
                                     name = 'innorms',
                                     interaction1 =  "attitudes", interaction2="visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="innorms", interaction1="Know")
  effects.twoWaves <- includeEffects(effects.twoWaves, effFrom, name="innorms", interaction1="SMP")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "innorms",
                                     indeg, interaction1 = "visits")
  effects.twoWaves <- includeEffects(effects.twoWaves, name = "innorms",
                                     outdeg, interaction1 = "visits")
  
  #fix the rate function 
  effects.twoWaves <- setEffect(effects.twoWaves, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  
  if (d == 1) {
    period2saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w3,nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2)
  } else {
    period2saom <- siena07ToConvergence(alg = estimation.options,
                                        dat = Data.w3,nodes=10,
                                        eff = effects.twoWaves,
                                        threshold = 0.2,
                                        ans0 = period2saom)
  }
  
  
  sims <- siena07(imputation.options, data = Data.w3,
                  effects = effects.twoWaves,
                  prevAns = period2saom,
                  returnDeps = TRUE)$sims[[10]]
  
  int3imp[,d] <- sims[[2]]
  save.image('conmi.RData') 
}

##############################################################################
############################# 4. Estimating the models ######################
##############################################################################

#modify the network slightly in each wave
n1 <- network1
tieList <- c(1:(nrow(n1)**2))[c(n1 == 0)]
tieList <- tieList[!is.na(tieList)]

changedTie <- sample(tieList,1)

n2 <- n1
n2[changedTie] <- NA
n3 <- n2
changedTie <- sample(tieList,1)
n3[changedTie] <- NA


consaomResults <- list()
constantDataList <- list()


for (d in 1:D) {
  cat('Imputation',d,'\n')
  
  visits <- sienaDependent(array(c(n1, n2, n3), dim = c(N,N, 3)) ,
                           allowOnly = FALSE)
  
  innorms <- sienaDependent(cbind(int1imp[,d], int2imp[,d], int3imp[,d]), 
                            type = "behavior", allowOnly = FALSE)
  Know <- varCovar(cbind((complete(miceImp, d)$follow.Knowledge/sd(complete(miceImp, d)$follow.Knowledge,na.rm=T)),
                         (complete(miceImp, d)$final.Knowledge/sd(complete(miceImp, d)$final.Knowledge,na.rm=T))))
  attitudes <-varCovar(cbind(complete(miceImp,d)$base.attitudes, 
                             complete(miceImp,d)$follow.attitudes, complete(miceImp,d)$final.attitudes))
  Data <- sienaDataCreate(visits,innorms,Dummy,Age,Wealth,Gender,SMP,pesticide,attitudes,Know)
  
  
  effects.constant <- getEffects(Data)
  effects.constant[effects.constant$shortName == 'recip',]$include <- FALSE
  effects.constant <- includeEffects(effects.constant, effFrom, name="innorms", interaction1="Know")
  effects.constant <- includeEffects(effects.constant, name = "innorms",effFrom, interaction1 = "SMP")
  effects.constant <- includeEffects(effects.constant, effFrom, name="innorms", interaction1="Dummy") 
  
  effects.constant <- includeEffects(effects.constant, name = "innorms",
                                     indeg, interaction1 = "visits")
  effects.constant <- includeEffects(effects.constant, name = "innorms",
                                     outdeg, interaction1 = "visits")
  effects.constant <- includeEffects(effects.constant, name = "innorms",
                                     avXAlt, interaction1 = "attitudes", interaction2="visits")
  
  effects.constant <- includeInteraction(effects.constant, avXAlt, effFrom,
                                         name="innorms", interaction1=c("attitudes","Know"),interaction2=c("visits",""))
  effects.constant <- includeInteraction(effects.constant, avXAlt, effFrom,
                                         name="innorms", interaction1=c("attitudes","Dummy"), interaction2=c("visits",""))
  
  
  #fix the rate function 
  effects.constant <- setEffect(effects.constant, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE)
  effects.constant <- setEffect(effects.constant, Rate, initialValue = 0.01,
                                name = "visits",fix = TRUE, 
                                type = "rate",test = FALSE, period=2)
  
  
  estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                             seed = 122,
                                             n3 = 3000, maxlike = FALSE,
                                             behModelType = c(innorms = 2),
                                             lessMem = FALSE, cond=F)
  
  if (d == 1) {
    consaomResults[[d]] <- siena07ToConvergence(alg = estimation.options,nodes=10,
                                                dat = Data, eff = effects.constant,
                                                threshold = 0.2)
  } else {
    consaomResults[[d]] <- siena07ToConvergence(alg = estimation.options,nodes=10,
                                                dat = Data, eff = effects.constant,
                                                threshold = 0.2,
                                                ans0 = consaomResults[[d - 1]])
  }
  
  save.image('conmi.RData') 
}

saveRDS(consaomResults, file="Constant network innorms D=20.rds")
write.csv(int1imp, "Constant network-Int1Imp-3-20.csv")
write.csv(int2imp, "Constant network-Int2Imp-3-20.csv")
write.csv(int3imp, "Constant network-Int3Imp-3-20.csv")
write.csv(n1, "Constant network-net1.csv")
write.csv(n2, "Constant network-net2.csv")
write.csv(n3, "Constant network-net3.csv")

##Combining results

rowVar <- function(x) {
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

npar <- sum(effects.constant$include)

conMIResults <- as.data.frame(matrix(,npar,(2 * D)))

for (d in 1:D) {
  names(conMIResults)[d * 2 - 1] <- paste("imp" , "mean", sep = as.character(d))
  names(conMIResults)[d * 2] <- paste("imp" , "se", sep = as.character(d))
  conMIResults[,d * 2 - 1] <- consaomResults[[d]]$theta
  conMIResults[,d * 2] <-  sqrt(diag(consaomResults[[d]]$covtheta))
}

WDMIs <- matrix(0,npar,npar)

for (d in 1:D) {
  WDMIs <- WDMIs + consaomResults[[d]]$covtheta
}

WDMIs <- (1/D) * WDMIs

confinalResults <- as.data.frame(matrix(,npar,2))
names(confinalResults) <- c("combinedEstimate", "combinedSE")
rownames(confinalResults) <- consaomResults[[1]]$effects$effectName
confinalResults$combinedEstimate <- rowMeans(conMIResults[,seq(1,2*D,2)])
confinalResults$combinedSE <- sqrt(diag(WDMIs) + ((D + 1)/D) *
                                     rowVar(conMIResults[,seq(1,2*D,2)]))
table(round(confinalResults, 3))

write.csv(confinalResults, "Constant network results innorms D=20.csv")

###########################################################################################
###################### Estimation of SAOM for information flow ###########################

#Create knowledge variable as a binary non-decreasing variable
##Create the knowledge variables
Knowledge.follow <- rowSums(cbind(networkdata$follow.hotline, networkdata$follow.pledge, networkdata$follow.story), na.rm=T) 
Knowledge.final <- rowSums(cbind(networkdata$final.hotline, networkdata$final.pledge, networkdata$final.story), na.rm=T)
Knowledge.base <- rep(0,365)
Knowledge <- as.data.frame(cbind(Knowledge.base, Knowledge.follow, Knowledge.final))

Attendance <- networkdata$follow.event_attendance
Attendance[is.na(Attendance)] <- 0

BinKnowledge <- as.data.frame(Knowledge)
BinKnowledge$one <- Attendance
BinKnowledge$two <- 0
BinKnowledge$three <- 0
for (i in 1:365){
  BinKnowledge$two[i] <- ifelse(BinKnowledge$Knowledge.follow[i]>0,1,0)
  BinKnowledge$three[i] <- ifelse(BinKnowledge$two[i]==1, 1, 0)
  BinKnowledge$three[i] <- ifelse(BinKnowledge$Knowledge.final[i]>0, 1, BinKnowledge$three[i])
  
  if(is.na(BinKnowledge$two[i])){BinKnowledge$two[i] <- BinKnowledge$one[i]}
  if(is.na(BinKnowledge$three[i])){BinKnowledge$three[i] <- BinKnowledge$two[i]}
  
}

BinKnowledge <- dplyr::select(BinKnowledge, one, two, three) #BinKnowledge is complete (imputed by us) and non-decreasing
colnames(BinKnowledge) <- c("base.know","follow.know","final.know")
Infomat <- as.matrix(BinKnowledge)

#Modify the network by one tie in each wave
n1 <- network
tieList <- c(1:(nrow(n1)**2))[c(n1 == 0)]
tieList <- tieList[!is.na(tieList)]
changedTie <- sample(tieList,1)
n2 <- n1
n2[changedTie] <- NA
n3 <- n2
changedTie <- sample(tieList,1)
n3[changedTie] <- NA

Sienanet <- sienaDependent(array(c(n1, n2, n3), dim = c(N,N, 3)) ,
                          allowOnly = FALSE)
SienaBinKnow <- sienaDependent(Infomat, type="behavior")

InfoData <- sienaDataCreate(Sienanet,SienaBinKnow)

diffusion.effects <- getEffects(InfoData)
diffusion.effects[diffusion.effects$shortName == 'recip',]$include <- FALSE

diffusion.effects <- includeEffects(diffusion.effects, totExposure, name="SienaBinKnow", 
                                    interaction1 = "Sienanet", type="rate")

#fix the rate function 
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "network",fix = TRUE, 
                               type = "rate",test = FALSE)
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "network",fix = TRUE, 
                               type = "rate",test = FALSE, period=2)

estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 214, lessMem=FALSE,
                                           n3 = 3000, maxlike = FALSE, cond=F)

DOI.saomResults <- siena07ToConvergence(alg = estimation.options,nodes=7,
                                        dat = InfoData, eff = diffusion.effects,
                                        threshold = 0.2)

DOIGOF <- sienaGOF(DOI.saomResults, varName = "SienaBinKnow", 
                   BehaviorDistribution)
plot(DOIGOF)
summary(DOIGOF)

saveRDS(DOI.saomResults, file="Diffusion of Innovations combined.rds")


## Now repeat with the networks split up
Visits <- as.matrix(read.csv("visits network.csv"))
#Visits <- as.matrix(read.csv("updated visits network.csv")) #for updated network
Visitors <- as.matrix(read.csv("Visitors network.csv"))
#Visitors <- as.matrix(read.csv("updated visitors network.csv")) #for updated network
HHnet <- as.matrix(read.csv("coresidence network.csv"))
#HHnet <- as.matrix(read.csv("coresidence network.csv")) #for updated network

#modify each network by one tie for each wave
n1 <- Visits
tieList <- c(1:(nrow(n1)**2))[c(n1 == 0)]
tieList <- tieList[!is.na(tieList)]
changedTie <- sample(tieList,1)
n2 <- n1
n2[changedTie] <- NA
n3 <- n2
changedTie <- sample(tieList,1)
n3[changedTie] <- NA

#coresidence
h1 <- HHnet
tieList <- c(1:(nrow(h1)**2))[c(h1 == 0)]
tieList <- tieList[!is.na(tieList)]
changedTie <- sample(tieList,1)
h2 <- h1
h2[changedTie] <- NA
h3 <- h2
changedTie <- sample(tieList,1)
h3[changedTie] <- NA

#visitors
v1 <- Visitors
tieList <- c(1:(nrow(v1)**2))[c(v1 == 0)]
tieList <- tieList[!is.na(tieList)]
cvangedTie <- sample(tieList,1)
v2 <- v1
v2[changedTie] <- NA
v3 <- v2
changedTie <- sample(tieList,1)
v3[changedTie] <- NA


visits <- sienaDependent(array(c(n1, n2, n3), dim = c(N,N, 3)) ,
                         allowOnly = FALSE)
visitors <- sienaDependent(array(c(v1, v2, v3), dim = c(N,N, 3)) ,
                           allowOnly = FALSE)
household <- sienaDependent(array(c(h1, h2, h3), dim = c(N,N, 3)) ,
                            allowOnly = FALSE)

SienaBinKnow <- sienaDependent(Infomat, type="behavior")

InfoData <- sienaDataCreate(visits,visitors,household,SienaBinKnow)

diffusion.effects <- getEffects(InfoData)
diffusion.effects[diffusion.effects$shortName == 'recip',]$include <- FALSE
diffusion.effects <- includeEffects(diffusion.effects, totExposure, name="SienaBinKnow", 
                                    interaction1 = "visits", type="rate")
diffusion.effects <- includeEffects(diffusion.effects, totExposure, name="SienaBinKnow", 
                                    interaction1 = "visitors", type="rate")
diffusion.effects <- includeEffects(diffusion.effects, totExposure, name="SienaBinKnow", 
                                    interaction1 = "household", type="rate")


#fix the rate function 
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "visits",fix = TRUE, 
                               type = "rate",test = FALSE)
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "visits",fix = TRUE, 
                               type = "rate",test = FALSE, period=2)
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "visitors",fix = TRUE, 
                               type = "rate",test = FALSE)
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "visitors",fix = TRUE, 
                               type = "rate",test = FALSE, period=2)
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "household",fix = TRUE, 
                               type = "rate",test = FALSE)
diffusion.effects <- setEffect(diffusion.effects, Rate, initialValue = 0.01,
                               name = "household",fix = TRUE, 
                               type = "rate",test = FALSE, period=2)


estimation.options <- sienaAlgorithmCreate(useStdInits = FALSE,
                                           seed = 214, lessMem=FALSE,
                                           n3 = 3000, maxlike = FALSE, cond=F)

DOI.saomResults <- siena07ToConvergence(alg = estimation.options,nodes=7,
                                        dat = InfoData, eff = diffusion.effects,
                                        threshold = 0.2)

saveRDS(DOI.saomResults, file="Diffusion of Innovations multinet.rds")


##################################################################################################
##################### Goodness of fit checking #################################################

#This script is adapted from a script provided by Chen Wang
# As written, the script can be applied to the models for 'intention'
# Some modification of the script is required to fit it to the other models, this is indicated

library(RSiena)
library(lattice)
library(MASS)
library(Matrix)
library(igraph)
library(gridExtra)


#read the sienafit object
fitlist <- readRDS("Constant network fit final=20.rds")
D <- length(fitlist) # number of imputations


for(imp in 1:D){
  
  behaviour <- cbind(int1imp[,imp], int2imp[,imp], int3imp[,imp])
  finalnet <- n3 ###the network used in wave 3
  finalnet[which(finalnet==11)] <- 1
  ans <- fitlist[[imp]] 
  
  ### GOF testing
  
  ### the original "BehaviorDistribution" in RSiena has some issues
  ### but it provides a perfect strawman object for GOF testing
  gofb <- sienaGOF(ans, BehaviorDistribution, varName = "intention",verbose=TRUE) # <- change intention for other dependent variable
  plot(gofb)
  save(gofb,file="gofb.Rdata")
  
  ### behavior distribution
  k <- table(factor(behaviour[,3], levels = (min(behaviour):max(behaviour))))
  observed <- matrix(k,nrow=1)
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    for(x in min(behaviour):max(behaviour)){
      simulated[z,x-1] <- length(ans$sims[[z]][[1]][[2]][[2]][which(ans$sims[[z]][[1]][[2]][[2]]==x)]) # <- check max and min for the dependent var
      
    }
  }
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Behavior Distribution"
  plot1<-plot(gofb, main="distribution")
  
  ##########################################################################################
  ### behavior transition
  
  
  a <- behaviour[,2]
  b <- behaviour[,3]
  
  k <- c(t(table(factor(a, levels=min(behaviour):max(behaviour)),factor(b, levels =min(behaviour):max(behaviour))))) # <- check max and min
  observed <- matrix(k,nrow=1)
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    g <- cbind(a,ans$sims[[z]][[1]][[2]][[2]])
    for (i in min(behaviour):max(behaviour)) for (j in min(behaviour):max(behaviour)) {
      p <- (i-2)*9+(j-1) # <- check these values depending on the max and min. i.e. for injunctive norm, it should be "(i-5)*15+(j-4)", for info "(i+1)*2+(j-1)"
      simulated[z,p] <- length(which(g[,1]==i & g[,2]==j))
    }
    #simulated[z,] <- c(t(table(a,g)))
  }
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Behavior Transition"
  plot2 <- plot(gofb, main="transition")
  
  ##########################################################################################
  ### behavior change
  
  
  b <- behaviour[,3]-behaviour[,2]
  values <- min(b):max(b)
  
  k <- table(factor(b, levels=values))
  observed <- matrix(k,nrow=1)
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    g <- ans$sims[[z]][[1]][[2]][[2]]-behaviour[,2]
    for(x in 1:n){
      simulated[z,x] <- length(g[which(g==values[x])])
    }
  }
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Behavior Change Values"
  plot3 <- plot(gofb, main="Values")
  
  ##########################################################################################
  ### out-degree & in-degree by behavior
  
  
  f3 <- finalnet
  f3[which(f3==10)] <- NA
  a <- igraph::degree(graph.adjacency(f3),mode="out")
  b <- igraph::degree(graph.adjacency(f3),mode="in")
  
  values <- min(behaviour):max(behaviour)
  n <- length(values)
  
  c <- d <- rep(0,n)
  for(i in 1:n){
    c[i] <- mean(a[which(behaviour[,3]==values[i])])
    d[i] <- mean(b[which(behaviour[,3]==values[i])])
  }
  
  
  k <- c
  observed <- matrix(k,nrow=1)
  observed[is.na(observed)==TRUE] <-0
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    f <- graph_from_edgelist(ans$sims[[z]][[1]][[1]][[2]][,1:2], directed=TRUE)
    h <- ans$sims[[z]][[1]][[2]][[2]]
    j <- igraph::degree(f,mode="out")
    for(i in 1:n){
      simulated[z,i] <- mean(j[which(h==values[i])],na.rm=TRUE)
    }
  }
  simulated[is.na(simulated)==TRUE] <- 0
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Average Out-degree by Behavior"
  plot4 <- plot(gofb, main="out-degree")
  
  k <- d
  observed <- matrix(k,nrow=1)
  observed[is.na(observed)==TRUE] <- 0
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    f <- graph_from_edgelist(ans$sims[[z]][[1]][[1]][[2]][,1:2], directed=TRUE)
    h <- ans$sims[[z]][[1]][[2]][[2]]
    j <- igraph::degree(f,mode="in")
    for(i in 1:n){
      simulated[z,i] <- mean(j[which(h==values[i])],na.rm=TRUE)
    }
  }
  simulated[is.na(simulated)==TRUE] <- 0
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Average In-degree by Behavior"
  plot5 <- plot(gofb, main="in-degree")
  
  ##########################################################################################
  ### edgewise homophily
  
  
  
  net <- finalnet
  net[which(net==10)] <- NA
  net <- graph.adjacency(net, mode="directed")
  edges <- as_edgelist(net)
  colnames(edges) <- c("i","j")
  beh <- behaviour[,3]
  p2 <- p1 <- cbind(beh,c(1:length(beh))) # two auxiliary matrices for merging depression
  colnames(p1)<-c("si","i")
  colnames(p2)<-c("sj","j")
  w <- merge(merge(edges,p1),p2)
  l <- k <- 0
  for (a in 1:nrow(edges)){
    if (is.na(w[a,3])==FALSE & is.na(w[a,4])==FALSE) {
      k <- k+(1-abs(w[a,3]-w[a,4])/2)
      l <- l+1
    }
  }
  k <- k/l
  rm(net,edges,beh,p1,p2,w)
  observed <- matrix(k,nrow=1)
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    edges <- ans$sims[[z]][[1]][[1]][[2]]
    colnames(edges) <- c("i","j","xij")
    beh <- ans$sims[[z]][[1]][[2]][[2]]
    p2 <- p1 <- cbind(beh,c(1:length(beh))) # two auxiliary matrices for merging depression
    colnames(p1)<-c("si","i")
    colnames(p2)<-c("sj","j")
    w <- merge(merge(edges,p1),p2)
    l <- k <- 0
    for (a in 1:nrow(edges)){
      k <- k+(1-abs(w[a,4]-w[a,5])/2)
      l <- l+1
    }
    simulated[z] <- k/l
  }
  
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Edgewise Homophily"
  #plot(gofb,key=c("Edgewise Homophily"))
  
  k1<-observed
  s1<-simulated
  mhp1<-p
  
  ##########################################################################################
  ### autocorrelation
  
  
  net <- finalnet
  net[which(net==10)] <- NA
  net <- graph.adjacency(net, mode="directed")
  edges <- as_edgelist(net)
  colnames(edges)<-c("i","j")
  beh <- behaviour[,3]
  y <- which(is.na(beh)==FALSE)
  nv <- length(y)
  p_bar <- mean(beh[y])
  denominator <- sum((beh[y]-p_bar)^2)/nv
  p2 <- p1 <- cbind(beh,c(1:length(beh))) # two auxiliary matrices for merging depression
  colnames(p1)<-c("pi","i")
  colnames(p2)<-c("pj","j")
  w <- merge(merge(edges,p1),p2)
  ne <- nrow(edges)
  numerator <- 0
  for (a in 1:ne)
  {
    if (is.na(w[a,3])==FALSE & is.na(w[a,4])==FALSE) {
      numerator <- numerator + (w[a,3]-p_bar)*(w[a,4]-p_bar)
    }
  }
  numerator <- numerator/ne
  moranw2 <- numerator/denominator
  moranw2
  
  denominator <- 2*sum((beh[y]-p_bar)^2)/(nv-1)
  numerator <- 0
  for (a in 1:ne)
  {
    if (is.na(w[a,3])==FALSE & is.na(w[a,4])==FALSE) {
      numerator <- numerator + (w[a,3]-w[a,4])^2
    }
  }
  numerator <- numerator/ne
  gearyw2 <- numerator/denominator
  gearyw2
  
  k <- moranw2
  observed <- matrix(k,nrow=1)
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    edges <- ans$sims[[z]][[1]][[1]][[2]]
    ne <- nrow(edges)
    colnames(edges)<-c("i","j","xij")
    nv <- length(ans$sims[[z]][[1]][[2]][[2]])
    t <- matrix(ans$sims[[z]][[1]][[2]][[2]],ncol=1)
    s_bar <- mean(t)
    denominator <- sum((t-s_bar)^2)/nv
    t2 <- t1 <- cbind(t,c(1:nv))
    colnames(t1)<-c("si","i")
    colnames(t2)<-c("sj","j")
    w <- merge(merge(edges,t1),t2)
    numerator <- sum(w[,3]*(w[,4]-s_bar)*(w[,5]-s_bar))/ne
    simulated[z] <- numerator/denominator
  }
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Moran's I"
  #plot(gofb,key=c("Moran's I"))
  
  k2<-observed
  s2<-simulated
  mhp2<-p
  
  k <- gearyw2
  observed <- matrix(k,nrow=1)
  observations <- nrow(observed)
  n <- length(k)
  simulated <- matrix(rep(0,1000*n),nrow=1000)
  for (z in 1:1000) {
    edges <- ans$sims[[z]][[1]][[1]][[2]]
    ne <- nrow(edges)
    colnames(edges)<-c("i","j","xij")
    nv <- length(ans$sims[[z]][[1]][[2]][[2]])
    t <- matrix(ans$sims[[z]][[1]][[2]][[2]],ncol=1)
    s_bar <- mean(t)
    denominator <- 2*sum((t-s_bar)^2)/(nv-1)
    t2 <- t1 <- cbind(t,c(1:nv))
    colnames(t1)<-c("si","i")
    colnames(t2)<-c("sj","j")
    w <- merge(merge(edges,t1),t2)
    numerator <- sum(w[,3]*(w[,4]-w[,5])^2)/ne
    simulated[z] <- numerator/denominator
  }
  variates <- ncol(simulated)
  a <- cov(simulated)
  ainv <- ginv(a)
  expectation <- colMeans(simulated)
  centeredSimulations <- scale(simulated, scale=FALSE)
  centeredObservations <- observed - expectation
  mhd <- function(x)
  {
    x %*% ainv %*% x
  }
  simTestStat <- apply(centeredSimulations, 1, mhd)
  obsTestStat <- apply(centeredObservations, 1, mhd)
  #if (twoTailed)
  #{
  #	p <- sapply(1:observations, function (i)
  #				1 - abs(1 - 2 * sum(obsTestStat[i] <=
  #				simTestStat)/length(simTestStat)) )
  #}
  #else
  #{
  p <- sapply(1:observations, function (i)
    sum(obsTestStat[i] <= simTestStat) /length(simTestStat))
  #}
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- p
  attr(gofb,"auxiliaryStatisticName")<-"Geary's C"
  #plot(gofb,key=c("Geary's c"))
  
  k3<-observed
  s3<-simulated
  mhp3<-p
  
  k <- c(k1,k2,k3)
  observed <- matrix(k,nrow=1)
  observations <- nrow(observed)
  n <- length(k)
  simulated <- cbind(s1,s2,s3)
  
  load("gofb.Rdata")
  gofb$Joint$Simulations <- simulated 
  gofb$Joint$Observations <- observed
  gofb$Joint$p <- c(mhp1,mhp2,mhp3)
  attr(gofb,"auxiliaryStatisticName")<-"Behavior Similarity"
  plot6 <- plot(gofb,key=c("Edgewise Homophily","Moran's I","Geary's c"), main="network-behaviour")
  
  jpeg(paste('Imputation',imp,'.jpeg'), width=297, height=210, units="mm", res=1080)
  grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3)
  dev.off()
}

