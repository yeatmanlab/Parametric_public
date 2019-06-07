################ MEDIATION ANALYSIS ##############################
rm(list = ls())
setwd("/home/eobrien/bde/Projects/Parametric_public/Analysis")

library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(mediation)

df <- read.csv("PCA_components.csv")
#df <- subset(df, PC_M_sensory < 3)

############## DOES PHONOLOGICAL AWARENESS MEDIATE SPEECH --> READ RELATIONSHIP?
########################################################################################
# STEP ONE - DOES SPEECH PREDICT READING
model.0 <- lm(read ~ PC_M_decision + wasi_mr_ts, df)
summary(model.0) # There is a significant effect here


# STEP TWO - DOES SPEECH PREDICT PA
model.M <- lm(ctopp_pa ~ PC_M_decision + wasi_mr_ts, df)
summary(model.M) # Yep

# STEP THREE - DO SPEECH AND PA s READING
model.Y<- lm(read ~ ctopp_pa + PC_M_decision + wasi_mr_ts , df)
summary(model.Y)
anova(model.Y)

model.Y2<- lm(read ~ ctopp_pa +PC_M_decision, df)
SSq <- anova(model.Y2)$`Sum Sq`
total_SSq <- sum(SSq)
SSq/total_SSq


#### Is mediation statistically significant? ####
set.seed(15)
results <- mediate(model.M,  model.Y, treat = 'PC_M_decision', mediator = 'ctopp_pa',boot = TRUE,
                   sims = 5000, boot.ci.type = "bca" )
summary(results)

##############################################################################################
####################### PC_M_sensory mediate PA ####################################################
# STEP ONE - DOES M_sensory PREDICT READING
model.0 <- lm(read ~ PC_M_sensory + wasi_mr_ts, df)
summary(model.0) # There is a significant effect here

# STEP TWO - DOES SPEECH PREDICT PA
model.M <- lm(ctopp_pa ~ PC_M_sensory + wasi_mr_ts, df)
summary(model.M) # Yep


# STEP THREE - DO SPEECH AND PA PREDICT READING
model.Y<- lm(read ~ ctopp_pa +PC_M_sensory + wasi_mr_ts, df)
summary(model.Y)


model.Y2<- lm(read ~ ctopp_pa +PC_M_sensory_nosv, df)
SSq <- anova(model.Y2)$`Sum Sq`
total_SSq <- sum(SSq)
SSq/total_SSq

#### Is mediation statistically significant? ###
results <- mediate(model.M, model.Y, treat = 'PC_M_sensory', mediator = 'ctopp_pa',
                   boot=TRUE, sims = 5000, boot.ci.type = "bca" )
summary(results)

##########################################################################################################
########################### DOES PC_M_DECISION mediate PA ###############################################
# STEP ONE - DOES PC_M_DECISION PREDICT READING
model.0 <- lm(read ~ a + wasi_mr_ts, df)
summary(model.0) # There is a significant effect here

# STEP TWO - DOES SPEECH PREDICT PA
model.M <- lm(ctopp_pa ~ sz, df)
summary(model.M) # No - phonological awareness and decision-making aren't correlated once IQ is controlled for

# STEP THREE - DO SPEECH AND PA PREDICT READING
model.Y<- lm(read ~ ctopp_pa + PC_M_decision + wasi_mr_ts , df)
summary(model.Y)


SSq <- anova(model.Y)$`Sum Sq`
total_SSq <- sum(SSq)
SSq/total_SSq

#### Is mediation statistically significant? ####
set.seed(15)
results <- mediate(model.M, model.Y, treat = 'PC_M_decision', mediator = 'ctopp_pa',
                   boot=TRUE, sims = 1000, boot.ci.type = "bca" )
summary(results)

################################################################################################################
################################################################################################################
##### RAN ##############
########################
# STEP ONE - DOES SPEECH PREDICT READING
model.0 <- lm(read ~ st + wasi_mr_ts, df)
summary(model.0) # There is a significant effect here

# STEP TWO - DOES SPEECH PREDICT PA
model.M <- lm(ctopp_rapid ~ st + wasi_mr_ts, df)
summary(model.M) # Yep

# STEP THREE - DO SPEECH AND PA PREDICT READING
model.Y<- lm(read ~ ctopp_rapid + st + wasi_mr_ts, df)
summary(model.Y)


SSq <- anova(model.Y)$`Sum Sq`
total_SSq <- sum(SSq)
SSq/total_SSq


#### Is mediation statistically significant? ####
results <- mediate(model.M, model.Y, treat = 'st', mediator = 'ctopp_rapid',
                   boot=TRUE, sims = 4000, boot.ci.type = "bca" )
summary(results)

#######################################################################################
############ M_sensory next ###################
# STEP ONE - DOES M_sensory PREDICT READING
model.0 <- lm(read ~ PC_M_sensory + wasi_mr_ts, df)
summary(model.0) # There is a significant effect here

# STEP TWO - DOES SENSORY/MOTION PREDICT PA 
model.M <- lm(ctopp_rapid ~ PC_M_sensory + wasi_mr_ts, df)
summary(model.M) # NO - THEY ARE NOT CORRELATED

# STEP THREE - DO SPEECH AND PA PREDICT READING
model.Y<- lm(read ~ PC_M_sensory +ctopp_rapid + wasi_mr_ts , df)
summary(model.Y)

#### Is mediation statistically significant? ####
results <- mediate(model.M, model.Y, treat = 'PC_M_sensory', mediator = 'ctopp_rapid',
                   boot=TRUE, sims = 4000, boot.ci.type = "bca" )
summary(results)

###############################################################################################
##################### M decision ##############################################################
# STEP ONE - DOES M_decision PREDICT READING
model.0 <- lm(read ~ PC_M_decision + wasi_mr_ts, df)
summary(model.0) # There is a significant effect here

# STEP TWO - DOES SPEECH PREDICT RAPID
model.M <- lm(ctopp_rapid ~ PC_M_decision + wasi_mr_ts, df)
summary(model.M) # NO #

# STEP THREE - DO SPEECH AND RAPID PREDICT READING
model.Y<- lm(read ~ PC_M_decision + ctopp_rapid + wasi_mr_ts, df)
summary(model.Y)

#### Is mediation statistically significant? ####
results <- mediate(model.M, model.Y, treat = 'PC_M_decision', mediator = 'ctopp_rapid',
                   boot=TRUE, sims = 500)
summary(results)
