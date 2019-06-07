# Analyze the quality of fits associated with the various models
library(ggplot2)
library(dplyr)
library(lme4)
library(reshape2)
rm(list = ls())
setwd("/home/eobrien/bde/Projects/Parametric_public/Motion/Exp/Data/Processed")

df <- read.csv("DDM_Fit_AIC_BIC.csv")
names(df) <- c("X","Full__AIC","Full__BIC","sz__AIC","sz__BIC","sv__AIC","sv__BIC","st__AIC","st__BIC",
               "st_sv__AIC","st_sv__BIC","st_sz__AIC", "st_sz__BIC","sv_sz__AIC","sv_sz__BIC","no__AIC",
               "no__BIC","subj_idx")

df <- subset(df, !(subj_idx %in% c(1025, 749)))

# Normalize each relative to the AIC/BIC with the null model
aic <- df[ , grepl( "AIC" , names( df ) ) ]
bic <- df[ , grepl( "BIC" , names( df ) ) ]



aic[1:ncol(aic)] <- aic[1:ncol(aic)]-aic[,8]
bic[1:ncol(bic)] <- bic[1:ncol(bic)]-bic[,8]



mu_aic <- colMeans(aic)
se_aic <- apply(aic, 2, sd)/sqrt(nrow(aic))
mu_bic <- colMeans(bic)
se_bic <- apply(bic, 2, sd)/sqrt(nrow(bic))


aic_results_df <- data.frame(model= names(aic),
                             mean = mu_aic,
                             se = se_aic,
                             measure = "AIC")
bic_results_df <- data.frame(model= names(bic),
                             mean = mu_bic,
                             se = se_bic,
                             measure = "BIC")

results_df <- rbind(aic_results_df, bic_results_df)

split <- colsplit(results_df$model, "__", c("model","measure"))
results_df$model <- split$model
results_df$measure <- split$measure

ggplot(results_df, aes(model, mean))+
  geom_point()+
  facet_grid(~measure)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  theme_bw()


aic_long <- aic %>%
  spread()
ggplot(aic, aes(model, mean))+
  geom_jitter()+
  facet_grid(~measure)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  theme_bw()



# For each subject, which model is best?
best_model = list()
for (i in 1:nrow(df)){
  subj <- df[i,2:11]
  subj <- dplyr::select(subj, -contains("BIC"))
  
  min_model <- col(subj)[subj==min(subj)]
  best_model = c(best_model, names(subj)[min_model])
  
}

table(unlist(best_model))
