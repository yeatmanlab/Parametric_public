# How do the parameters of the model fit relate to reading ability?
library(tidyr)
library(dplyr)
library(lme4)
library(pbkrtest)
rm(list = ls())

setwd("/home/eobrien/bde/Projects/Parametric_public/Analysis")

# Write this out to file
df <- read.csv("DDM_Fit_Results.csv")

df$group <- ifelse(df$read < 85, "Dyslexic",
                   ifelse(df$read >= 85 & df$dys_dx == 0, "Control",
                          "Other"))

df <- subset(df, group != "Other")


# some formatting
out <- df %>%
  gather(level, v, v.6.:v.48.)
out$level <-  gsub("[^0-9\\.]", "", out$level)
out$level <-  gsub("\\.", "", out$level)
out$level <- as.numeric(out$level)
out$group <- as.factor(out$group)

# Set group contrasts
group_dimnames <- list(levels(out$group),
                          levels(out$group)[2])
contrasts(out$group) <- matrix(c(-0.5, 0.5), nrow=2, dimnames=group_dimnames)



######################################################################################################################################
#############################
##### Linear modeling #####
out$adhd_dx <- as.logical(out$adhd_dx)
out$read <- scale(out$read, scale = TRUE)
out$wasi_mr_ts <- scale(out$wasi_mr_ts, scale=TRUE)
out$age_at_testing <- scale(out$age_at_testing, scale = TRUE)

out$log_level <- log2(out$level)
out$log_level <- scale(out$log_level, scale = TRUE)
out <- subset(out, adhd_dx != "NA" & wasi_mr_ts != "NA")

# Big model
lmfit <- lmer(v ~ group*log_level +  adhd_dx  + age_at_testing + wasi_mr_ts + (1|subj_idx),out)
summary(lmfit)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs


# Nuisance terms
lmfit_NUI1 <- lmer(v ~ group*log_level + adhd_dx + age_at_testing + (1|subj_idx),out)
lmfit_NUI2 <- lmer(v ~ group*log_level + wasi_mr_ts + age_at_testing + (1|subj_idx),out)
lmfit_NUI3 <- lmer(v ~ group*log_level + wasi_mr_ts + adhd_dx + (1|subj_idx),out)
anova(lmfit_NUI1, lmfit) # OK to remove wasi
anova(lmfit_NUI2, lmfit) # OK to remove adhd term
anova(lmfit_NUI3, lmfit) # NOT OK to remove age_at_testing


lmfit <- lmer(v ~ group*log_level + age_at_testing + (1|subj_idx),out)
summary(lmfit)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

##### Correlations with other parameters of the DDM model ##############
a_t_sum <- out %>% 
  dplyr::group_by(subj_idx) %>%
  dplyr::summarise(a = unique(a),
                   t = unique(t),
                   sz = unique(sz),
                   sv = unique(sv),
                   st = unique(st),
                   v_mu = mean(v),
                   adhd_dx = unique(adhd_dx),
                   group = unique(group),
                   wasi_mr_ts = unique(wasi_mr_ts),
                   age_at_testing = unique(age_at_testing))




### Test correlations with non-decision parameters ####
# No significant relationship with a
lmfit <- lm(a ~ group, a_t_sum)
summary(lmfit)


lmfit <- lm(sz ~ group,a_t_sum)
summary(lmfit)

lmfit <- lm(t ~ group + wasi_mr_ts + age_at_testing, a_t_sum)
summary(lmfit)

lmfit <- lm(st ~ group + age_at_testing, a_t_sum)
summary(lmfit)

