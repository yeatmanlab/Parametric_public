# How do the parameters of the model fit relate to reading ability?
library(tidyr)
library(dplyr)
library(lme4)
library(pbkrtest)
rm(list = ls())

setwd("/home/eobrien/bde/Projects/Parametric_public/Analysis")

#### Read in all of the parameters from files #########
## load the raw response data
param_df <- data.frame()
data_dir <- file.path("..","Motion","Exp", "Data","Processed","Fit_Files")
raw_files <- list.files(path=data_dir)
opts <- options(warn=2)  # convert warnings to errors, while reading in files
for (fname in raw_files) {
  ## skip=1 because first row of each file is a timestamp
  df_tmp <- tryCatch(read.csv(file.path(data_dir, fname)),
                     error=function(e) {print(paste("skipping", fname, 
                                                    conditionMessage(e))); e})
  if(inherits(df_tmp, "error")) next
  ## only keep complete blocks
    
    # Get the 
    ## concatenate with other files
  names(df_tmp) <- c("X","a","v..1.5","v..0.5","v.0.5","v.1.5.","t","sv","sz","st","subj_idx")
  param_df <- rbind(param_df, df_tmp)
}
options(opts)  # restore default options

#################################################################################################################################
########## First outlier detection to get rid of any individuals who could not be well-fit by the DDM model, which happens on occasion #
# Mahalanobis outlier detection first to identify any subjects that 
m_dist <- mahalanobis(param_df[,2:10], colMeans(param_df[,2:10]),
                      cov(param_df[,2:10]))

param_df$m_dist <- m_dist

# Use the chi-squared distribution critical value, alpha = 0.001
alpha = 0.001
threshold <- qchisq(1-alpha, df = 9)
param_df$outlier <- ifelse(param_df$m_dist > threshold, 1,0)
print(paste0(sum(param_df$outlier)," outliers found."))


############################################3
######### FORMATTING ########################
param_df <- param_df %>%
  subset(outlier != 1) %>%
  dplyr::select(-c("outlier","m_dist"))

# Column names
params <- param_df
colnames(params) <- c("X","a","v.6.","v.12.","v.24.","v.48.","t","sv","sz","st","subj_idx") #

# Merge with biographical data
df <- read.csv("Clean_Motion_Data.csv")
bio <- df
bio[c("X","trial","stim","direction","response","rt","block","timestamp","block_id")] <- NULL
bio <- unique( bio[ , 1:length(bio) ] )
out <- merge(params, bio, by = "subj_idx")

# Write this out to file
write.csv(out, "DDM_Fit_Results.csv")


# some formatting
out <- out %>%
  gather(level, v, v.6.:v.48.)
out$level <-  gsub("[^0-9\\.]", "", out$level)
out$level <-  gsub("\\.", "", out$level)
out$level <- as.numeric(out$level)





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
lmfit <- lmer(v ~ read*log_level + wasi_mr_ts + adhd_dx + age_at_testing + (1|subj_idx),out)
summary(lmfit)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs


# Nuisance terms
lmfit_NUI1 <- lmer(v ~ read*log_level + adhd_dx + age_at_testing + (1|subj_idx),out)
lmfit_NUI2 <- lmer(v ~ read*log_level + wasi_mr_ts + age_at_testing + (1|subj_idx),out)
lmfit_NUI3 <- lmer(v ~ read*log_level + wasi_mr_ts + adhd_dx + (1|subj_idx),out)
anova(lmfit_NUI1, lmfit) # OK to remove wasi
anova(lmfit_NUI2, lmfit) # OK to remove adhd term
anova(lmfit_NUI3, lmfit) # NOT OK to remove age_at_testing


lmfit <- lmer(v ~ read*log_level + age_at_testing + (1|subj_idx),out)
summary(lmfit)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

##### Correlations with other parameters of the DDM model ##############
a_t_sum <- out %>% 
  dplyr::group_by(subj_idx) %>%
  subset(level == 24)%>%
  dplyr::summarise(a = unique(a),
                   t = unique(t),
                   sz = unique(sz),
                   sv = unique(sv),
                   st = unique(st),
                   v.24. = unique(v),
                   wj_brs = unique(wj_brs),
                   adhd_dx = unique(adhd_dx),
                   read = unique(read),
                   wasi_mr_ts = unique(wasi_mr_ts),
                   age_at_testing = unique(age_at_testing),
                   ctopp_pa = unique(ctopp_pa),
                   ctopp_rapid = unique(ctopp_rapid))




### Test correlations with non-decision parameters ####
# No significant relationship with a
lmfit <- lm(a ~ read + wasi_mr_ts + age_at_testing + adhd_dx, a_t_sum)
summary(lmfit)


lmfit <- lm(a ~read,  a_t_sum)
summary(lmfit)

lmfit <- lm(sz~ read, a_t_sum)
summary(lmfit)

lmfit1 <- lm(t ~ age_at_testing, a_t_sum)
lmfit2 <- lm(t ~ read + age_at_testing, a_t_sum)
anova(lmfit2,lmfit1)
summary(lmfit1)

lmfit <- lm(st ~ read  + age_at_testing, a_t_sum)
summary(lmfit)

ggplot(a_t_sum, aes(read, t))+geom_point()

library(boot) #load the package
# Now we need the function we would like to estimate
# In our case the beta:
betfun = function(data,b){  
  # b is the random indexes for the bootstrap sample
  d = data[b,] 
  return(lm(sz ~ a, data = d)$coef[2])  
  # thats for the beta coefficient
}
# now you can bootstrap:
bootbet = boot(data=out, statistic=betfun, R=5000) 



lmfit <- lm(read ~ t + wasi_mr_ts, a_t_sum)
summary(lmfit)

#### Are these parameters correlated with one another? ####
summary(lm(a ~ sz, a_t_sum))
summary(lm(a ~ v.48., a_t_sum))
summary(lm(sz ~ v.48., a_t_sum))

lmfit <- lmer(v ~ a +(1|subj_idx), out)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

lmfit <- lmer(v ~ sz +(1|subj_idx), out)
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

# What if you used a model of all these predictors
lmfit3 <- lm(read ~ sz + a + wasi_mr_ts + ctopp_rapid + v.24. + ctopp_pa, a_t_sum)
summary(lmfit3) 


#################################################################
##### Does sz correlated with propensity to make fast errors? ###
RT_med = df %>%
  group_by(response, subj_idx) %>%
  subset(rt < 10 & rt > 0.2) %>%
  subset(stim != 100) %>%
  summarise(rt = median(rt, na.rm = TRUE))

RT_med <- RT_med %>% spread(response, rt)
colnames(RT_med)[2:3] <- c("Error_RT","Correct_RT")
# Get the ratio of median error time to median correct time
RT_med$ratio <- RT_med$Error_RT/RT_med$Correct_RT
RT_med <- merge(RT_med, a_t_sum, all = TRUE)

# Check that the ratio is correlated with sz
summary(lm(ratio ~ sz, RT_med))

# Check whether the ratio is correlated with reading score
summary(lm(read ~ sz, RT_med))


