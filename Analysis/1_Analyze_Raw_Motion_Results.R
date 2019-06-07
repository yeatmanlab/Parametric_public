rm(list = ls())
library(lme4)
library(pbkrtest)

df <- read.csv("Clean_Motion_Data.csv")

########## First do it with continuous measures ###########################
#### Is there an effect of stimulus coherence and reading skill on accuracy?
subj_summary <- df %>%
  group_by(subj_idx, stim)%>%
  subset(stim != 100) %>%
  subset(rt < 10 & rt > 0.2) %>%
  subset(subj_idx != 1025 | subj_idx != 749)%>%
  summarise(acc = mean(response),
            rt = median(rt),
            read = unique(read),
            wasi_mr_ts = unique(wasi_mr_ts),
            adhd_dx = unique(adhd_dx),
            age_at_testing = unique(age_at_testing),
            dys_dx = unique(dys_dx))

subj_summary$stim <- log2(subj_summary$stim)

fit <- lmer(acc ~ read + stim + wasi_mr_ts + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
summary(fit)

fit2 <- lmer(acc ~ read + stim  + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
anova(fit,fit2) # OK to drop nonverbal IQ
summary(fit2)

fit3 <- lmer(acc ~ read + stim  + age_at_testing + (1|subj_idx), subj_summary)
anova(fit2,fit3) # OK to drop adhd

fit4 <- lmer(acc ~ stim  + age_at_testing + (1|subj_idx), subj_summary)
anova(fit3,fit4) # OK to drop adhd

lmfit <- fit4
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

##### How about on reaction time?
fit <- lmer(rt ~ read + stim + wasi_mr_ts + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
summary(fit)

fit2 <- lmer(rt ~ read + stim  + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
anova(fit,fit2) # OK to drop nonverbal IQ
summary(fit2)

fit3 <- lmer(rt ~ stim*read  + age_at_testing +  (1|subj_idx), subj_summary)
anova(fit2,fit3) # OK to drop adhd
lmfit <- fit3
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs


# Now check that the ratio of correct and error responses was related to reading skill
RT_med = df %>%
  group_by(response, subj_idx) %>%
  subset(rt < 10 & rt > 0.2) %>%
  subset(stim != 100) %>%
  summarise(rt = median(rt, na.rm = TRUE),
            age_at_testing = unique(age_at_testing),
            read = unique(read),
            wasi_mr_ts = unique(wasi_mr_ts),
            adhd_dx = unique(adhd_dx))

RT_med <- RT_med %>% spread(response, rt)
colnames(RT_med)[6:7] <- c("Error_RT","Correct_RT")
# Get the ratio of median error time to median correct time
RT_med$ratio <- RT_med$Error_RT/RT_med$Correct_RT

ggplot(RT_med, aes(read, ratio)) + geom_point()

fit <- lm(ratio ~ read +age_at_testing, RT_med)
summary(fit)


##################################################################################33
########### Now do it with group measures ##################3
subj_summary$group <- ifelse(subj_summary$read < 85, "Dyslexic",
                             ifelse(subj_summary$read >= 85 & subj_summary$dys_dx == 0,"Control",
                             "Other"))

subj_summary <- subset(subj_summary, group != "Other")

# Accuracy
fit <- lmer(acc ~ group + stim + wasi_mr_ts + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
summary(fit)

fit2 <- lmer(acc ~ group + stim  + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
anova(fit,fit2) # OK to drop nonverbal IQ
summary(fit2)

fit3 <- lmer(acc ~ group + stim  + age_at_testing + (1|subj_idx), subj_summary)
anova(fit2,fit3) # OK to drop adhd
lmfit <- fit3
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs

##################3###########RT######################
fit <- lmer(rt ~ group + stim + wasi_mr_ts + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
summary(fit)

fit2 <- lmer(rt ~ group + stim  + adhd_dx + age_at_testing + (1|subj_idx), subj_summary)
anova(fit,fit2) # OK to drop nonverbal IQ
summary(fit2)

fit3 <- lmer(rt ~ group + stim  + age_at_testing + (1|subj_idx), subj_summary)
anova(fit2,fit3) # OK to drop adhd

fit4 <- lmer(rt ~ group + stim + (1|subj_idx), subj_summary)
anova(fit3,fit4) # Don't want to drop age

lmfit <- fit3
coefs <- data.frame(coef(summary(lmfit)))
df.KR <- get_ddf_Lb(lmfit, fixef(lmfit))
coefs$p.KR <- 2*(1-pt(abs(coefs$t.value), df.KR))
coefs
