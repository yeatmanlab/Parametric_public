############# FACTOR ANALYSIS #####################
rm(list = ls())
setwd("/home/eobrien/bde/Projects/Parametric_public/Analysis")

library(tidyr)
library(dplyr)
library(ggplot2)
library(DescTools)
library(stats)
library(nFactors)
library(data.table)

df_orig <- read.csv("PCA_components.csv")

df_psy <- df_orig %>%
  dplyr::select(c("v.6.","v.12.","v.24.","v.48.","a","t","sv","st","sz","wasi_mr_ts",
                  "ctopp_elision_ss","ctopp_bw_ss","ctopp_pi_ss","ctopp_md_ss","ctopp_nr_ss","ctopp_rd_ss","ctopp_rl_ss"
                  )) %>%
  na.omit(.)

##### How many factors should we be looking for?
ev <- eigen(cor(df_psy))
ap <- parallel(subject = nrow(df_psy), var = ncol(cor(df_psy)), rep = 1000)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# The winner is 4!
fit <- factanal(df_psy,
                4,
                scores = "Bartlett",
                rotation = "varimax")
fit

big_df <- cbind(df_psy, fit$scores)
big_df <- merge(df_orig, big_df)


summary(lm(read ~ Factor1 + Factor2 + Factor3 + Factor4, big_df))



# PLOT IT #
loadings <- fit$loadings[,1:4]
d <- loadings

d <- setDT(as.data.frame(d), keep.rownames = TRUE)[]
long_d <- d %>% gather(Component, Loading, -rn) %>%
  subset(Component %in% c("Factor1","Factor2","Factor3","Factor4","Factor5")) 
levels(long_d$Component) <- c("Factor 1", "Factor 2", "Factor 3","Factor 4")
long_d$Variable <- factor(long_d$rn, levels = c("a","t","sz","st","sv","v.6.","v.12.","v.24.","v.48.",
                                               "PC_S","in_category_rt",
                                               "wasi_mr_ts",
                                               "ctopp_rl_ss","ctopp_rd_ss","ctopp_pi_ss","ctopp_nr_ss",
                                               "ctopp_md_ss","ctopp_elision_ss","ctopp_bw_ss"))
long_d$Component<- as.factor(long_d$Component)
long_d$variance <- long_d$Loading^2

px <- ggplot(data = long_d, aes(x=Variable, y=Component, fill=abs(Loading))) + 
  geom_tile(colour = "white") +
  geom_text(aes(label=round(Loading,2)), size = 4) +
  scale_x_discrete(labels=c("a","t","sz","st","sv",
                            expression(v[6]),expression(v[12]), expression(v[24]), expression(v[48]),
                            "Nonverbal IQ",
                            "CTOPP Rapid Letters", "CTOPP Rapid Digits",
                            "CTOPP Phoneme Isolation","CTOPP Nonword Repetition",
                            "CTOPP Memory for Digits","CTOPP Elision","CTOPP Blending Words")
  )+
  guides(fill=guide_legend(title="% of\nComponent"))+
  geom_segment(aes(x = 9.5, xend = 9.5, y = 0.5, yend = 4.5), colour = "gray50",size = 1)+
  geom_segment(aes(x = 10.5, xend = 10.5, y = 0.5, yend = 4.5), colour = "gray50",size = 1)+
  geom_segment(aes(x = 12.5, xend = 12.5, y = 0.5, yend = 4.5), colour = "gray50",size = 1)+
  scale_fill_continuous(low = "white",high = "#B1B695",  name = "Loading")+ # "#368F8B",
  theme_classic()+
  theme(text = element_text(size = 16),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  xlab("")+
  ylab("")
px

ggsave("Factor_Loadings.png", px, height = 5, width = 8)
ggsave("Factor_Loadings.eps", px, height = 5, width = 8)
#big_df$read <- scale(big_df$read)
summary(lm(read ~ Factor2, big_df))

# Make some bar charts
ggplot(big_df, aes(Factor1, Factor4)) +
  geom_point(aes(colour = read), size = 2) 

big_df$group <-ifelse(big_df$read < 85, "Dyslexic",
                      ifelse(big_df$read >=85 & big_df$dys_dx == 0, "Control",
                             "Other"))


#### USING LOO-CV, look at the predictions of the model
df <- big_df
k = nrow(df)
df$fold <- cut(seq(1,nrow(df)),breaks=k,labels=FALSE)

############ TWO FACTOR MODEL
acc_vec = vector()
pred = vector()
for (i in 1:k){
  qda_fit <- lm(read ~ Factor2, data = subset(df, fold != i))
  yhat <- predict(qda_fit, subset(df, fold == i))
  ytrue <- df %>% subset(fold ==i) %>% dplyr::select(read)
  acc_vec[i] = (unname(yhat) - ytrue$read)^2
  pred[i] = unname(yhat)
  
}
mean(acc_vec)
df$yhat1 <- pred

#### What is the r2?
tss <- sum( (df$read - mean(df$read))^2 )
rss <- sum( (df$read - df$yhat1)^2) 

sfr2 <- 1 - (rss/tss)


######### FOUR FACTOR MODEL
acc_vec = vector()
pred = vector()
for (i in 1:k){
  qda_fit <- lm(read ~ Factor1 + Factor2 + Factor3 + Factor4, data = subset(df, fold != i))
  yhat <- predict(qda_fit, subset(df, fold == i))
  ytrue <- df %>% subset(fold ==i) %>% dplyr::select(read)
  acc_vec[i] = (unname(yhat) - ytrue$read)^2
  pred[i] = unname(yhat)
  
}
mean(acc_vec)
df$yhat4 <- pred

#### What is the r2?
tss <- sum( (df$read - mean(df$read))^2 )
rss <- sum( (df$read - df$yhat4)^2) 

ffr2 <- 1 - (rss/tss)
printr <- round(ffr2,2)

df_pred <- df %>%
  dplyr::select(c("read","yhat1","yhat4"))

df_pred_g <- df_pred %>%
  gather(nfact, pred, -read)

#### What is the LOO-CV r2?
text_label <- as.character(as.expression(paste("LOO-CV R^2 == ", printr)))

l <- list(r2 = printr,
          pval = 0.01)
eq <- substitute("LOO-CV "*italic(R)^2 == r2,l)
eqstr4 <- as.character(as.expression(eq))
px2 <- ggplot(subset(df_pred_g, nfact == "yhat4"), aes(read,pred))+
  geom_point()+
  geom_abline()+
  xlab("True reading skill")+
  ylab("Predicted reading skill")+
  coord_fixed(ratio = 1)+
  xlim(c(50,135))+
  ylim(c(50,135))+
  annotate(geom="text", x=70,y=130, label = eqstr4, parse = TRUE, size = 4)+
  theme_bw()+
  theme(text = element_text(size = 16),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
px2  
  
ggsave("Model_predictions_four_factor.png", px2, height = 4, width = 4)


############ DO IT FOR THE TWO FACTOR MODEL
l <- list(r2 = round(sfr2,2),
          pval = 0.01)
eq <- substitute("LOO-CV "*italic(R)^2 == r2,l)
eqstr <- as.character(as.expression(eq))
px3 <- ggplot(subset(df_pred_g, nfact == "yhat1"), aes(read,pred))+
  geom_point()+
  geom_abline()+
  xlab("True reading skill")+
  ylab("Estimated reading skill")+
  coord_fixed(ratio = 1)+
  xlim(c(50,135))+
  ylim(c(50,135))+
  annotate(geom="text", x=70,y=130, label = eqstr, parse = TRUE, size = 4)+
  theme_bw()+
  theme(text = element_text(size = 16),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
px3

ggsave("Model_predictions_one_factor.png", px3, height = 4, width = 4)

########### Compare the model predictions
df_pred$f1_resid = abs((df_pred$yhat1 - df_pred$read))
df_pred$f4_resid = abs((df_pred$yhat4 - df_pred$read))
df_pred$mdif <- df_pred$f4_resid - df_pred$f1_resid

px4 <- ggplot(df_pred, aes(read, yhat4))+
  geom_point(aes(colour = mdif), size = 2)+
  geom_abline()+
  xlab("True reading skill")+
  ylab("Predicted reading skill")+
  coord_fixed(ratio = 1)+
  xlim(c(50,135))+
  ylim(c(50,135))+
  annotate(geom="text", x=75,y=130, label = eqstr4, parse = TRUE, size = 5)+
  theme_bw()+
  theme(axis.title = element_text(size = 16),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom",
        panel.background = element_rect(colour = "transparent"))+
  scale_colour_gradient2(low = "#008977", mid = "gray80",high = "#890089", midpoint = 0,
                         limits = c(-22,22),
                         breaks = c(-20,20),
                         labels = c("-20\n(4 factor best)","20\n(1 factor best)"))+
  guides(colour = guide_colorbar(title = "Difference in CV error",
                                 title.position = "top", title.hjust = 0.5, barwidth = 15))
px4


ggsave("Model_predictions_four_factor_color.png", px4, height = 5, width = 5,
       bg ="transparent")
ggsave("Model_predictions_four_factor_color.eps", px4, height = 5, width = 5,
       bg ="transparent")
# Compare AIC and BIC for reduced and full model
full <- lm(read ~ Factor1 + Factor2 + Factor3 + Factor4, big_df)
single <- lm(read ~ Factor2, big_df)

AIC(full)
AIC(single)
BIC(full)
BIC(single)
