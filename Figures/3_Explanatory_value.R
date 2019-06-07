rm(list = ls())
setwd("/home/eobrien/bde/Projects/Parametric_public/Analysis")

library(tidyr)
library(dplyr)
library(ggplot2)

df <- read.csv("PCA_components.csv")
df$v_mu <- 0.25 * (scale(df$v.6.) + scale(df$v.12.) + scale(df$v.24.) + scale(df$v.48.))

good_pa <- df %>% subset(ctopp_pa >= 100)


summary(lm(read ~ ctopp_rapid + v_mu + wasi_mr_ts, good_pa))


# What is the correlatio to report?
fit <- summary(lm(read ~ v_mu, good_pa))
label_text = paste0("\U1D633  = ", round(sqrt(fit$r.squared),2),", \U1D631 = ", round(fit$coefficients[2,4],4))

px1 <- ggplot(good_pa, aes(v_mu, read))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", colour = "gray30")+
  theme_bw()+
  xlab("Average drift rate")+
  ylab("Reading skill")+
  annotate("text",x = 0, y = 145, label = label_text, size = 5)+
  theme(text = element_text(size = 16),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
px1


ggsave("Motion_with_Good_PA.png", px1, height = 5, width = 5)

df$this_group = as.factor(ifelse(df$ctopp_pa >= 100, 1,0))
px2 <- ggplot(df, aes(ctopp_pa, read))+
  geom_point(size = 1, aes(colour = this_group))+
  theme_bw()+
  xlab("PA")+
  ylab("Reading skill")+
  theme(text = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")+
  scale_colour_manual(values = c("gray70","black"))+
  coord_fixed(ratio = 1)
px2
ggsave("inset.png", px2, height = 1.5, width = 1.5, bg = "transparent")

