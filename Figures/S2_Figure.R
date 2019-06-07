# Compare correct/error response times as a function of group
library(dplyr)
library(ggplot2)
library(cowplot)
rm(list = ls())

# Load in the clean data
df <- read.csv('../Analysis/Clean_Motion_Data.csv')
df <- df %>% 
  subset(rt > 0.2 & rt < 10) %>%
  subset(stim != 100) 

df$group <- ifelse(df$read < 85, "Dyslexic",
                          ifelse(df$read >= 85 & df$dys_dx == 0,"Control",
                                 "Other"))
df <- subset(df, group != "Other")

# Make separate columns for rts that are correct and incorrect
correct <- df %>%
  subset(response == 1)
correct$rt_correct <- correct$rt

error <- df %>%
  subset(response != 1)
error$rt_error <- error$rt

sum_df <- merge(correct,error, all = TRUE)
sum_df$stim <- as.vector(sum_df$stim)


# Color palettes
cmap_dys <- rev(colorRampPalette(c("royalblue3","white"))(8))
cmap_aa <- rev(colorRampPalette(c("firebrick3","white"))(8))

peaks <- df %>%
  dplyr::group_by(group, response,stim)%>%
  summarize(peak = median(rt))

pks <- peaks$peak

sum_df$stim <- factor(sum_df$stim, levels = c("6","12","24","48"))

#levels(sum_df$stim) <- c("6%","12%","24%","48%")
#sum_df$stim <- as.numeric(sum_df$stim)
px1 <- ggplot(subset(sum_df, group == "Dyslexic"), aes(rt, colour = stim, fill = stim))+
  scale_colour_manual(values = cmap_dys[4:8], name = "Coherence")+
  scale_fill_manual(values = cmap_dys[4:8], name = "Coherence")+
  geom_density(alpha = 0.5, aes(x=rt_correct, y = (..density..)))+
  geom_density(alpha = 0.5, aes(x=rt_error, y = -(..density..)))+
  coord_cartesian(ylim = c(-1.3, 1.3),xlim = c(0,3)) +
  ylab("Density")+
  #annotate("text",x = pks[2],y=1.3, label = "*", size = 10)+
  #annotate("text", x = pks[1], y = -.8, label = "*",size = 10)+
  #annotate("segment", x = pks[2], xend = pks[1], y = -, yend = -2)+
  theme_bw()+
  xlab("")+
  ggtitle("Dyslexic Readers")
px1

px2 <- ggplot(subset(sum_df, group == "Control"), aes(rt, colour = stim, fill = stim))+
  scale_colour_manual(values = cmap_aa[4:8], name = "Coherence")+
  scale_fill_manual(values = cmap_aa[4:8], name = "Coherence")+
  geom_density(alpha = 0.5, aes(x=rt_correct, y = (..density..)))+
  geom_density(alpha = 0.5, aes(x=rt_error, y = -(..density..)))+
  coord_cartesian(ylim = c(-1.3, 1.3), xlim = c(0,3)) +
  ylab("Density")+
  #annotate("text",x = pks[4],y=1.3, label = "*", size = 10)+
  #annotate("text", x = pks[3], y = -.8, label = "*",size = 10)+
  theme_bw()+
  xlab("")+
  ggtitle("Control Readers")
px2

prow <- plot_grid( px1,
                   px2,
                   align = 'vh',
                   hjust = -0.5,
                   nrow = 2,
                   labels = c("A","B")
)

prow
prow <- ggdraw(add_sub(prow, "Reaction Time (s)", y = 1, size = 12))
prow



px3 <- ggplot(subset(sum_df, stim == "12"), aes(rt,colour = group, fill = group))+
  geom_density(alpha = 0.5, aes(x=rt_correct, y = (..density..)))+
  geom_density(alpha = 0.5, aes(x=rt_error, y = -(..density..)))+
  scale_fill_manual(values = c("royalblue3", "firebrick3"), name = "Group")+
  scale_colour_manual(values = c("royalblue3", "firebrick3"), name = "Group")+
  xlab("Reaction time (s)")+
  ylab("Density")+
  theme_bw()+
  ggtitle("Reaction times\nat 12% coherence")
px3

pr2 <- plot_grid(prow, px3, labels = c("","C"))
pr2

ggsave("figure_rt_distributions.pdf", pr2,
       device=cairo_pdf, width=9, height=5)
ggsave("figure_rt_distributions.png", pr2,
       width = 9, height = 5, dpi = 300)

write.csv(sum_df,"Group_RTs.csv", row.names = FALSE)

## Make a new, positive only plot
px3 <- ggplot(subset(sum_df, stim == "12"), aes(rt,colour = group, fill = group))+
  geom_density(alpha = 0.5, aes(x=rt_correct, y = (..density..)))+
  scale_fill_manual(values = c("firebrick3","royalblue3"), name = "Group")+
  scale_colour_manual(values = c("firebrick3","royalblue3"), name = "Group")+
  xlab("Reaction time (s)")+
  ylab("Density")+
  theme_void()+
  coord_cartesian(ylim = c(0, 1.3), xlim = c(0,5))+
  theme(text = element_text(size = 24))
px3

ggsave("RT_distribution.png", px3)
