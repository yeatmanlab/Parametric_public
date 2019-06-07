# This figure makes scatter plots of reaction times and accuracy at each stimulus level
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)
rm(list = ls())


# Load in the clean data
df <- read.csv('../Analysis/Clean_Motion_Data.csv')


## Remove any trials that violate our exemption rules- too fast or too slow
df <- df %>% 
  subset(rt > 0.2 & rt < 10) %>%
  subset(stim < 100)

## ## ## ## ## ## ##
## COLORMAP STUFF ##
## ## ## ## ## ## ##
## RED-PURPLE-BLUE
cmap <- rev(colorRampPalette(c("royalblue3", "firebrick3"))(3))


subj_agg <- df %>%
  group_by(subj_idx, stim) %>%
  summarise(
            read=unique(read),
            response=mean(response, na.rm=TRUE),
            rt = median(rt, na.rm = TRUE)) %>%
  gather(.,type,measure,response:rt) 


ggplot(subj_agg, aes(read,measure))+
  facet_wrap(type~stim, nrow = 2)+
  theme_bw()+
  geom_point()

subj_agg$stim <- as.factor(subj_agg$stim)
levels(subj_agg$stim) <- c("6%", "12%","24%","48%")

px1 <- ggplot(subset(subj_agg, type == "rt"), aes(read,measure))+
  geom_point(aes(colour = stim))+
  #scale_colour_brewer(palette = "Greens")+
  scale_colour_manual(values = brewer.pal(5,"Greens")[2:5])+
  facet_wrap(~stim, nrow = 1)+
  geom_smooth(method = "lm", colour = "gray50")+
  theme_bw()+
  ylab("Reaction time (s)")+
  xlab("")+
  theme(axis.title.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 18, face = "bold"))+
  scale_x_continuous(breaks = c(60,80,100,120))
px1

px2 <- ggplot(subset(subj_agg, type == "response"), aes(read,measure))+
  geom_point(aes(colour = stim))+
  scale_colour_manual(values = brewer.pal(5,"Reds")[2:5])+
  facet_wrap(~stim, nrow = 1)+
  geom_smooth(method = "lm", colour = "gray50")+
  theme_bw()+
  ylab("Accuracy")+
  xlab("")+
  theme(axis.title.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 18, face = "bold"))+
  scale_x_continuous(breaks = c(60,80,100,120))
px2

prow <- plot_grid( px1 + theme(legend.position="none"),
                   px2 + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -0.5,
                   nrow = 2
)

prow

prow <- ggdraw(add_sub(prow, "Reading Score", y = 1, size = 16))
prow

## SAVE TO FILE
#setwd('./Figures')
ggsave("figure_indiv_rt_and_resp.pdf", prow,
       device=cairo_pdf, width=8, height=6)
ggsave("figure_indiv_rt_and_resp.png", prow,
       width = 8, height = 6, dpi = 300)


