# Compare correct/error response times as a function of group
library(dplyr)
library(ggplot2)
library(cowplot)
library(MASS)
rm(list = ls())

# Load in the clean data
df <- read.csv('HBN_dataset.csv')
df <- subset(df, dx != "No Dyslexia DX")
df <- droplevels(df)
levels(df$dx) <- c("Dyslexic","Control")

palette = c("royalblue3","firebrick3")
p1 <- ggplot(df, aes(x=CTOPP_EL_S, fill = dx))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("Phonological Awareness\nCTOPP Elision")+
  ylab("Density")+
  theme(text = element_text(size = 16))+
  scale_fill_manual(values = palette, name = "Group")
p1

p2 <- ggplot(df, aes(x=CTOPP_RSN_Comp, fill = dx))+
  geom_density(alpha = 0.5)+
  theme_bw()+
  xlab("Automaticity\nCTOPP Rapid Symbolic Naming")+
  ylab("Density")+
  theme(text = element_text(size = 16))+
  scale_fill_manual(values = palette, name = "Group")
p2

###############################################################
# Make the QDA classifier #
qda_fit <- qda(dx ~ CTOPP_RSN_Comp + CTOPP_EL_S, df)

# Make a grid of points
pa_range <- seq(0,17,0.05)
ran_range <- seq(40,140,0.5)

mygrid <- expand.grid(pa_range, ran_range)
mygrid <- as.data.frame(mygrid)
names(mygrid) <- c("CTOPP_EL_S","CTOPP_RSN_Comp")

mygrid$dx<- predict(qda_fit, mygrid)$class

p3 <- ggplot(mygrid, aes(x=CTOPP_EL_S, y = CTOPP_RSN_Comp))+
  geom_tile(aes(fill = dx), alpha = 0.5)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme_classic()+
  scale_fill_manual(values = palette, name = "Group")+
  scale_color_manual(values = palette, name = "Group")+
  theme(text = element_text(size = 16))+
  geom_jitter(data = df, aes(x = CTOPP_EL_S, y = CTOPP_RSN_Comp, colour = dx), size = 2)+
  xlab("Phonological Awareness")+
  ylab("Automaticity")
p3  

prow <- plot_grid( p1 + theme(legend.position = "none"),
                   p2 + theme(legend.position = "none"),
                   p3 + theme(legend.position = "none"),
                   align = 'vh',
                   labels = c("A", "B","C"),
                   label_size = 24,
                   hjust = -0.5,
                   ncol = 3)
prow

legend_b <- get_legend(p3 + theme(legend.position = "bottom"))
p <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(3,.5))
p

ggsave("1_HBN_classifier.png", p, height = 5, width = 12)
