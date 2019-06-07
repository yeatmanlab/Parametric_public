##############################################
### 3. PLOT DRIFT RATE AS A FUNCTION OF LEVEL  #
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("../Analysis/PCA_components.csv")
df_v <- df %>%
  dplyr::select(c("subj_idx","read","dys_dx","v.6.","v.12.","v.24.","v.48."))%>%
  gather(key = level, value = v, v.6.:v.48.)
df_v$level <- factor(df_v$level)
df_v$level <- ordered(df_v$level, levels = c("v.6.","v.12.","v.24.","v.48."))
levels(df_v$level) <- c("6%","12%","24%","48%")


mycolors <- colorRampPalette(c("white","#3e000c"))
mypalette <- mycolors(7)[3:7]


# Get the actual correlations
r1 <- sqrt(summary(lm(v ~ read, subset(df_v, level == "6%")))$r.squared)
r2 <- sqrt(summary(lm(v ~ read, subset(df_v, level == "12%")))$r.squared)
r3 <- sqrt(summary(lm(v ~ read, subset(df_v, level == "24%")))$r.squared)
r4 <- sqrt(summary(lm(v ~ read, subset(df_v, level == "48%")))$r.squared)

p1 <- summary(lm(v ~ read, subset(df_v, level == "6%")))$coefficients[2,4]
p2 <- summary(lm(v ~ read, subset(df_v, level == "12%")))$coefficients[2,4]
p3 <- summary(lm(v ~ read, subset(df_v, level == "24%")))$coefficients[2,4]
p4 <- summary(lm(v ~ read, subset(df_v, level == "48%")))$coefficients[2,4]



l1 <- paste0("r = ", round(r1,2), ", \U1D631 = ", round(p1,3))
l2 <- paste0("r = ", round(r2,2), ", \U1D631 = ", round(p2,3))
l3 <- paste0("r = ", round(r3,2), ", \U1D631 = ", round(p3,3))
l4 <- paste0("r = ", round(r4,2), ", \U1D631 = ", round(p4,3))

ann_text <- data.frame(read = 85, v = 6,
                       label = c(l1,l2,l3,l4),
                       level = c("6%","12%","24%","48%"))

px1 <- ggplot(df_v, aes(read, v)) +
  geom_point(size = 2)+
  facet_wrap(~level, nrow = 1)+
  geom_smooth(aes(group = level), method = lm, colour = "black") + 
  xlab("Reading Score") + 
  ylab("Drift Rate (v)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 18, face = "bold"))+
  geom_text(data = ann_text,
            mapping = aes(x = read, y = v, label = label),
            colour = "black", size = 5)+
  scale_x_continuous(breaks = c(60,80,100,120))

px1

ggsave("2_drift_rate.png",px1, height = 5, width = 10)

px2 <- ggplot(df, aes(read, sv))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", colour = "black")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 16))
px2

######### Let's also make a little bar graph for the inset
df_grp <- df_v %>%
  subset(level == "12%")
df_grp$group <- ifelse(df_grp$read < 85, "Dyslexic",
                       ifelse(df_grp$read >= 85 & df_grp$dys_dx == 0, "Control",
                              "Other"))
df_grp <- subset(df_grp, group != "Other")

sum_df <- df_grp %>%
  group_by(group) %>%
  summarise(mu_v = mean(v),
            se_v = sd(v)/sqrt(n()))

# Some image parameters
pd <- position_dodge(.9) # move them .05 to the left and right
cols <- c("firebrick3","royalblue3")# Text properties
corr_size = 5
label_size = 24
tick_size = 18
y_rat = 1.0

px3 <- ggplot(sum_df, aes(group, mu_v))+
  geom_bar(stat= "identity", position = "dodge", aes(colour = group, fill = group),width = 0.6, alpha = 0.5)+
  #scale_y_continuous(limits = c(-0.65,0.65), oob = rescale_none)+
  scale_color_manual("Group",values = cols)+
  scale_fill_manual("Group", values = cols)+
  geom_errorbar(aes(ymin=mu_v-se_v, ymax=mu_v+se_v), width=.1, position = pd,size=0.75)+
  xlab("")+
  ylab(expression(italic(v)))+
  theme(axis.title = element_text(size = 36),
        axis.text.y = element_text(size = tick_size),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18))+
  coord_fixed(ratio = 1)
px3

ggsave("v_group_difference.png",px3)
