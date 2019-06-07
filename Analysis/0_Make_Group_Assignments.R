# # This script creates two groups that are age and IQ matched: one group of control readers, and one group of readers who meet our criteria 
# # for dyslexia
library(dplyr)
library(MatchIt)

rm(list = ls())
# Load in a dataframe of all the participants that are currently in the study
df_M <- read.csv("DDM_Fit_Results.csv")

# Read in the cleaned speech data
df_S <- read.csv("Clean_Speech_Data.csv")
df <- merge(df_S, df_M)

# Separate into two groups
dys <-  df %>%
  subset(read < 85)%>%
  mutate(group = 1)

ctrl <- df %>%
  subset(read >= 85) %>%
  subset(dys_dx != 1)%>% 
  mutate(group = 0)

grps <- rbind(dys,ctrl) %>%
  dplyr::select(c("group","wasi_mr_ts","age_at_testing","subj_idx"))

zz <- matchit(group ~ wasi_mr_ts + age_at_testing, data = grps,
              method = "nearest",
              discard = "both")


grps$class <- zz$weights
grps <- grps %>%
  subset(class == 1) 

t.test(wasi_mr_ts ~ group, grps)
t.test(age_at_testing ~ group, grps)

##### Not quite balanced for nonverbal IQ - remove top wasi scores, then retry
grps <- grps %>%
  subset(wasi_mr_ts < 68) 

t.test(wasi_mr_ts ~ group, grps)
t.test(age_at_testing ~ group, grps)

# Great, now save this.
all_group <- grps %>%
  dplyr::select(c("subj_idx","group"))%>%
  mutate(group = ifelse(group == 1, "Dyslexic","Control"))
write.csv(all_group, file = "Group_Assignments.csv", quote = FALSE, row.names = FALSE)




# # library(dplyr)
# 
# rm(list = ls())
# # Load in a dataframe of all the participants that are currently in the study
# df <- read.csv("PCA_components.csv")
# 
# dys <-  df %>%
#   subset(read < 85)%>%
#   #subset(wasi_mr_ts > 30 & wasi_mr_ts < 70)%>%
#   mutate(group = "Dyslexic")
# 
# 
# ctrl <- df %>%
#   subset(read >= 85) %>%
#   subset(dys_dx != 1)%>%
#   mutate(group = "Control")
# 
# 
# 
# 
# casecont <- rbind(dys,ctrl)
# 
# m <- matchControls(group ~ wasi_mr_ts,
#                    data = casecont,
#                    contlabel = "Control")
# 
# 
# is_balanced = t.test(dys$wasi_mr_ts, ctrl$wasi_mr_ts)
# is_balanced
# 
# is_balanced = t.test(dys$age_at_testing, ctrl$age_at_testing)
# is_balanced
# 
# balanced = 0
# while(balanced == 0){
# 
#   # Order by nonverbal IQ
#   ctrl <- ctrl[order(-ctrl$wasi_mr_ts),]
#   #dys <- dys[order(dys$wasi_mr_ts),]
# 
#   # Remove one from each group
#   ctrl <- ctrl[2:nrow(ctrl),]
#   #dys <- dys[2:nrow(dys),]
#   is_balanced = t.test(dys$wasi_mr_ts, ctrl$wasi_mr_ts)
#   if (is_balanced$p.value > 0.15){
#     balanced = 1
#   }
# 
# 
# }
# 
# # For sanity check that the ages are also not significantly different
# t.test(dys$age_at_testing, ctrl$age_at_testing)
# 
# ctrl$group <- "Control"
# dys$group <- "Dyslexic"
# all_group <- rbind(ctrl, dys)
# all_group <- all_group %>%
#   dplyr::select(c("subj_idx","group"))
# write.csv(all_group, file = "Group_Assignments.csv", quote = FALSE, row.names = FALSE)
