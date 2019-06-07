library(kableExtra)
library(tidyr)
df <- read.csv("Clean_Motion_Data.csv")
df$group <- ifelse(df$read < 85, "Dyslexic",
                   ifelse(df$read >= 85 & df$dys_dx == 0, "Control",
                   "Other"))


table_df <- df %>%
  subset(group != "Other") %>%
  dplyr::select(c("group","age_at_testing","gender","adhd_dx","wasi_fs2","wasi_mr_ts","wasi_vocab_ts","twre_index","wj_brs","ctopp_pa",
                  "ctopp_rapid","ctopp_pm","wj_wa_ss","wj_lwid_ss","twre_pde_ss","twre_swe_ss")) %>%
  unique(.)

# How many individuals?
table(table_df$group)
# How many have an ADHD diagnosis?
table(table_df$group, table_df$adhd_dx)
# How may boys?
table(table_df$group, table_df$gender)

mu <- aggregate(table_df[,2:length(names(table_df))], list(table_df$group), mean)

sigma <- aggregate(table_df[,2:length(names(table_df))], list(table_df$group), sd)

sig <- lapply(table_df[,2:length(names(table_df))], function(x) t.test(x ~ table_df$group))

all_p <- vector()
for (s in sig){
  this_p <- s$p.value
  all_p <- rbind(all_p, this_p)
}
all_p2 <- data.frame(measure = names(table_df)[2:length(names(table_df))],
                     p = all_p)

# Join them together into one dataframe
mu_long <- gather(mu, measure, mean, age_at_testing:twre_swe_ss, factor_key = TRUE)
mu_long <- spread(mu_long, Group.1, mean)
names(mu_long) <- c("measure","Control Mean","Dyslexic Mean")

sd_long <- gather(sigma, measure, sd, age_at_testing:twre_swe_ss, factor_key = TRUE)
sd_long <- spread(sd_long, Group.1, sd)
names(sd_long) <- c("measure","Control SD","Dyslexic SD")

sum_df <- merge(mu_long, sd_long)
sum_df <- merge(sum_df, all_p2)
names(sum_df) <- c("measure","Control","Dyslexic","Control","Dyslexic","p")

is.num <- sapply(sum_df, is.numeric)
is.num[6] <- FALSE
sum_df[is.num] <- lapply(sum_df[is.num], round, 3)

sum_df$measure <- as.factor(sum_df$measure)
levels(sum_df$measure)  <- c("Age","Gender","ADHD","Full-Scale IQ", "Matrix Reasoning t-score","Vocabulary t-score",
                             "TOWRE Index","Basic Reading Score","Phonological Awareness",
                             "Rapid Automatic Naming","Phonological Memory","Word Attack","Letter Word Identification",
                             "Pseudoword Decoding","Sight Word Efficiency")


sum_df$p <- format.pval(sum_df$p, digits = 3)
align_vec <- rep('c',6)
align_vec[1] <- 'l'

kable(sum_df, col.names = c("","Control","Dyslexic","Control","Dyslexic","$p$"),
      align=align_vec)%>%
  kable_styling(bootstrap_options = "striped")%>%
  add_header_above(c(" " = 1, "Mean" = 2, "SD" = 2, " " = 1))%>%
  group_rows("CTOPP-2",2,4) %>%
  group_rows("TOWRE-2",5,7) %>%
  group_rows("WASI-II",8,10) %>%
  group_rows("Woodcock Johnson",11,13)


