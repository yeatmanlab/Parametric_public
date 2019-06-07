df <- read.csv("Clean_Motion_Data.csv")
df <- unique(df)


table_df <- df %>%
  dplyr::select(c("read","age_at_testing","adhd_dx","gender","wasi_fs2","wasi_mr_ts","wasi_vocab_ts","ctopp_pa","ctopp_rapid","ctopp_pm","twre_swe_ss","twre_pde_ss","wj_lwid_ss","wj_wa_ss")) %>%
  unique(.)


# For each variable that isn't reading, get the correlation with reading score
vars <- names(table_df)[-1]

# Initialize some vectors
all_se <- vector()
all_b <- vector()
all_p <- vector()

for (v in vars){
  form <- paste0("read ~ ", v)
  out_lm <- summary(lm(form, table_df))$coefficients
  
  
  all_p <- rbind(all_p, out_lm[2,4])
  all_b <- rbind(all_b, out_lm[2,1])
  all_se <- rbind(all_se, out_lm[2,2])
}

lm_df <- data.frame(measure = vars,
                    beta = all_b,
                    se = all_se,
                    p = all_p)

# Join them together into one dataframe
is.num <- sapply(lm_df, is.numeric)
is.num[4] <- FALSE
lm_df[is.num] <- lapply(lm_df[is.num], round, 2)

lm_df$measure <- as.factor(lm_df$measure)
levels(lm_df$measure)  <- c("ADHD diagnosis","Age","Phonological Awareness",
                            "Phonological Memory","Rapid Automatic Naming","Gender","Pseudoword Decodign","Sight Word Reading","Full-scale IQ","Matrix Reasoning t-score","Vocabulary t-score","Letter Word Identification","Word Attack")

target = c("Age","ADHD diagnosis","Gender","Phonological Awareness",
           "Phonological Memory","Rapid Automatic Naming","Pseudoword Decodign","Sight Word Reading","Full-scale IQ","Matrix Reasoning t-score","Vocabulary t-score","Letter Word Identification","Word Attack")

lm_df <- lm_df[match(target, lm_df$measure),]

lm_df$p <- format.pval(lm_df$p, digits = 3)
align_vec <- rep('c',3)
align_vec[1] <- 'l'
row.names(lm_df) <- NULL

kable(lm_df,col.names = c("","$\\beta$","SE","$p$"),
      align=align_vec, escape = FALSE)%>%
  kable_styling(bootstrap_options = "striped")%>%
  group_rows("CTOPP-2",4,6)%>%
  group_rows("TOWRE-2",7,8)  %>%
  group_rows("WASI-II",9,11) %>%
  group_rows("Woodcock Johnson",12,13)
