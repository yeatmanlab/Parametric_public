#######################################
# How reliable are the DDM parameters?#
#######################################
library(dplyr)

rm(list = ls())

params <- read.csv("/home/eobrien/bde/Projects/Parametric_public/Motion/Exp/Data/Processed/DDM_Fit_reliability.csv")

params <- subset(params, !(subj_idx %in% c(1025,749)))
# For each parameter, correlate its values 
a1 <- subset(params, run == "1")
a0 <- subset(params, run == "0")

# A retest reliability
predictors = names(params)[2:10]

rel <- vector()
rel_adj <- vector()

for (p in predictors){
  
  tmp <- eval(parse(text = paste0("cor(a1$", p, ", a0$", p, ")")))
  print(paste0("For parameter ", p, ", reliability is ", tmp, "."))
  
  tmp_pr = 2*tmp / (1 + tmp)
  
  print(paste0("Adjusted: ", tmp_pr))
  print(paste("\n"))
  
  rel <- rbind(rel, tmp)
  rel_adj <- rbind(rel_adj, tmp_pr)
  
}

rel_df <- data.frame(parameter = predictors,
                     reliability = rel,
                     adjusted = rel_adj)
row.names(rel_df) <- NULL
library(gridExtra)
library(kableExtra)

kable(rel_df) %>%
  kable_styling(bootstrap_options = "striped")


