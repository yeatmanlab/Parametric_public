# Clean data for further analysis
rm(list = ls())
# Set working directory
library(dplyr)
library(tidyr)
library(stringr)

## When loading/saving, this script assumes the working directory is set to the
## root directory of the repo. Relative to this script's location that is:
setwd("/home/eobrien/bde/Projects/Parametric_public/Analysis")

## load the raw response data
response_df <- data.frame()
data_dir <- file.path("..","Motion","Exp", "Data","Raw")
raw_files <- list.files(path=data_dir)
## keep only categorization data (not discrimination); remove practice blocks
## and pilot data (subject "nnn")
raw_files <- raw_files[!grepl("Motion", raw_files)]
raw_files <- raw_files[!grepl("nnn", raw_files)]
raw_files <- raw_files[!grepl(".mat", raw_files)]
raw_files <- raw_files[!grepl("HDDM", raw_files)]
raw_files <- raw_files[!grepl("Pilot", raw_files)]
raw_files <- raw_files[!grepl("Psychometric", raw_files)]

## read in remaining raw data files
opts <- options(warn=2)  # convert warnings to errors, while reading in files
for (fname in raw_files) {
  ## skip=1 because first row of each file is a timestamp
  df_tmp <- tryCatch(read.csv(file.path(data_dir, fname)),
                     error=function(e) {print(paste("skipping", fname, 
                                                    conditionMessage(e))); e})
  if(inherits(df_tmp, "error")) next
  ## only keep complete blocks
  if(dim(df_tmp)[1] == 50) {
    
    ## get the timestamp
    date_str <- str_split(fname, "_|-|\\.")[[1]][2]
    df_tmp$psych_date <- as.Date(date_str, "%Y%m%d")
    time_str <- str_split(fname, "_|-|\\.")[[1]][3]
    df_tmp$time <- as.numeric(time_str)
    
    # Get the 
    ## concatenate with other files
    response_df <- rbind(response_df, df_tmp)
  } else {
    print(paste("skipping", fname, "(incomplete block)"))
  }
}
options(opts)  # restore default options

# Change the column names to what is required for HDDM
colnames(response_df) <- c("subj_idx", "trial", "stim", "direction","response", "rt","psych_date","timestamp")
response_df$timestamp <- as.numeric(response_df$timestamp)
# Scale the coherence levels so they are in tens, not decimals, for ease of reading
response_df$stim <- 100*response_df$stim

# Make sure all the subject names use capital letters and are properly named
# These are some naming errors that were made on the raw data.
response_df$subj_idx <- as.numeric(gsub("\\D", "", response_df$subj_idx))

# How many subjects were found?
print(paste(length(unique(response_df$subj_idx)), "subjects found in raw files."))

# How many subjects meet our demographic constraints?
# Load in biographical data
demog_df <- read.csv("Demographic.csv")
names(demog_df)[2] <- "subj_idx"

response_df <- response_df %>%
  subset(subj_idx %in% demog_df$subj_idx)

# How many subjects were found?
print(paste(length(unique(response_df$subj_idx)), "subjects meet study criteria."))

# Exclude the first three blocks of HC896, who did 9 test blocks instead of 6.
# This participant did not comply with instructions on first 3 blocks and we asked them to try again.
response_df <- response_df %>%
  subset(!(subj_idx == "896" & timestamp %in% c("958","954","949")))

# For each subject... turn the timestamps into indicators of blocks
get_block <- function(sub){
  x_unique <- unique(sub$timestamp)
  x_ranks <- rank(x_unique)
  sub$block <- x_ranks[match(sub$timestamp,x_unique)]
  return(sub)
}

response_df <- response_df %>% 
  group_by(subj_idx) %>%
  do(., get_block(.))

response_df$block_id = paste0(response_df$subj_idx,"_", response_df$block)

# Exclude any subjects if they did not complete the whole experiment
is_incomplete <- response_df %>% 
  dplyr::group_by(subj_idx)%>%
  dplyr::summarise(num_blocks = max(block))%>%
  subset(num_blocks < 6)
print(paste(length(unique(is_incomplete$subj_idx)), "subjects didn't complete 6 blocks."))
response_df <- filter(response_df, !(subj_idx %in% is_incomplete$subj_idx ))

# Filter out any NA responses/rts
response_df <- filter(response_df, rt != "NA" & response != "NA")

# Remove any subjects who don't show evidence of being above chance at their best stimulus coherence level
n_trials = 60
mu = n_trials/2
sigma = sqrt(mu*0.5*0.5)
t_value = qt(1 - (0.05/4), n_trials-1)
num_correct = mu + t_value*sigma
prop_threshold = num_correct/n_trials

best_df <- response_df %>%
  dplyr::group_by(subj_idx,stim) %>%
  dplyr::summarise(score = mean(response, na.rm = TRUE)) %>%
  dplyr::group_by(subj_idx)%>%
  dplyr::summarise(best = max(score))%>%
  filter(best >= prop_threshold) 

nsub <- length(unique(response_df$subj_idx)) - length(unique(best_df$subj_idx))
print(paste(nsub, "subjects didn't score above chance"))

response_df <- response_df %>%
  filter(subj_idx %in% best_df$subj_idx)

# Lastly, remove any subject for whom we'd need to remove more than 15% of their data (outside of time range)
enough_pts <- response_df %>%
  mutate(in_range = ifelse(rt > 0.2 & rt < 10,1,0)) %>%
  group_by(subj_idx) %>%
  summarise(prct_in_range = mean(in_range)) %>%
  filter(prct_in_range >= 0.85)

# How many subjects did not generate enough data points?
nsub <- length(unique(response_df$subj_idx)) - nrow(enough_pts)
print(paste(nsub, "subjects did not produce enough usable data."))

response_df <- response_df %>%
  filter(subj_idx %in% enough_pts$subj_idx)



use_df <- merge(demog_df, response_df, by = "subj_idx")
print(paste("There are currently", length(unique(use_df$subj_idx)), "subjects."))


# Clean up some duplicate/unnecessary columns
use_df <- use_df %>%
  dplyr::select(-c("record_id","timestamp","block_id"))

# Define the reading composite score
use_df$read <- 0.5*(use_df$wj_brs + use_df$twre_index)
write.csv(use_df, "Clean_Motion_Data.csv", row.names = FALSE)


df <- use_df %>%
  subset(subj_idx == 1025)
hist(df$rt)
