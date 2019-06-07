#################################################
#### DO PCA ON FEATURES                   #######
####################################################
rm(list = ls())
setwd("/home/eobrien/bde/Projects/Parametric_public/Analysis")

library(tidyr)
library(dplyr)
library(lme4)
library(leaps)
library(glmnet)
library(ggplot2)

# Read in the cleaned vision data
df <- read.csv("DDM_Fit_Results.csv")

########## Get a composite measure of slope #####################
df_M_sensory <- df %>%
  dplyr::select(c("subj_idx","v.6.","v.12.","v.24.","v.48.","sv")) %>%
  scale(.)

PCA <- prcomp(df_M_sensory[,2:6], scale = TRUE)
summary(PCA)
df$PC_M_sensory <- scale(PCA$x[,1]) #scale(rowMeans(df_M_sensory[,2:6]))

# Do it without sv
#PCA <- prcomp(df_M_sensory[,2:5], scale = TRUE)
#summary(PCA)
#df$PC_M_sensory_nosv <- scale(PCA$x[,1])
#df$PC_M_sensory_nosv <- scale(rowMeans(df_M_sensory[,2:5]))

#df$PC_M_sensory <- rowMeans(df_M_sensory)

ggplot(df, aes(read, PC_M_sensory))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(read ~ PC_M_sensory, df))


####### LOOK AT THE THREE CORRELATED DECISION MAKING THINGS
df_M_decision <- df %>%
  dplyr::select(c("subj_idx","t","st","sz"))%>%
  scale(.)

PCA <- prcomp(df_M_decision[,2:4], scale = TRUE)
summary(PCA)
#df$PC_M_decision <- rowMeans(df_M_decision)#scale(PCA$x[,1])
df$PC_M_decision <- scale(PCA$x[,1])


ggplot(df, aes(read, PC_M_decision))+
  geom_point()+
  geom_smooth(method="lm")
summary(lm(read ~ PC_M_decision, df))


############ Are these components related to one another? #####
summary(lm(PC_M_decision ~ PC_M_sensory, df))
summary(lm(PC_M_decision ~ a, df))
summary(lm(PC_M_sensory ~ a, df))

write.csv(df, "PCA_components.csv")

# Scale some things
#df$read <- scale(df$read)
df$wasi_mr_ts <- scale(df$wasi_mr_ts)
df$ctopp_pa <- scale(df$ctopp_pa)
df$ctopp_rapid <- scale(df$ctopp_rapid)

lmfit1 <- lm(read ~ PC_M_sensory + a + PC_M_decision + wasi_mr_ts + age_at_testing + adhd_dx, df)
summary(lmfit1)

# Remove adhd
lmfit2 <- lm(read ~ PC_M_sensory + a + PC_M_decision + wasi_mr_ts + age_at_testing, df)
anova(lmfit1, lmfit2)
summary(lmfit2)

# Remove age
lmfit3 <- lm(read ~ PC_M_sensory + a + PC_M_decision +  wasi_mr_ts, df)
anova(lmfit2, lmfit3)
summary(lmfit3)




#####################################################################################
#### How does the quality of the model change with the number of predictors?
k = nrow(df)
df$fold <- cut(seq(1,nrow(df)),breaks=k,labels=FALSE)



pred1 = vector()
pred2 = vector()
pred3 = vector()

for (i in 1:k){
  lm1 <- lm(read ~ PC_M_sensory + wasi_mr_ts, data = subset(df, fold != i))
  lm2 <- lm(read ~ PC_M_sensory + PC_M_decision + wasi_mr_ts, data = subset(df, fold != i))
  lm3 <- lm(read ~ PC_M_sensory + PC_M_decision + a + wasi_mr_ts, data = subset(df, fold != i))
  
  
  yhat1 <- predict(lm1, subset(df, fold == i))
  yhat2 <- predict(lm2, subset(df, fold == i))
  yhat3 <- predict(lm3, subset(df, fold == i))
  
  pred1[i] = unname(yhat1)
  pred2[i] = unname(yhat2)
  pred3[i] = unname(yhat3)
  
}
df$yhat1 <- pred1
df$yhat2 <- pred2
df$yhat3 <- pred3



#### What is the r2?
getr2 <- function(y,yhat){
  tss <- sum( (y- mean(y))^2 )
  rss <- sum( (yhat - y)^2) 
  r2 <- 1 - (rss/tss)
  printr <- round(r2,2)
  return(printr)
}



l <- list(r2 = getr2(df$read, df$yhat1))
eq <- substitute("LOO-CV "*italic(R)^2 == r2,l)
eqstr <- as.character(as.expression(eq))
px1 <- ggplot(df, aes(read,yhat1))+
  geom_point()+
  geom_abline()+
  xlab("True reading skill")+
  ylab("Predicted reading skill")+
  coord_fixed(ratio = 1)+
  xlim(c(50,135))+
  ylim(c(50,135))+
  annotate(geom="text", x=70,y=130, label = eqstr, parse = TRUE, size = 5)+
  theme_bw()+
  theme(text = element_text(size = 14),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
px1  


l <- list(r2 = getr2(df$read, df$yhat2))
eq <- substitute("LOO-CV "*italic(R)^2 == r2,l)
eqstr <- as.character(as.expression(eq))
px2 <- ggplot(df, aes(read,yhat2))+
  geom_point()+
  geom_abline()+
  xlab("True reading skill")+
  ylab("Predicted reading skill")+
  coord_fixed(ratio = 1)+
  xlim(c(50,135))+
  ylim(c(50,135))+
  annotate(geom="text", x=70,y=130, label = eqstr, parse = TRUE, size = 5)+
  theme_bw()+
  theme(text = element_text(size = 14),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
px2

l <- list(r2 = getr2(df$read, df$yhat3))
eq <- substitute("LOO-CV "*italic(R)^2 == r2,l)
eqstr <- as.character(as.expression(eq))
px3 <- ggplot(df, aes(read,yhat3))+
  geom_point()+
  geom_abline()+
  xlab("True reading skill")+
  ylab("Predicted reading skill")+
  coord_fixed(ratio = 1)+
  xlim(c(50,135))+
  ylim(c(50,135))+
  annotate(geom="text", x=70,y=130, label = eqstr, parse = TRUE, size = 5)+
  theme_bw()+
  theme(text = element_text(size = 14),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
  
px3


ggsave("DDM_model_1factor.png",px1, dpi = 300)
ggsave("DDM_model_2factor.png",px2, dpi = 300)
ggsave("DDM_model_3factor.png",px3, dpi = 300)


########### Repeat model selection with phonological processing
lmfit1 <- lm(read ~ PC_M_sensory + a + PC_M_decision + wasi_mr_ts + ctopp_pa + ctopp_rapid + age_at_testing + adhd_dx, df)
summary(lmfit1)

# Remove adhd
lmfit2 <- lm(read ~ PC_M_sensory + a + PC_M_decision + wasi_mr_ts + ctopp_pa + ctopp_rapid + age_at_testing, df)
anova(lmfit1,lmfit2) # OK


# Remove age
lmfit3 <- lm(read ~ PC_M_sensory + a + PC_M_decision + wasi_mr_ts + ctopp_pa + ctopp_rapid, df)
anova(lmfit2,lmfit3) # OK
summary(lmfit3) 


############# Compare a model with no psychophysical predictors to the selected model ############
lmfit_noddm <- lm(read ~ wasi_mr_ts + ctopp_pa + ctopp_rapid, df)

anova(lmfit_noddm, lmfit3)
################ Are any of the DDM parameters correlated with phonological awareness?
summary(lm(PC_M_sensory ~ ctopp_pa, df))
summary(lm(PC_M_decision ~ ctopp_pa, df))
summary(lm(a ~ ctopp_pa, df))




##################### DO ON HELD OUT DATA #####################################
set.seed(2)
##### What if we did this on held-out data? IE Lasso model. ###########
df <- slice(df, sample(1:n()))
# Assign each point to test or training set
folds = 1
df$fold <- 1 #cut(seq(1,nrow(df)),breaks=folds,labels=FALSE)


r2 <- vector()
dropped <- vector()
i = 2
#for(i in 1:folds){
  
  train <- subset(df, fold != i)
  test <- subset(df, fold == i )
  
  train_lasso <- train %>%
    dplyr::select("read","a","PC_M_sensory","PC_M_decision","wasi_mr_ts","ctopp_rapid","ctopp_pa",
                  "adhd_dx","age_at_testing")%>%
    #dplyr::select("read","ctopp_rapid","ctopp_pa")%>%
    na.omit(.)
  
  
  # Training data
  train_mat <- scale(as.matrix(train_lasso[,-c(1)]))
  train_vec <- as.matrix(train_lasso$read)
  
  # Fit the lasso model
  cv.lasso <-cv.glmnet(train_mat, train_vec)
  out <- coef(cv.lasso, s = cv.lasso$lambda.min)
  dropped[i] <- sum(out==0)
  print(out)
  
cvdf <- data.frame(lambda = cv.lasso$lambda,
                   mse = cv.lasso$cvm,
                   sd = cv.lasso$cvsd/sqrt(10))
px1 <- ggplot(cvdf, aes(lambda, mse))+
  geom_line() + 
  xlab(expression(lambda))+
  ylab("Mean Squared Error")+
  #geom_vline(xintercept = cv.lasso$lambda.min, linetype = 2, color = "red")+
  theme(axis.title = element_text(size = 24))
  
px1
ggsave("lasso_MSE.png", px1)
npred <- vector()
for (s in cv.lasso$lambda){
  out <- coef(cv.lasso, s)
  npred <- rbind(npred, nnzero(out))
}

cvdf$npred <- npred

px2 <- ggplot(cvdf, aes(lambda, npred))+
  geom_line() + 
  xlab(expression(lambda))+
  ylab("# of predictors")+
  #geom_vline(xintercept = cv.lasso$lambda.min, linetype = 2, color = "red")+
  theme(axis.title = element_text(size = 24))+
  scale_y_continuous(breaks = c(1,3,5,7,9))
px2
ggsave("lasso_npredictors.png",px2)
