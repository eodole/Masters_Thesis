
library(igraph)
library(dplyr)
library(ggplot2)
library(mice)
source("/Users/eodole/Desktop/Masters_Thesis/R_code/utils.R")
library(tidyr)
library(latex2exp)
set.seed(1000)

# generate_dataset <- function(matrix){
#   # would be nice to have a function that generates any data set based on a linear model given an adjacency matrix 
# }

# p = 0.5 # prob of mcar missingness

full_data <- gen_dataset1(10000)
# obs_data <- apply(full_data, c(1,2), induce_na, p_missing = p)


# want to impute the obs_data using mice defaults 
# default_imp <- complete(mice(obs_data, m =1, maxit = 10, method = 'norm', seed = 123))

# want to impute the obs_data using the structure as given in gen_dataset1 
# A -> B 
# C -> B 
# C -> D
# pred = matrix(c(0,0,0,0,
#                 1,0,1,0,
#                 0,0,0,0,
#                 0,0,1,0), ncol = 4, byrow = T)
# causal_imp <- complete(mice(obs_data, m=1, maxit = 10, method = 'norm', seed = 123, predictorMatrix = pred))
# 
# # want to then evaluate them in some way 
#   #-first could use compare their mse with the full data 
# 
# # compare_to_gt(full_data, default_imp, obs_data)
# # compare_to_gt(full_data, causal_imp, obs_data)
# 
# 
# 
# ## experiment time: single rep 
# # something interesting might be to vary the p_missing and then graph the mse of the two datasets 
# 
# 
# mse_plain_normpred <- c()
# mse_plain_pmm <- c()
# mse_cause_normpred <- c()
# mse_cause_pmm <- c()
# mse_cause_norm <- c()
# 
# for (p in seq(0.2, 0.9, by=0.1)){
#   obs_data2 <- apply(full_data, c(1,2), induce_na, p_missing = p)
#   
#   plain <- complete(mice(obs_data2, m =1, maxit = 10, method = 'norm.predict', seed = 123))
#   mse_plain_normpred <- c(mse_plain_normpred, mse(full_data, plain))
#   
#   plain_pmm <- complete(mice(obs_data2, m =1, maxit = 10, method = 'pmm', seed = 123))
#   mse_plain_pmm <- c(mse_plain_pmm, mse(full_data, plain_pmm))
#   
#   caus_pmm <- complete(mice(obs_data2, m=1, maxit = 10, method = 'pmm', seed = 123, predictorMatrix = pred))
#   mse_cause_pmm <- c(mse_cause_pmm, mse(full_data, caus_pmm))
#   
#   caus_norm <- complete(mice(obs_data2, m=1, maxit = 10, method = 'norm', seed = 123, predictorMatrix = pred))
#   mse_cause_norm <- c(mse_cause_norm, mse(full_data, caus_norm))
#   
#   caus_normpred <- complete(mice(obs_data2, m=1, maxit = 10, method = 'norm.predict', seed = 123, predictorMatrix = pred))
#   mse_cause_normpred <- c(mse_cause_normpred, mse(full_data, caus_normpred))
#   
# }
# 
# 
# mse_df <- data.frame(p = seq(0.2, 0.9, by =0.1), 
#                      plain_pmm = mse_plain_pmm, 
#                      plain_norm_pred = mse_plain_normpred, 
#                      cause_pmm = mse_cause_pmm, 
#                      cause_norm = mse_cause_norm, 
#                      cause_norm_pred = mse_cause_normpred)
# 
# ggplot(mse_df, aes(x = p)) + 
#   geom_line(mapping = aes(y = plain_pmm), color = "#4BA0D7") + 
#   geom_line(mapping = aes(y = plain_norm_pred), color = "#288CCD") + 
#   geom_line(mapping = aes(y = cause_pmm), color = "#FF7B4E") + 
#   geom_line(mapping = aes(y = cause_norm_pred), color = "#FF4100") + 
#   geom_line(mapping = aes(y = cause_norm), color = "#FF5D25") + 
#   labs(title = "Comparing MICE Methods", y = "MSE" ) 
# 
# 
# 
# ## I want to extend this and also compare these to the other graphs but with an incorrect prediction Matrix 
# p_false = matrix(c(0,0,0,1,
#                    1,0,1,0,
#                    1,0,0,0,
#                    1,0,1,0), ncol = 4, byrow = T) # arbitrary 
# 
# plain_norm <- mice_accuracy(full_data, list(m =1,maxit = 10, method = 'norm', printFlag = F))
# false_cause_norm <- mice_accuracy(full_data, list(m =1,maxit = 10, method = 'norm', predictorMatrix = p_false, printFlag = F ))
# false_cause_norm_pred <- mice_accuracy(full_data, list(m =1,maxit = 10, method = 'norm.predict', predictorMatrix = p_false,printFlag = F ))
# 
# mse_df$false_cause_pmm <- false_cause_pmm
# mse_df$false_cause_norm <- false_cause_norm
# mse_df$false_cause_norm_pred <- false_cause_norm_pred
# mse_df$plain_norm <- plain_norm
# 
# 
# mse_df_long <-  mse_df %>% gather(key = "Combined", value = "Accuracy", -p) 
# 
# #asked chatgpt
# mse_df_long$graph_type <- sub("_.*", "", mse_df_long[["Combined"]])
# mse_df_long$method_type <- sub("^(false_)?[^_]+_", "", mse_df_long[["Combined"]])
#   
# 
# mse_df_long %>% ggplot(aes(x = p, y = Accuracy)) + 
#   geom_line(aes(color = graph_type, linetype = method_type)) + 
#   labs(y = "MSE", x = "Missing Probability p")
# 
# ## End of single Rep testing 




## Multi Replicate Testing 

pred = matrix(c(0,0,0,0,
                1,0,1,0,
                0,0,0,0,
                0,0,1,0), ncol = 4, byrow = T)

p_false = matrix(c(0,0,0,1,
                   1,0,1,0,
                   1,0,0,0,
                   1,0,1,0), ncol = 4, byrow = T)



for(rep in c(1:5)){
  # 1. default pmm 
    plain_pmm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'pmm', printFlag = F))
  # 2. default norm 
    plain_norm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'norm', printFlag = F))
  # 3. default norm.pred
    plain_norm_pred <- mice_accuracy(full_data = full_data, list(m =1 ,maxit = 10, method = 'norm.predict', printFlag = F))
  # 4. correct causl pmm 
    cause_pmm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'pmm', printFlag = F, predictorMatrix = pred))
  # 5. correct causl normr 
    cause_norm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'norm', printFlag = F, predictorMatrix = pred))
  # 6. correct causl norm_pred 
    cause_norm_pred <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'norm.predict', printFlag = F, predictorMatrix = pred))
  # 7. false causl pmm 
    false_cause_pmm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'pmm', printFlag = F, predictorMatrix = p_false))
  # 8. false causal norm 
    false_cause_norm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'norm', printFlag = F, predictorMatrix = p_false))
  # 9.  false causal norm pred 
    false_cause_norm_pred <- mice_accuracy(full_data = full_data, list(m =1,maxit = 10, method = 'norm.predict', printFlag = F, predictorMatrix = p_false))
    
  # create the temporary data frame 
    temp_df <- data.frame(p = seq(0.2, 0.9, by =0.1), 
                          plain_pmm = plain_pmm,
                          plain_norm = plain_norm,
                          plain_norm_pred = plain_norm_pred,
                          cause_pmm = cause_pmm,
                          cause_norm = cause_norm,
                          cause_norm_pred = cause_norm_pred,  
                          false_cause_pmm = false_cause_pmm,
                          false_cause_norm = false_cause_norm, 
                          false_cause_norm_pred = false_cause_norm_pred)
# transform it 
    temp_long <-  temp_df %>% gather(key = "Combined", value = "Accuracy", -p) 
    #include the rep number 
    temp_long$replicate <- rep.int(rep, nrow(temp_long))
    
  
  # add it to the bunch 
    if(rep ==1){
      result <- temp_long
    }else{
      result <- bind_rows(result, temp_long)
    }
}

result$graph_type <- sub("_.*", "", result[["Combined"]])
result$method_type <- sub("^(false_)?[^_]+_", "", result[["Combined"]])

result %>% ggplot(aes(x = p, y = Accuracy)) +
  geom_smooth(aes(color = graph_type, linetype = method_type)) +
  labs(y = "Mean Squared Error", x = TeX("Missingness Probability $ \\alpha $"), color = "Predictor Matrix Type", linetype = "Prediction Model") + 
  scale_color_manual(values = c("#f8766d", "#00ba39", "#619cff"), labels = c("correct causal", "arbitrary causal", "default")) + 
  scale_linetype(labels =c("bayesian linear regression", "linear regression", "predictive mean matching"))



