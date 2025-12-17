# Libraries 
library(pcalg)
library(dplyr)
library(gRbase)
library(ggplot2)
source("/Users/eodole/Desktop/Masters_Thesis/R_code/utils.R")
library(latex2exp)
library(reticulate)
library(tidyr)




#import dataset 
library(readr)
full_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/random_er_dag/experiment4_small_data.csv",  col_names = FALSE)

#test smaller graph
full_data <- read_csv("Desktop/Masters_Thesis/Py_Code/exp5/exp5_zscore_true_data.csv",  col_names = FALSE)
pred <- as.matrix(read_csv("Desktop/Masters_Thesis/Py_Code/exp5/exp5_zscore_true_adj.csv",  col_names = FALSE))

View(experiment4_data)

#creating scms 
# pred <- as.matrix(read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/random_er_dag/experiment4_true_adj.csv",  col_names = FALSE))
pred <- as.matrix(read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/random_er_dag/experiment4_small_true_adj.csv",  col_names = FALSE))
rownames(pred) <- colnames(full_data)
pred <- t(pred) # need to flip the matrix for it to work properly

p_false <- matrix(sample(c(1,0), nrow(pred)^2, replace = T, prob = c(0.3,.7)), nrow=nrow(pred))
rownames(p_false) <- colnames(full_data)
colnames(p_false) <- colnames(full_data)
diag(p_false)<-0






experiment4 <-function(full_data, pred, p_false,n_reps){
"  full_data: dataset to analyze 
  pred: true adj matrix
  p_false: random adj matrix "

  
  for(rep in c(1:n_reps)){
    # 1. default pmm 
    plain_pmm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'pmm', printFlag = F))
    print("finished plain pmm")
    # 2. default norm 
    plain_norm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'norm', printFlag = F))
    print("finished plain norm")
    # 3. default norm.pred
    plain_norm_pred <- mice_accuracy(full_data = full_data, list(m =1 ,maxit = 5, method = 'norm.predict', printFlag = F))
    print("finished plain norm pred")
    # 4. correct causl pmm 
    cause_pmm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'pmm', printFlag = F, predictorMatrix = pred))
    print("finished cause pmm")
    # 5. correct causl normr 
    cause_norm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'norm', printFlag = F, predictorMatrix = pred))
    print("finished caus norm")
    # 6. correct causl norm_pred 
    cause_norm_pred <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'norm.predict', printFlag = F, predictorMatrix = pred))
    print("finished cause norm pred")
    # 7. false causl pmm 
    false_cause_pmm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'pmm', printFlag = F, predictorMatrix = p_false))
    print("finished false cause pmm")
    # 8. false causal norm 
    false_cause_norm <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'norm', printFlag = F, predictorMatrix = p_false))
    print("finished false cause norm")
    # 9.  false causal norm pred 
    false_cause_norm_pred <- mice_accuracy(full_data = full_data, list(m =1,maxit = 5, method = 'norm.predict', printFlag = F, predictorMatrix = p_false))
    print("all done with mice")
    # create the temporary data frame 
    temp_df <- data.frame(p = seq(0.2, 0.9, by =0.1), 
                          # replicate = rep(rep, 8)
                          plain_pmm = plain_pmm,
                          plain_norm = plain_norm,
                          plain_norm_pred = plain_norm_pred,
                          cause_pmm = cause_pmm,
                          cause_norm = cause_norm,
                          cause_norm_pred = cause_norm_pred,  
                          false_cause_pmm = false_cause_pmm,
                          false_cause_norm = false_cause_norm, 
                          false_cause_norm_pred = false_cause_norm_pred)
    print("made temp")
    # transform it 
    temp_long <-  temp_df %>% gather(key = "Combined", value = "Accuracy", -p) 
    print("made temp long")
    #include the rep number 
    temp_long$replicate <- rep.int(rep, nrow(temp_long))
    
    
    #add it to the bunch
    if(rep ==1){
      result <- temp_long
      print(result)
    }else{
      result <- bind_rows(result, temp_long)
    }
  }
  # result <- temp_long
  return (result)
}


result <- experiment4(full_data, pred, p_false, 5)



## Results and graph for MSE 
result$graph_type <- sub("_.*", "", result[["Combined"]])
result$method_type <- sub("^(false_)?[^_]+_", "", result[["Combined"]])

result %>% ggplot(aes(x = p, y = Accuracy)) +
  geom_smooth(aes(color = graph_type, linetype = method_type)) +
  labs(y = "Mean Squared Error", x = TeX("Missingness Probability $ \\alpha $"), color = "Predictor Matrix Type", linetype = "Prediction Model") +
  scale_color_manual(values = c("#f8766d", "#00ba39", "#619cff"), labels = c("correct causal", "arbitrary causal", "default")) +
  scale_linetype(labels =c("bayesian linear regression", "linear regression", "predictive mean matching"))




## Here we want to do an extension on with this dataset and look at the z-scores
#defne the impute method, I will have linear model and the diff pred matrices, 

impute <- function(dataset){
  print("default")
  return(complete(mice(dataset, 
                       m =1 ,maxit = 5, method = 'norm.predict', printFlag = F)))
                       
}


res6 = run_experiment2_extension(full_data) #pause game, this is basiclaly not written for a bigger experiment, it is only written for the four variables sadly
# to get around this, i am not going to test alll sep sets up to 3, but i somehow need to rewrite compute zscore for this
# maybe i could rewrite to break the loop? idk idk 

res6  %>% ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test),se = F) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Default Prediction Matrix Z-scores ", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))



impute <- function(dataset){
  print("correct")
  return(complete(mice(dataset, 
                       m =1 ,maxit = 5, method = 'norm.predict', printFlag = F, predictorMatrix = pred)))
  # m =1, maxit = 10, 
  # method = 'pmm', 
  # seed = 123, 
  # # predictorMatrix = true_pred, 
  # printFlag = F)))
}

res2 = run_experiment2_extension(full_data) 
res2  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test),se = F) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

impute <- function(dataset){
  print("incorrect")
  return(complete(mice(dataset, 
                       m =1 ,maxit = 5, method = 'norm.predict', printFlag = F, predictorMatrix = p_false)))
  # m =1, maxit = 10, 
  # method = 'pmm', 
  # seed = 123, 
  # # predictorMatrix = true_pred, 
  # printFlag = F)))
}

res3 = run_experiment2_extension(full_data) 
res3  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test ),se = F) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Incorrect Prediction Matrix", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))



