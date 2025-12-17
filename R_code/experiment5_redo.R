library(dplyr)
library(mice)
library(readr)


"""
Here I want to try and figure out why the scores are the same for two independence tests that should be different: 
2, 3 | 
and 
2,3 | 4 
"""

# first using the correct impute function
impute <- function(dataset){
  print("exp_correct")
  return(complete(mice(dataset, 
                       m =1 ,maxit = 5, method = 'norm.predict', printFlag = F, predictorMatrix = true_adj)))
  # m =1, maxit = 10, 
  # method = 'pmm', 
  # seed = 123, 
  # # predictorMatrix = true_pred, 
  # printFlag = F)))
}



exp5_1_alpha_zero <- impute(data)

exp5_1_alpha_zero_z_scores<- compute_zscores(cor(exp5_1_alpha_zero), n = nrow(exp5_1_alpha_zero))


exp5_1_z_scores_results <- data.frame(
  x = numeric(), 
  y = numeric(), 
  S = numeric(), 
  z_score = numeric()
)

for (i in 1:length(exp5_1_alpha_zero_z_scores)){
  temp <- data.frame(
    x = exp5_1_alpha_zero_z_scores[[i]]$x,
    y =  exp5_1_alpha_zero_z_scores[[i]]$y,
    S = paste(exp5_1_alpha_zero_z_scores[[i]]$S, collapse =''), ### currently having problems with the seperating set idk how to make it behave nicely
    indep_test = paste0(exp5_1_alpha_zero_z_scores[[i]]$x, ",", exp5_1_alpha_zero_z_scores[[i]]$y, "|", paste(exp5_1_alpha_zero_z_scores[[i]]$S, collapse ='')),
    z_score =  exp5_1_alpha_zero_z_scores[[i]]$z_stat
  )
  exp5_1_z_scores_results <- rbind(exp5_1_z_scores_results, temp)
}


exp5_1_z_scores_results %>% 
  ggplot(aes( y = z_score))
  geom_point(aes(color = indep_test)) +
  geom_line(aes(color = matrix))+
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))



  
  ## Now I want to test at the most basic level, does the test actually work as we would expect? 
  
C <- cor(exp5_1_alpha_zero)
n <- nrow(exp5_1_alpha_zero)
x =2
y=3


pcalg::zStat(2,4, S = NULL, C= C, n = n)
pcalg::zStat(2,4, S = c(3), C= C, n = n)

pcalg::condIndFisherZ(6,5, S = NULL, C= C, n = n, cutoff = qnorm(1-0.05/2))
pcalg::condIndFisherZ(6,7, S = c(3,4), C= C, n = n,cutoff = qnorm(1-0.05/2))

exp5_cca <- data[complete.cases(data),]

C_cca <- cor(exp5_cca)
n_cca <- nrow(exp5_cca)
pcalg::zStat(2,4, S = NULL, C= C_cca, n = n_cca)
pcalg::zStat(2,4, S = c(3), C= C_cca, n = n_cca)


pcalg::zStat(2,4, S=NULL, C = cor(true_data), n = nrow(true_data))
pcalg::zStat(2,4, S=3, C = cor(true_data), n = nrow(true_data))

pcalg::zStat(6,4, S=NULL, C = cor(true_data), n = nrow(true_data))
pcalg::zStat(6,4, S=3, C = cor(true_data), n = nrow(true_data))




weights<- read.csv("/Users/eodole/Desktop/Masters_Thesis/Py_Code/exp5/exp5_zscore_true_weights.csv", header = F)


generate_exp5 <- function(n =10000){
  n1 <- rnorm(n)
  n4<- rnorm(n)
  n5 <- weights[4,5]*n4 + rnorm(n)
  n2 <- weights[1,2]*n1 + rnorm(n)
  n3 <- weights[2,3]*n2 + weights[4,3]*n4 + rnorm(n)
  n6 <- weights[3,5]*n3 + rnorm(n)
  n7 <- weights[6,7]*n6 + rnorm(n)
  
  return(data.frame(n1 = n1, n2=n2, n3=n3, n4=n4, n5=n5 , n6= n6, n7=n7))
}

redone_exp5_data_full <- generate_exp5()

## now i need to add in the mnar data 

mnar_exp5 <- function(row){
  if(row[1] >= t_means[1] + t_sd[1]){
    row[3] <- NA 
    row[1] <- NA
  }
  if(row[6]<= t_means[6]-t_sd[6]){
    row[5] <- NA
  }
  return(row)
}

redone_exp5_data <-  as.data.frame(t(apply(redone_exp5_data_full, 1, mnar_exp5)))

## I found and confirmed that the bug had to do with how the data was generated
# there was no error forward in the way that i generated in the other data, which is why i think 
#it didn't work :) 
pcalg::zStat(2,4, S=NULL, C = cor(redone_exp5_data), n = nrow(redone_exp5_data))
pcalg::zStat(2,4, S=3, C = cor(redone_exp5_data), n = nrow(redone_exp5_data))




## So now I want to redo the z score computation but only on some specific tests 


impute <- function(dataset) {
  # Ensure predictor matrix matches dataset dimensions
  n_vars <- ncol(dataset)
  pred_matrix <- true_adj[1:n_vars, 1:n_vars]
  
  # Force matching names
  colnames(pred_matrix) <- colnames(dataset)
  rownames(pred_matrix) <- colnames(dataset)
  
  complete(mice(dataset, m = 1, maxit = 5, method = "norm.predict", 
                printFlag = F, predictorMatrix = pred_matrix))
}


singe_cit_stability <- function(data, v1, v2, S){
  S_char <- if(is.null(S)) "" else as.character(S)
  replicate_results <- data.frame(
    alpha = numeric(), 
    x = numeric(), 
    y = numeric(), 
    S = character(), 
    z_score = numeric(), 
    indep_test = character(),
    replicate = numeric(),
    stringsAsFactors = TRUE
  )
  
  for(rep in 1:5){
    n = nrow(data)
    
    # Original data (alpha = 0, no missingness)
    temp_z <- data.frame(
      alpha = 0, 
      x = v1, 
      y = v2, 
      S = S_char, 
      z_score = pcalg::zStat(v1, v2, S, cor(data), n), 
      indep_test = paste0(v1, ",", v2, "|",S_char, collapse = ""), 
      replicate = rep,
      stringsAsFactors = TRUE
    )
    # print(temp_z)
    
    replicate_results <- rbind(replicate_results, temp_z)  # Changed from 'temp' to 'temp_z'
    # print(replicate_results)
    
    # Loop through missingness levels
    for(a in seq(0.02, 0.5, 0.05)){
      
      data_induced = apply(data, c(1,2), induce_na, p_missing = a)
      
      # Impute the dataset
      imp_data <- impute(data_induced)
      n = nrow(imp_data)
      
      # Changed: use 'a' for alpha, 'imp_data' for correlation
      temp_z <- data.frame(
        alpha = a,  # Changed from 0 to a
        x = v1, 
        y = v2, 
        S = S_char, 
        z_score = pcalg::zStat(v1, v2, S, cor(imp_data), n),  # Changed from cor(data) to cor(imp_data)
        indep_test = paste0(v1, ",", v2, "|", S_char, collapse = ""), 
        replicate = rep,
        stringsAsFactors = TRUE
      )
      
      replicate_results <- rbind(replicate_results, temp_z)  # Changed from 'temp' to 'temp_z'
    }
  }
  
  return(replicate_results)  # Moved outside the for loop
}
  






redone_exp5_res_true <- singe_cit_stability(redone_exp5_data, 2, 4, 3 )
redone_exp5_res_true<- rbind(redone_exp5_res_true,singe_cit_stability(redone_exp5_data, 2, 4, NULL ))
redone_exp5_res_true<- rbind(redone_exp5_res_true,singe_cit_stability(redone_exp5_data, 3, 5, 4 ))
redone_exp5_res_true<- rbind(redone_exp5_res_true,singe_cit_stability(redone_exp5_data, 3, 5, NULL ))
redone_exp5_res_true<- rbind(redone_exp5_res_true,singe_cit_stability(redone_exp5_data, 5, 6, NULL ))
# redone_exp5_res_true<- rbind(redone_exp5_results,singe_cit_stability(redone_exp5_data, 5, 6, c(3,4)))
redone_exp5_res_true$matrix <- "true"



impute <- function(dataset) {
  # Ensure predictor matrix matches dataset dimensions
  n_vars <- ncol(dataset)
  pred_matrix <- me_adj[1:n_vars, 1:n_vars]
  
  # Force matching names
  colnames(pred_matrix) <- colnames(dataset)
  rownames(pred_matrix) <- colnames(dataset)
  
  complete(mice(dataset, m = 1, maxit = 5, method = "norm.predict", 
                printFlag = F, predictorMatrix = pred_matrix))
}
redone_exp5_res_false <- singe_cit_stability(redone_exp5_data, 2, 4, 3 )
redone_exp5_res_false<- rbind(redone_exp5_res_false,singe_cit_stability(redone_exp5_data, 2, 4, NULL ))
redone_exp5_res_false<- rbind(redone_exp5_res_false,singe_cit_stability(redone_exp5_data, 3, 5, 4 ))
redone_exp5_res_false<- rbind(redone_exp5_res_false,singe_cit_stability(redone_exp5_data, 3, 5, NULL ))
redone_exp5_res_false<- rbind(redone_exp5_res_false,singe_cit_stability(redone_exp5_data, 5, 6, NULL ))
redone_exp5_res_false$matrix <- "markov equiv"



# ggplot(redone_exp5_res_false, aes(x = alpha, y = z_score))+ 
#   geom_smooth(aes(color=indep_test)) + 
#   geom_hline(yintercept = 1.959964, linetype = "dotted") + 
#   geom_hline(yintercept = -1.959964, linetype = "dotted") 



impute <- function(dataset) {
  # Ensure predictor matrix matches dataset dimensions
  n_vars <- ncol(dataset)
  pred_matrix <- random_adj[1:n_vars, 1:n_vars]
  
  # Force matching names
  colnames(pred_matrix) <- colnames(dataset)
  rownames(pred_matrix) <- colnames(dataset)
  
  complete(mice(dataset, m = 1, maxit = 5, method = "norm.predict", 
                printFlag = F, predictorMatrix = pred_matrix))
}
redone_exp5_res_random <- singe_cit_stability(redone_exp5_data, 2, 4, 3 )
redone_exp5_res_random<- rbind(redone_exp5_res_random,singe_cit_stability(redone_exp5_data, 2, 4, NULL ))
redone_exp5_res_random<- rbind(redone_exp5_res_random,singe_cit_stability(redone_exp5_data, 3, 5, 4 ))
redone_exp5_res_random<- rbind(redone_exp5_res_random,singe_cit_stability(redone_exp5_data, 3, 5, NULL ))
redone_exp5_res_random<- rbind(redone_exp5_res_random,singe_cit_stability(redone_exp5_data, 5, 6, NULL ))
redone_exp5_res_random$matrix <- "random"



# ggplot(redone_exp5_res_random, aes(x = alpha, y = z_score))+ 
#   geom_smooth(aes(color=indep_test)) + 
#   geom_hline(yintercept = 1.959964, linetype = "dotted") + 
#   geom_hline(yintercept = -1.959964, linetype = "dotted") 


redone_exp5_results_full <- rbind(redone_exp5_res_true,redone_exp5_res_false)
redone_exp5_results_full <- rbind(redone_exp5_results_full, redone_exp5_res_random)



facet_labels <- c(
  "2,4|3" = "X2 not indep X4 | X3",
  "2,4|" = "X2 indep X4",
  "3,5|" = "X3 not indep X5",
  "3,5|4" = "X3 indep X5 | X4",
  "5,6|" = "X5 indep X6"
)

redone_exp5_results_full%>% filter(indep_test != "5,6|")  %>% group_by(indep_test, alpha, matrix) %>% mutate(z_score = mean(z_score)) %>%
  ggplot(aes(x = alpha, y = z_score))+ 
  geom_line(aes(color=matrix), method = "loess") + 
  geom_hline(aes(yintercept = 1.959964, linetype = "α = 0.05")) + 
  geom_hline(aes(yintercept = -1.959964, linetype = "α = 0.05")) + 
  geom_hline(aes(yintercept = 2.575829, linetype = "α = 0.01")) + 
  geom_hline(aes(yintercept = -2.575829, linetype = "α = 0.01")) + 
  facet_grid(~indep_test, labeller =labeller(indep_test = facet_labels))+
  scale_linetype_manual(
    name = "Significance Level",
    values = c("α = 0.05" = "dashed", "α = 0.01" = "dotted")
  ) +
  scale_color_discrete(
    name = "Prediction Matrix",
    labels = c("true" = "True Causal Model", 
               "markov equiv" = "Markov Equivalent",
               "random" = "Random Model")
  )+
  labs( x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + theme_light()

