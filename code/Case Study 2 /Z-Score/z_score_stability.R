# Libraries 
library(pcalg)
library(dplyr)
library(gRbase)
library(ggplot2)
source("/Users/eodole/Desktop/Masters_Thesis/R_code/utils.R")
library(latex2exp)
library(reticulate)

# First need two data sets with two different models 

gen_dataset1 <- function(n) {
  A <- rnorm(n)
  C <- rnorm(n)
  B <- A + C + rnorm(n)
  D <- C + rnorm(n)
  # A -> B 
  # C -> B 
  # C -> D
  return(data.frame(A = A, B=B, C=C, D=D))
}


gen_dataset2 <- function(n) {
  A <- rnorm(n)
  B <- rnorm(n)
  C <- A + B + rnorm(n)
  D <- C+  rnorm(n)

  return(data.frame(A = A, B=B, C=C, D=D))
}

# dataset mixing 

dataset_mixing <- function(ds1, ds2, alpha){
  new_ds = rbind(slice_sample(ds1, prop = alpha), slice_sample(ds2, prop = (1-alpha)))
  return(new_ds)
}





compute_zscores <- function(C, n) {
  " 
  
  C : correltion matrix 
  n : number of observations 
  results: list that needs to then be converted to a dataframe 
  "
  vars <- 1:4
  results <- list()
  count <- 1
  
  for (i in 1:(length(vars)-1)) {
    for (j in (i+1):length(vars)) {
      x <- vars[i]
      y <- vars[j]
      
      # All subsets of remaining nodes (as conditioning sets)
      cond_vars <- setdiff(vars, c(x, y))
      cond_sets <- all_subsets(cond_vars)
      
      # test null set 
      z_score <- pcalg::zStat(x,y, S = NULL, C= C, n = n)
      
      results[[count]] <- list(x = x, y = y, S = NULL, z_stat = z_score)
      count <- count +1
      for (s in 1:length(cond_sets)) {
        
        S <- cond_sets[[s]]
        z_score <- pcalg::zStat(x,y, S = S, C= C, n = n)
        
        
        results[[count]] <- list(x = x, y = y, S = S, z_stat = z_score)
        count <- count + 1
      }
      
    }
  }

  
  return(results)
}
  

run_experiment <- function(n) {
  # the infor needed would be 
  # alpha = mixing percentage of ds 2 
  # conditional indep test
  # x , y , sep set 
  # z score 
  
  z_scores_results <- data.frame(
   alpha = numeric(), 
   x = numeric(), 
   y = numeric(), 
   S = numeric(), 
   z_score = numeric()
  )
  
  df1 <- gen_dataset1(n)
  df2 <- gen_dataset2(n)
  
  # different mixing percentages 
  alphas <- seq(0.02,0.98,by = 0.02 )
  
  # alpha = 0 
  temp_z <- compute_zscores(cor(df1), n)
    
  # change results into df format 
  for (i in 1:length(temp_z)){
    temp <- data.frame(
      alpha = 0, 
      x = temp_z[[i]]$x,
      y =  temp_z[[i]]$y,
      S = paste(temp_z[[i]]$S, collapse =''), ### currently having problems with the seperating set idk how to make it behave nicely
      indep_test = paste0(temp_z[[i]]$x, ",", temp_z[[i]]$y, "|", paste(temp_z[[i]]$S, collapse ='')),
      z_score =  temp_z[[i]]$z_stat
    )
    z_scores_results <- rbind(z_scores_results, temp)
  }
  
  # alpha 0.1 to 0.9 
  
  for(alpha in alphas){
    df <- dataset_mixing(df1,df2, alpha )
    temp_z <- compute_zscores(cor(df), n)
    
    for (i in 1:length(temp_z)){
      temp <- data.frame(
        alpha = alpha, 
        x = temp_z[[i]]$x,
        y =  temp_z[[i]]$y,
        S = paste(temp_z[[i]]$S, collapse =''), ### currently having problems with the seperating set idk how to make it behave nicely
        indep_test = paste0(temp_z[[i]]$x, ",", temp_z[[i]]$y, "|", paste(temp_z[[i]]$S, collapse ='')),
        z_score =  temp_z[[i]]$z_stat
      )
      z_scores_results <- rbind(z_scores_results, temp)
    }
    
  }
  
  # alpha = 1 
  temp_z <- compute_zscores(cor(df2), n)
  # change results into df format 
  for (i in 1:length(temp_z)){
    temp <- data.frame(
      alpha = 1, 
      x = temp_z[[i]]$x,
      y =  temp_z[[i]]$y,
      S = paste(temp_z[[i]]$S, collapse =''), ### currently having problems with the seperating set idk how to make it behave nicely
      indep_test = paste0(temp_z[[i]]$x, ",", temp_z[[i]]$y, "|", paste(temp_z[[i]]$S, collapse ='')),
      z_score =  temp_z[[i]]$z_stat
    )
    z_scores_results <- rbind(z_scores_results, temp)
  }
  
  return(z_scores_results)
}


  
  
# visualizaiton 

# run the experiment with 1000 observations 

exp_results <- run_experiment(10000)
exp_results$indep_test <- as.factor(exp_results$indep_test)

visualise_experiment <- function(experment_results){
  exp_results  %>% 
    ggplot(aes(x = alpha, y = z_score)) + 
    geom_line(aes(color = indep_test)) + 
    # geom_vline(xintercept = 0.1, linetype = "dotted") + 
    # geom_vline(xintercept = 0.9, linetype = "dotted") + 
    geom_hline(yintercept = 1.959964, linetype = "dotted") + 
    geom_hline(yintercept = -1.959964, linetype = "dotted") + 
    labs(title = "Experiment 2",subtitle = "Mixed Models", x = "Mixing Percentage p", y = "Z Score" ) + 
    theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
}



visualise_experiment(exp_results)


# for mcar 
run_experiment2_extension <- function(data){
  
  n = nrow(data)
  # initialize results df 
  z_scores_results <- data.frame(
    alpha = numeric(), 
    x = numeric(), 
    y = numeric(), 
    S = numeric(), 
    z_score = numeric()
  )
  
  # alpha = 0 
  temp_z <- compute_zscores(cor(data), n)
  for (i in 1:length(temp_z)){
    temp <- data.frame(
      alpha = 0, 
      x = temp_z[[i]]$x,
      y =  temp_z[[i]]$y,
      S = paste(temp_z[[i]]$S, collapse =''), ### currently having problems with the seperating set idk how to make it behave nicely
      indep_test = paste0(temp_z[[i]]$x, ",", temp_z[[i]]$y, "|", paste(temp_z[[i]]$S, collapse ='')),
      z_score =  temp_z[[i]]$z_stat
    )
    z_scores_results <- rbind(z_scores_results, temp)
  }
  
  
  for(a in seq(0.02, 0.5, 0.02)){
    # create missing dataset with mcar
    data_induced = apply(data, c(1,2), induce_na, p_missing = a)
    #impute the dataset
    imp_data <- impute(data_induced)
    n = nrow(imp_data)
    #estimate z scores
    temp_z <- compute_zscores(cor(imp_data), n)
    # print(temp_z)
    for (i in 1:length(temp_z)){
      temp <- data.frame(
        alpha = a,
        x = temp_z[[i]]$x,
        y =  temp_z[[i]]$y,
        S = paste(temp_z[[i]]$S, collapse =''), ### currently having problems with the seperating set idk how to make it behave nicely
        indep_test = paste0(temp_z[[i]]$x, ",", temp_z[[i]]$y, "|", paste(temp_z[[i]]$S, collapse ='')),
        z_score =  temp_z[[i]]$z_stat
      )
      z_scores_results <- rbind(z_scores_results, temp)
    }
  }
  # change z to factor
  z_scores_results$indep_test <- as.factor(z_scores_results$indep_test)



  #return results
  return(z_scores_results)
}


# Mean Imputation 
impute <- function(dataset){
  print("mean")
  return(mean_impute(dataset))
}

set.seed(123)
df = gen_dataset1(10000)
res = run_experiment2_extension(df)

res  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Mean Imputation", x = TeX("Missingness Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))



# Complete Case Imp
impute <- function(dataset){
  print("comp_case")
  return(dataset[complete.cases(dataset),])
  
}

res2 = run_experiment2_extension(df)

res2  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Complete Case Analysis", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


# now i need ot import python libraries into R
use_condaenv("thesis3")
source_python("/Users/eodole/Desktop/Masters_Thesis/R_code/python_impute_helpers.py")

#testing 
# l <- gen_dataset1(100)
# l <- apply(l, c(1,2), induce_na, p_missing = 0.2)
# py_mice(l, c("A", "B", "C", "D"))


impute <- function(dataset){
  print("mice")
  return(py_mice(dataset,c("A", "B", "C", "D")))
}

res3 = run_experiment2_extension(df)

res3  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "MICE (1)", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


impute <- function(dataset){
  print("m2")
  return(py_mice2(dataset,c("A", "B", "C", "D")))
}

res4 = run_experiment2_extension(df)
res4  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "MICE (10)", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


# soft impute 

impute <- function(dataset){
  print("soft")
  return(py_softimp(dataset))
}

res5 = run_experiment2_extension(df)
res5  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "SoftImpute", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


true_pred = matrix(c(0,0,0,0,
                1,0,1,0,
                0,0,0,0,
                0,0,1,0), ncol = 4, byrow = T)

false_pred = matrix(c(0,0,0,1,
                   1,0,1,0,
                   1,0,0,0,
                   1,0,1,0), ncol = 4, byrow = T) # arbitrary 


impute <- function(dataset){
  print("mice37")
  return(complete(mice(dataset, m =1, maxit = 10, 
                       method = 'pmm', 
                       seed = 123, 
                       # predictorMatrix = true_pred, 
                       printFlag = F)))
}

res6 = run_experiment2_extension(df)
res6  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Default Prediction Matrix & Predictive Mean Matching", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))



## Here we want to do an extension on experiment 4 

## have the large dataset tht we have 
# has already been read in 

## have the prediction matrix 
## have the false matrix 

#defne the impute method, I will have linear model and the diff pred matrices, 

res6 = run_experiment2_extension(df)
res6  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Default Prediction Matrix & Predictive Mean Matching", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


