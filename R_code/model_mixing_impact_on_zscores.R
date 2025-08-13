# Libraries 
library(pcalg)
library(dplyr)
library(gRbase)

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

# gen_dataset2 <- function(n) {
#   A <- rnorm(n)
#   D <- rnorm(n)
#   C <- D + rnorm(n)
#   B <- A + C + rnorm(n)
#   # A -> B 
#   # D -> C -> B 
#   return(data.frame(A = A, B=B, C=C, D=D))
# }

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



#compute and record all z scores 

# compute_zscores <- function(variables) {
#   # for z scores will need
#   pcalg::zStat()
#   
# }

compute_zscores <- function(C, n) {
  " 
  reused piece of code following the format of 
  test_all_cond_independencies from comparing_conditional_indep_among_imp_datasets.R
  C : correltion matrix 
  n : number of observations 
  results: list that needs to then be converted to a dataframe .. or ? 
  "
  vars <- 1:4
  results <- list()
  count <- 1
  
  for (i in 1:(length(vars)-1)) {
    for (j in (i+1):length(vars)) {
      x <- vars[i]
      y <- vars[j]
      # print(i)
      # print(j)
      # All subsets of remaining nodes (as conditioning sets)
      cond_vars <- setdiff(vars, c(x, y))
      # print(cond_vars)
      # for (k in 0:length(cond_vars)) {
      cond_sets <- all_subsets(cond_vars)
      
      # test null set 
      # pval <- condIndFisherZ(x, y, NULL, C, n, cutoff = 1.959964)
      z_score <- pcalg::zStat(x,y, S = NULL, C= C, n = n)
      
      results[[count]] <- list(x = x, y = y, S = NULL, z_stat = z_score)
      count <- count +1
      for (s in 1:length(cond_sets)) {
        # S <- as.integer(cond_sets[s, cond_sets[s, ] != 0])
        S <- cond_sets[[s]]
        # print(S)
        # pval <- condIndFisherZ(x, y, S, C, n, cutoff = 1.959964) # alpha =0.05
        z_score <- pcalg::zStat(x,y, S = S, C= C, n = n)
        
        # print(pval)
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
  alphas <- seq(0.1,0.9,by = 0.02 )
  
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




# format the data frame properly 

  
##  put them into a dataframe 
# the infor needed would be 
# alpha = mixing percentage of ds 2 
# conditional indep test
  # x , y , sep set 
# z score 


  # for(i in 1:length(results)){
  #   results_df <- data.frame(
  #     imp_method = "soft_imp",
  #     set_size = length(ci_results[[i]]$S),
  #     is_correct = (ci_results[[i]]$pval == ci_soft_imp[[i]]$pval)
  #   ) 


  
  
# visualizaiton 

# run the experiment with 1000 observations 
exp_results <- run_experiment(10000)

# mofidfy the indep test to be a factor   
exp_results$indep_test <- as.factor(exp_results$indep_test)

exp_results  %>% 
  ggplot(aes(x = alpha, y = z_score)) + 
  geom_line(aes(color = indep_test)) + 
  geom_vline(xintercept = 0.1, linetype = "dotted") + 
  geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted", color = "red") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted", color = "red")

