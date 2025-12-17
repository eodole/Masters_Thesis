## Experiment 2 Repeated with permutation scores 

library(bnlearn) # ci.test
library(gRbase) # subsets
library(dplyr) # slice_sample
library(ggplot2) #data vis
library(latex2exp) #latex in data vis
library(reticulate)


# data generation function
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


compute_permutation_stat <- function(data) {
  ##redid the experiments again to also  calculate the p-value
  " 
  reused piece of code 'compute z scores'  in utils  with the modification of using permuation score. 
  "
  vars <- colnames(data) 
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
      # permutation_score <- bnlearn::ci.test(x,y, data = data, test =  "mc-mi-g")$statistic
      permutation_score <- bnlearn::ci.test(x,y, data = data, test =  "mc-mi-g")$p.value
      
      results[[count]] <- list(x = x, y = y, S = NULL, test_stat = permutation_score)
      count <- count +1
      for (s in 1:length(cond_sets)) {
        S <- cond_sets[[s]]
        permutation_score <- bnlearn::ci.test(x,y, S, data = data, test =  "mc-mi-g")$p.value
        results[[count]] <- list(x = x, y = y, S = S, test_stat = permutation_score)
        count <- count + 1
      }
      
    }
  }
  
  
  return(results)
}

### Experiment 

# taken from previous experiment with Z-scores 
dataset_mixing <- function(ds1, ds2, alpha){
  new_ds = rbind(slice_sample(ds1, prop = alpha), slice_sample(ds2, prop = (1-alpha)))
  return(new_ds)
}


run_experiment_mixed_models <- function(n) {
  # the infor needed would be 
  # alpha = mixing percentage of ds 2 
  # conditional indep test
  # x , y , sep set 
  # z score 
  
  results <- data.frame(
    alpha = numeric(), 
    x = numeric(), 
    y = numeric(), 
    S = numeric(), 
    test_stat = numeric()
  )
  
  # baseline case 
  df1 <- gen_dataset1(n)
  df2 <- gen_dataset1(n)

  # mixed model case 
  # df1 <- gen_dataset1(n)
  # df2 <- gen_dataset2(n)
  
  # different mixing percentages 
  alphas <- seq(0.02,0.98,by = 0.02 )
  
  # alpha = 0 
  temp_scores <- compute_permutation_stat(df1)
  
  # change results into df format 
  for (i in 1:length(temp_scores)){
    temp <- data.frame(
      alpha = 0, 
      x = temp_scores[[i]]$x,
      y =  temp_scores[[i]]$y,
      S = paste(temp_scores[[i]]$S, collapse =''), 
      indep_test = paste0(temp_scores[[i]]$x, ",", temp_scores[[i]]$y, "|", paste(temp_scores[[i]]$S, collapse ='')),
      test_stat =  temp_scores[[i]]$test_stat
    )
    results <- rbind(results, temp)
  }
  
  # alpha 0.1 to 0.9 
  
  for(alpha in alphas){
    df <- dataset_mixing(df1,df2, alpha )
    temp_scores <- compute_permutation_stat(df)
    
    for (i in 1:length(temp_scores)){
      temp <- data.frame(
        alpha = alpha, 
        x = temp_scores[[i]]$x,
        y =  temp_scores[[i]]$y,
        S = paste(temp_scores[[i]]$S, collapse =''), 
        indep_test = paste0(temp_scores[[i]]$x, ",", temp_scores[[i]]$y, "|", paste(temp_scores[[i]]$S, collapse ='')),
        test_stat =  temp_scores[[i]]$test_stat
      )
      results <- rbind(results, temp)
    }
    
  }
  
  # alpha = 1 
  temp_scores <- compute_permutation_stat(df2)
  # change results into df format 
  for (i in 1:length(temp_scores)){
    temp <- data.frame(
      alpha = 1, 
      x = temp_scores[[i]]$x,
      y =  temp_scores[[i]]$y,
      S = paste(temp_scores[[i]]$S, collapse =''), 
      indep_test = paste0(temp_scores[[i]]$x, ",", temp_scores[[i]]$y, "|", paste(temp_scores[[i]]$S, collapse ='')),
      test_stat =  temp_scores[[i]]$test_stat
    )
    results <- rbind(results, temp)
  }
  
  return(results)
}



# run the experiment with different imputation methods 
run_experiment2_permutation <- function(data){
  " here we compare the z score stability for differrent alphas,
  we can change the impute method by overwriting the impute method "
  replicate_results <- data.frame(
    alpha = numeric(),
    x = numeric(),
    y = numeric(),
    S = numeric(),
    test_stat = numeric(),
    indep_test = factor(),
    replicate = numeric()
  )

  for(rep in c(1:3)){

    # initialize results df
    test_stat_results <- data.frame(
      alpha = numeric(),
      x = numeric(),
      y = numeric(),
      S = numeric(),
      test_stat = numeric()
    )

    # alpha = 0

    temp_z <- compute_permutation_stat(data)
    for (i in 1:length(temp_z)){
      temp <- data.frame(
        alpha = 0,
        x = temp_z[[i]]$x,
        y =  temp_z[[i]]$y,
        S = paste(temp_z[[i]]$S, collapse =''), 
        indep_test = paste0(temp_z[[i]]$x, ",", temp_z[[i]]$y, "|", paste(temp_z[[i]]$S, collapse ='')),
        z_score =  temp_z[[i]]$test_stat
      )
      test_stat_results <- rbind(test_stat_results, temp)
    }


    for(a in seq(0.05, 0.5, 0.05)){
      # create missing dataset with mcar
      data_induced = apply(data, c(1,2), induce_na, p_missing = a)
      #impute the dataset
      imp_data <- impute(data_induced)
      n = nrow(imp_data)
      #estimate z scores
      # temp_z <- compute_zscores(cor(imp_data), n) ## fix
      temp_z <- compute_permutation_stat(imp_data)
      # print(temp_z)
      for (i in 1:length(temp_z)){
        temp <- data.frame(
          alpha = a,
          x = temp_z[[i]]$x,
          y =  temp_z[[i]]$y,
          S = paste(temp_z[[i]]$S, collapse =''), ### currently having problems with the seperating set idk how to make it behave nicely
          indep_test = paste0(temp_z[[i]]$x, ",", temp_z[[i]]$y, "|", paste(temp_z[[i]]$S, collapse ='')),
          z_score =  temp_z[[i]]$test_stat
        )
        test_stat_results <- rbind(test_stat_results, temp)
      }
    }
    # change z to factor
    test_stat_results$indep_test <- as.factor(test_stat_results$indep_test)
    test_stat_results$replicate <- rep(rep, nrow(test_stat_results))

    #return results
    replicate_results <- rbind(replicate_results, test_stat_results)
  }
  return(replicate_results)
}



  


### Mixed Model vs. Baseline 
mixed_model_out <- run_experiment_mixed_models(1000)
mixed_model_out$indep_test <- as.factor(mixed_model_out$indep_test)

mixed_model_out %>% ggplot(aes(x = alpha, y = test_stat)) + 
    geom_line(aes(color = indep_test)) + 
    # geom_vline(xintercept = 0.1, linetype = "dotted") + 
    # geom_vline(xintercept = 0.9, linetype = "dotted") + 
    # geom_hline(yintercept = 1.959964, linetype = "dotted") + 
    # # geom_hline(yintercept = -1.959964, linetype = "dotted") + 
    labs( x = "Mixing Percentage p", y = "P-Value" ) + 
    theme(legend.position="none", 
          plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          text = element_text(size = 20))


baseline_out <- run_experiment_mixed_models(1000) # before running i modified this code to have df also be df 1 

baseline_out$indep_test <- as.factor(baseline_out$indep_test)

baseline_out %>% ggplot(aes(x = alpha, y = test_stat)) + 
  geom_line(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  # geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  # # geom_hline(yintercept = -1.959964, linetype = "dotted") + 
  labs( x = "Mixing Percentage p", y = "P-Value" ) + 
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 20))





## Comparing Permutation Scores for Various Imputation Methods 
mean_impute <-function(df){
  " Impute the data with column means, i.e. feature means"
  mu_col <- colMeans(df, na.rm = T)
 
  
  for(row in 1:nrow(df)){
    for(col in 1:ncol(df)){
      if(is.na(df[row,col])){
        
        df[row,col] <- mu_col[col]
       
      }
    }
  }
  
  
  df <- as.data.frame(df)
  return(df)
}


use_condaenv("thesis3")
source_python("/Users/eodole/Desktop/Masters_Thesis/R_code/python_impute_helpers.py")

# Mean Imputation 
impute <- function(dataset){
  print("mean")
  return(mean_impute(dataset))
}

set.seed(123)
df = gen_dataset1(10000)
res = run_experiment2_permutation(df)

# baseline_out$indep_test <- as.factor(baseline_out$indep_test)
res$indep_test <- as.factor(res$indep_test)

res  %>% group_by(indep_test, alpha) %>% mutate(z_score = mean(z_score)) %>% ggplot(aes(x = alpha, y = z_score)) + 
  # geom_smooth(aes(color = indep_test)) + 
  geom_line(aes(color = indep_test)) +
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  # geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  labs(title = "Mean Imputation", x = TeX("Missingness Probability $ \\alpha $"), y = "P-Value" ) +
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 20))

impute <- function(dataset){
  print("mice")
  return(py_mice(dataset,c("A", "B", "C", "D")))
}

res_perm_mice1 = run_experiment2_permutation(df)

res_perm_mice1  %>% group_by(indep_test, alpha) %>% mutate(z_score = mean(z_score)) %>% ggplot(aes(x = alpha, y = z_score)) + 
  geom_line(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  # geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  # geom_hline(yintercept = -1.959964, linetype = "dotted") +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  labs(title = "MICE (1)", x = TeX("Missing Probability $ \\alpha $"), y = "P-Value" ) +
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 20))


# Complete Case Imp
impute <- function(dataset){
  print("comp_case")
  return(as.data.frame(dataset[complete.cases(dataset),]))
  
}

res_perm_cca = run_experiment2_permutation(df)

res_perm_cca    %>% group_by(indep_test, alpha) %>% mutate(z_score = mean(z_score)) %>% ggplot(aes(x = alpha, y = z_score)) + 
  geom_line(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  # geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  # geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Complete Case Analysis", x = TeX("Missing Probability $ \\alpha $"), y = "P-Value" ) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 20))


impute <- function(dataset){
  print("m2")
  return(py_mice2(dataset,c("A", "B", "C", "D")))
}

res_perm_mice10 = run_experiment2_permutation(df)
res_perm_mice10  %>% group_by(indep_test, alpha) %>% mutate(z_score = mean(z_score)) %>% ggplot(aes(x = alpha, y = z_score)) + 
  geom_line(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  # geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  # geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "MICE (10)", x = TeX("Missing Probability $ \\alpha $"), y = "P-Value" ) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 20))

impute <- function(dataset){
  print("soft")
  return(as.data.frame(py_softimp(dataset)))
}

res_perm_softimp = run_experiment2_permutation(df)
res_perm_softimp  %>% group_by(indep_test, alpha) %>% mutate(z_score = mean(z_score)) %>% ggplot(aes(x = alpha, y = z_score)) + 
  geom_line(aes(color = indep_test)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  # geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  # geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "SoftImpute", x = TeX("Missing Probability $ \\alpha $"), y = "P-Value" ) +
  geom_hline(yintercept = 0.05, linetype = "dotted") +
  theme(legend.position="none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(size = 20))


### Want to find out the way that these will actually fit causal graphs 
### so need to save the data 
#induce mcar missing high and low 
low_miss_df <- apply(df, c(1,2), induce_na, p_missing = 0.05)
high_miss_df <- apply(df, c(1,2), induce_na, p_missing = 0.5)

#
write.csv(df, file = "full_data.csv", row.names = F)
write.csv(low_miss_df, file = "low_missing_data.csv", row.names = F, na ="")
write.csv(high_miss_df, file = "high_missing_data.csv", row.names = F, na = "")





