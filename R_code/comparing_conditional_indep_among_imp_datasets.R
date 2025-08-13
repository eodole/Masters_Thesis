# Goal: Compare The accuracy of conditional independence testing 

# first establish a ground truth 

library(readr)
library(pcalg)

ground_truth <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/presentation1.2.csv", col_names = FALSE)


# mean_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/mean_imp_data.csv", col_names = FALSE)
mean_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/mean_imp_data.csv", col_names = FALSE)


# BindepA <- c(BindepA, pcalg::gaussCItest(1,2, NULL, list(C=Sigma, n = size)))


### Vibes 

library(pcalg)
library(gRbase) # for 'subsets'

# Example: create some synthetic Gaussian data (as required by condIndFisherZ)
set.seed(42)
n <- 1000
p <- 4
data <- matrix(rnorm(n * p), ncol = p)
colnames(data) <- paste0("V", 1:4)

# Define sufficient statistics
# C <- list(C = cor(data), n = nrow(data))
C <- cor(data)

# All variables
vars <- 1:4

# Function to test all conditional independencies
test_all_cond_independencies <- function(vars, C, n) {
  "
  vars: vector of variable names 
  C: correlation matrix 
  n : dataset size 
  results: 
  "
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
      pval <- condIndFisherZ(x, y, NULL, C, n, cutoff = 1.959964)
      # print(pval)
      results[[count]] <- list(x = x, y = y, S = NULL, pval = pval)
      count <- count +1
      for (s in 1:length(cond_sets)) {
        # S <- as.integer(cond_sets[s, cond_sets[s, ] != 0])
        S <- cond_sets[[s]]
        # print(S)
        pval <- condIndFisherZ(x, y, S, C, n, cutoff = 1.959964) # alpha =0.05
        # print(pval)
        results[[count]] <- list(x = x, y = y, S = S, pval = pval)
        count <- count + 1
      }
      
    }
  }
  return(results)
}

# Run the function
C<- cor(ground_truth)
ci_results <- test_all_cond_independencies(vars, C, n = 1000)

# Print results
for (res in ci_results) {
  cat(sprintf("Test: %s _||_ %s | %s -> p=%.4f\n",
              paste0("V", res$x), paste0("V", res$y),
              paste0("V", res$S, collapse = ", "), res$pval))
}

C_mean <- cor(mean_imp_data)
ci_mean <- test_all_cond_independencies(vars, C_mean, n = 1000)




# compare results 

# empty df 

results = data.frame(
  imp_method = character(),
  set_size = integer(),
  is_correct = logical()
)


## for mean imputation 
for(i in 1:length(ci_results)){
 temp <- data.frame(
   imp_method = "mean",
   set_size = length(ci_results[[i]]$S),
   is_correct = (ci_results[[i]]$pval == ci_mean[[i]]$pval)
 ) 
 results <- rbind(results, temp)
}


## for complete cases 
# complete_case <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/complete_case.csv",  col_names = FALSE)
complete_case <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/complete_case.csv",  col_names = FALSE)

C_comp_case <- cor(complete_case)

ci_cc <- test_all_cond_independencies(vars, C_comp_case, n = nrow(complete_case))

for(i in 1:length(ci_results)){
  temp <- data.frame(
    imp_method = "complete_cases",
    set_size = length(ci_results[[i]]$S),
    is_correct = (ci_results[[i]]$pval == ci_cc[[i]]$pval)
  ) 
  results <- rbind(results, temp)
}


# for mice data 
# mice_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/mice_data.csv", col_names = FALSE)
mice_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/mice_data.csv", col_names = FALSE)


C_mice <- cor(mice_data)

ci_mice <- test_all_cond_independencies(vars, C_mice, n = 1000)


for(i in 1:length(ci_results)){
  temp <- data.frame(
    imp_method = "mice",
    set_size = length(ci_results[[i]]$S),
    is_correct = (ci_results[[i]]$pval == ci_mice[[i]]$pval)
  ) 
  results <- rbind(results, temp)
}

# for soft impute 

# mtrx_completion_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/mtrx_completion_data.csv", col_names = F)
mtrx_completion_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/mtrx_completion_data.csv", col_names = F)

C_soft_imp <- cor(mtrx_completion_data)

ci_soft_imp <- test_all_cond_independencies(vars, C_soft_imp, n=1000)


for(i in 1:length(ci_results)){
  temp <- data.frame(
    imp_method = "soft_imp",
    set_size = length(ci_results[[i]]$S),
    is_correct = (ci_results[[i]]$pval == ci_soft_imp[[i]]$pval)
  ) 
  results <- rbind(results, temp)
}


### Analysis Time 

library(dplyr)
library(ggplot2)

graph_results <- results %>% group_by(imp_method, set_size) %>% summarise(total = n(), n_correct = sum(is_correct), percentage = n_correct/total)

graph_results %>% ggplot(aes(x = set_size, y = percentage, fill = as.factor(imp_method))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    title = "Percentage of Correct Conditional Independence Tests By Set Size",
    fill = "Imputation Type",
    x = "Seperation Set Size",
    y = "Percentage Correct",
  )
  

