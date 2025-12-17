library(mice)
library(gRbase)

## Dataset generation 
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


#Missing Data 
induce_na <- function(cell, p_missing){
  # the na is in
  r <- runif(1)
  if( r < p_missing){
    return (NA)
  }else{
    return(cell)
  }
}

mean_impute <-function(df){
  " Impute the data with column means, i.e. feature means"
  mu_col <- colMeans(df, na.rm = T)
  # print(mu_col)
  # print("========")
  
  for(row in 1:nrow(df)){
    for(col in 1:ncol(df)){
      if(is.na(df[row,col])){
        
        df[row,col] <- mu_col[col]
        # print(col)
        # print(mu_col[col])
      }
    }
  }
  return(df)
}

mse <- function(df_true, df_pred){
  if(nrow(df_true)!= nrow(df_pred)||ncol(df_true)!=ncol(df_pred)){
    print("Dims Don't Match")
    return(NA)
  }
  diff <- df_true - df_pred 
  return(sum(diff*diff)/(nrow(diff)*ncol(diff)))
}

mice_accuracy <- function(full_data, mice_param_list){
  "
  full_data: data_frame to analyze and compare against 
  mice_param_list: list of parameters to be passed to mice, should include 
    m (n datasets out),maxiter, method, and printFlag = F, could also include predictor matrix 
  "
  
  result_vect <- c()
  
  for(p in seq(0.2, 0.9, 0.1)){
    missing_data <- apply(full_data, c(1,2), induce_na, p_missing = p) 
    mice_param_list[["data"]] <- missing_data
    mice_param_list[["seed"]] <- 123
    temp_mids <- do.call(mice, mice_param_list)
    #print(temp_mids)
    #print(complete(temp_mids))
    
    result_vect <- c(result_vect, mse(full_data, complete(temp_mids)))
    # print(full_data - complete(temp_mids))
    # print(mse(full_data, complete(temp_mids)))
    # 
    # print(result_vect)
    # plain <- complete(mice(obs_data2, m =1, maxit = 10, method = 'norm.predict', seed = 123))
    # mse_plain_normpred <- c(mse_plain_normpred, mse(full_data, plain))
  }
  return(result_vect)
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


# for mcar 
run_experiment2_extension <- function(data){
  " here we compare the z score stability for differrent alphas,
  we can change the impute method by overwriting the impute method " 
  replicate_results <- data.frame(
    alpha = numeric(), 
    x = numeric(), 
    y = numeric(), 
    S = numeric(), 
    z_score = numeric(), 
    indep_test = factor(),
    replicate = numeric()
  )
  
  for(rep in c(1:3)){
    
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
    
    
    for(a in seq(0.05, 0.5, 0.05)){
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
    z_scores_results$replicate <- rep(rep, nrow(z_scores_results))
    
    #return results
    replicate_results <- rbind(replicate_results, z_scores_results)
}
return(replicate_results)
}

compute_zscores <- function(C, n) {
  " 
  reused piece of code following the format of 
  test_all_cond_independencies from comparing_conditional_indep_among_imp_datasets.R
  C : correltion matrix 
  n : number of observations 
  results: list that needs to then be converted to a dataframe .. or ? 
  "
  vars <- 1:nrow(C)
  # print("check")
  # print(vars)
  # print(nrow(C))
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
        # if(length(S) >3){
        #   next
        # }
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

