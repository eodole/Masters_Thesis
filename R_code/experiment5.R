library(mice)
library(readr)


true_data <- read_csv("Desktop/Masters_Thesis/Py_Code/exp5/exp5_zscore_true_data.csv",  col_names = FALSE)
true_adj <- as.matrix(read_csv("Desktop/Masters_Thesis/Py_Code/exp5/exp5_zscore_true_adj.csv",  col_names = FALSE))
true_adj <- t(true_adj)
rownames(true_adj) <- colnames(true_data)
colnames(true_adj) <- colnames(true_data)

test<- true_data[1:10,]

t_means <- colMeans(true_data)
t_sd <- apply(true_data, 2, sd)

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

data <-  as.data.frame(t(apply(true_data, 1, mnar_exp5)))
write.csv(data, file = "Desktop/Masters_Thesis/Py_Code/exp5/exp5_induce mcar.csv")
sum(is.na(data))/(10000*7) # percentage of data missing 



experiment5_zscores <- function(data){
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
  
  for(rep in c(1:5)){ # changed back to 5 reps
    
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
    #impute the dataset
    imp_data <- impute(data)
    n = nrow(imp_data)
    #estimate z scores
    temp_z <- compute_zscores(cor(imp_data), n)
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

exp5_res_true_scm <- experiment5_zscores(data)

exp5_res_true_scm  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test),se = F) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


me_adj <- as.matrix(read_csv("Desktop/Masters_Thesis/Py_Code/exp5/exp5_zscore_me_adj.csv",  col_names = FALSE))
me_adj <- t(me_adj)
rownames(me_adj) <- colnames(true_data)
colnames(me_adj) <- colnames(true_data)

impute <- function(dataset){
  print("exp_me")
  return(complete(mice(dataset, 
                       m =1 ,maxit = 5, method = 'norm.predict', printFlag = F, predictorMatrix = me_adj)))
}


exp5_res_me_scm <- experiment5_zscores(data)
exp5_res_me_scm  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test),se = F) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

random_adj <- as.matrix(read_csv("Desktop/Masters_Thesis/Py_Code/exp5/exp5_zscore_random_adj.csv",  col_names = FALSE))
random_adj <- t(random_adj)
rownames(random_adj) <- colnames(true_data)
colnames(random_adj) <- colnames(true_data)

impute <- function(dataset){
  print("exp_random")
  return(complete(mice(dataset, 
                       m =1 ,maxit = 5, method = 'norm.predict', printFlag = F, predictorMatrix = random_adj)))
}


exp5_res_random_scm <- experiment5_zscores(data)
exp5_res_random_scm  %>%  ggplot(aes(x = alpha, y = z_score)) + 
  geom_smooth(aes(color = indep_test),se = F) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


## visualization 
exp5_res_random_scm$matrix <- "random"
exp5_res_me_scm$matrix <- "markov equiv"
exp5_res_true_scm$matrix <- "true"


exp5_results <- bind_rows(exp5_res_me_scm, exp5_res_random_scm, exp5_res_true_scm)
# exp5_results %>% ggplot(aes(x=alpha, y = z_score)) + geom_smooth(aes(color = matrix)) + facet_wrap(~indep_test)

pdf("Desktop/Masters_Thesis/Py_Code/exp5/exp5_results.pdf", width = 8, height = 6)

exp5_results %>%
  split(.$indep_test) %>%
  lapply(function(df) {
    p <- ggplot(df, aes(x = alpha, y = z_score)) + 
      geom_smooth(aes(color = matrix)) +
      geom_hline(yintercept = 1.959964, linetype = "dotted") + 
      geom_hline(yintercept = -1.959964, linetype = "dotted") +
      labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
      ggtitle(unique(df$indep_test))
    print(p)
  })


dev.off()

exp5_results %>% filter(indep_test == "1,2|46", replicate ==1) %>% 
  ggplot(aes(x=alpha, y=z_score)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_point(aes(color = matrix)) +
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))



## more close analyzing of the results 
exp5_res$replicate <- as.factor(exp5_results$replicate)
exp5_results %>% filter(replicate == 4)%>%group_by(indep_test, alpha, matrix) %>% mutate(z_score = mean(z_score)) %>% filter(indep_test == "6,7|") %>% 
  ggplot(aes(x=alpha, y=z_score)) + 
  # geom_vline(xintercept = 0.1, linetype = "dotted") + 
  # geom_vline(xintercept = 0.9, linetype = "dotted") + 
  geom_point(aes(color = matrix)) +
  geom_line(aes(color = matrix,, linetype = replicate))+
  geom_hline(yintercept = 1.959964, linetype = "dotted") + 
  geom_hline(yintercept = -1.959964, linetype = "dotted") +
  labs(title = "Correct Prediction Matrix Z Scores", x = TeX("Missing Probability $ \\alpha $"), y = "Z Score" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))


