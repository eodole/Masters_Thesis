# Libraries 

library(pcalg)
library(tidyr)
library(dplyr)
library(ggplot2)


#Functions 

"Generate a linear normal dataset"
gen_dataset <- function(size){
  # A <- rnorm(1000)
  A <- rnorm(size,1,1) # change to not be standard normal
  B<- 2*A + rnorm(size)
  C <- 3*A + 1 + rnorm(size)
  D <- -1*C + rnorm(size)
  
  return(data.frame(A = A, B = B, C= C, D=D))
}

"Function to use with apply for induce missingness MCAR data"
induce_na <- function(x, p_missing){
  r <- runif(1)
  if( r < p_missing){
    return (NA)
  }else{
    return(x)
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


# Pipeline 
#for % 5 -95 
p = seq(0,0.95, 0.05)
size = 1000

# gen data 
data <- gen_dataset(size)

for( j in 1:length(p)){
  
  missing_p = p[j]
  # create indep recorders 
  BindepCgivA <- c()
  BindepDgiveA <-  c()
  AindepC <- c()
  BindepA <- c()
  AindepDgivC <- c()
    
    for(i in 1:10){ # 10 repititons
    
      if(missing_p>0){
        # induce data 
        data2 <- apply(data, c(1,2), induce_na, p_missing = missing_p)
        # print("missing p real:")
        # print(missing_p)
        # print("missing_p est:")
        # print(sum(is.na(data2))/4000)
        # impute data 
        data2 <- mean_impute(data2)
      }else{
        data2 <- data
      }
      
      # calculate corr matrix
      Sigma <- cor(data2)
      print(Sigma)
      # calc & record cond indep 
      # A/ B
      BindepA <- c(BindepA, pcalg::gaussCItest(1,2, NULL, list(C=Sigma, n = size))) ## A/ B| empty
      
      AindepC <- c(AindepC, pcalg::gaussCItest(1,3, NULL, list(C=Sigma, n = size)))
      
      AindepDgivC <- c(AindepDgivC, pcalg::gaussCItest(1,4, 3, list(C=Sigma, n = size)) )
      
      BindepDgiveA <- c(BindepDgiveA, pcalg::gaussCItest(2,4, 1, list(C=Sigma, n = size)) ) 
      
      BindepCgivA <- c(BindepCgivA, pcalg::gaussCItest(2,3, 1, list(C=Sigma, n = size)) )
    }         
    
  if(missing_p==0){
    #create data frame 
    p_vals_df <- data.frame( AindepC = AindepC, AindepDgivC = AindepDgivC, BindepA = BindepA, BindepCgivA = BindepCgivA, BindepDgiveA = BindepDgiveA)
    
    # reshape data frame 
    p_vals_df <- pivot_longer(p_vals_df, everything(), names_to = "conditional_indep", values_to = "pval")
    
    # add missinging p 
    p_vals_df$missing_p <- rep(missing_p, nrow(p_vals_df))
  
  }else{
    p_vals_df_temp <- data.frame( AindepC = AindepC, AindepDgivC = AindepDgivC, BindepA = BindepA, BindepCgivA = BindepCgivA, BindepDgiveA = BindepDgiveA)
    p_vals_df_temp <- p_vals_df_temp %>% pivot_longer(everything(), names_to = "conditional_indep", values_to = "pval")
    p_vals_df_temp$missing_p <- rep(missing_p, nrow(p_vals_df_temp))
    
    p_vals_df <- bind_rows( p_vals_df,p_vals_df_temp)
  }
  
}
p_vals_df %>% filter(pval >0.5) %>% group_by(conditional_indep) %>% summarise(mini = unique(missing_p))


## Visualization
p_vals_df %>% filter(pval >=0.5) %>% #filter(conditional_indep %in% c("BindepCgivA", "BindepDgiveA"), pval >0.05) %>%
ggplot( aes(x = missing_p, y = pval)) + 
  geom_point() + 
  # geom_smooth() + 
  facet_grid(~conditional_indep)


## I also wanted to look at the graph that is being created 
sample_size = 10000
test <- gen_dataset(sample_size)
pc.fit2 <- pc(list(C = cor(test),n = sample_size ), indepTest = gaussCItest, alpha = 0.05, labels = colnames(test))
plot(pc.fit2, main = "Estimated CPDAG")

test2 <- mean_impute(apply(test, c(1,2), induce_na, p_missing =0.2))
pc.fit3 <- pc(list(C = cor(test2),n = sample_size ), indepTest = gaussCItest, alpha = 0.05, labels = colnames(test))
plot(pc.fit3, main = "Estimated CPDAG")
  
