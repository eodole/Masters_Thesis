# Comparing Imputation Methods on the numbers 

library(readr)

library(xtable)
library(dplyr)

presentation1_2 <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/presentation1.2.csv", col_names = FALSE)
View(presentation1_2)

## first is with 5  percents 
# mean_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/mean_imp_data.csv", col_names = FALSE)

## second is with 50 percents
mean_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/mean_imp_data.csv", col_names = FALSE)

## check diff 
# mean_diff <- (presentation1_2 - mean_imp_data)

# avg abs error 
# sum(abs(mean_diff))/4000

# avg adj abs error 
# sum(abs(mean_diff))/(sum(mean_diff != 0 ))

# avg abs error by column 
# apply(abs(mean_diff), MARGIN = 2, FUN =  sum )/1000  

# avg adj abs error by col 

  # adjustment 
  # apply(mean_diff != 0, MARGIN = 2, FUN = sum)

  # comparison by col 
  # apply(abs(mean_diff), MARGIN = 2, FUN =  sum )/apply(mean_diff != 0, MARGIN = 2, FUN = sum)

  # here are all the values that are imputed for sure, but maybe be missing corrected predicted imputations 
  # so what i actually need is to use the places that are na in the missing data. 
  # mean_imp_data[mean_diff != 0]
  
# five_percent_missing <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/five_percent_missing.csv", row)

#misleading name but 
five_percent_missing <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/fifty_percent_missing.csv", col_names = TRUE)

five_percent_missing <- five_percent_missing[,2:5]

sum(is.na(five_percent_missing))

# sum(mean_diff != 0 )

## To Do : write a function that does all this and reports it cleanly so that i can make a nice table 
  
compare_to_gt <- function(truth, df2, missing_pattern = NA){
  diff <- abs(truth - df2)
  
  if(typeof(missing_pattern) == "list" ){
    n_imputed <- sum(is.na(missing_pattern))
    diff_in_imp_vals <- diff[is.na(missing_pattern)]
    # n_by_col <- (apply(is.na(missing_pattern), MARGIN = 2, FUN = sum))
  }else{
    n_imputed <- sum(diff != 0)
    diff_in_imp_vals <- diff[diff != 0]
    # n_by_col <- (apply(diff != 0, MARGIN = 2, FUN = sum))
  }
  
  ## add sd to reporting 
  
  abs_err <- sum(diff)/(nrow(truth)*ncol(truth))
  
  
  abs_adj_err <- sum(diff)/n_imputed
  
  
  abs_err_by_col <- apply(diff, MARGIN = 2, FUN = sum)/nrow(truth)
  
  abs_adj_err_by_col <- apply(diff, MARGIN = 2, FUN = sum)/apply(diff != 0, MARGIN = 2, FUN = sum) # slightly diff here 
  
  # print(abs_err)
  # print(abs_adj_err)
  # print(abs_err_by_col)
  # print(abs_adj_err_by_col)

  return(c(abs_err, abs_adj_err, abs_err_by_col, abs_adj_err_by_col))
}

d<- as.data.frame(t(compare_to_gt(presentation1_2, mean_imp_data, five_percent_missing)))
d<- `colnames<-`(d, c('abs_err', 'abs_adj_error', 'abs_err_c1','abs_err_c2','abs_err_c3','abs_err_c4', 'abs_adj_err_c1','abs_adj_err_c2','abs_adj_err_c3','abs_adj_err_c4'))


# to do: run compare to ground truth on the other datasets and then make a nice table reporting the abs error

#five percent 
# soft_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/mtrx_completion_data.csv", col_names = FALSE)

#fifty percent
soft_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/mtrx_completion_data.csv", col_names = FALSE)


temp <- as.data.frame(t(compare_to_gt(presentation1_2, soft_imp_data, five_percent_missing)))
temp<- `colnames<-`(temp, c('abs_err', 'abs_adj_error', 'abs_err_c1','abs_err_c2','abs_err_c3','abs_err_c4', 'abs_adj_err_c1','abs_adj_err_c2','abs_adj_err_c3','abs_adj_err_c4'))

d <- rbind(d, temp)

# five percent 
# mice_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/mice_data.csv", col_names = FALSE)

#fifty percent
mice_imp_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/fifty_percent/mice_data.csv", col_names = FALSE)

temp <- as.data.frame(t(compare_to_gt(presentation1_2, mice_imp_data, five_percent_missing)))
temp<- `colnames<-`(temp, c('abs_err', 'abs_adj_error', 'abs_err_c1','abs_err_c2','abs_err_c3','abs_err_c4', 'abs_adj_err_c1','abs_adj_err_c2','abs_adj_err_c3','abs_adj_err_c4'))

d <- rbind(d, temp)

d <- cbind("imp_type" = c("mean", "soft impute", "mice"), d)

xtable(select(d,c("imp_type", "abs_err", "abs_adj_error")))

xtable(select(d, setdiff(colnames(d), c("abs_err", "abs_adj_error"))))



