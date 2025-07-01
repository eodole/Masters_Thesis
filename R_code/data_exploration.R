
# Packages to read special data
library(R.matlab)
library(readxl)

#Other libaries
library(dplyr)
library(ggplot2)
library(sf)


alldata <- read_excel("./data/alldata.xlsx")

#alldata %>% summarise(anyNA())

#anyNA.data.frame(alldata)

#sapply(alldata, sum(is.na))


completeness <- sapply(alldata, anyNA)

sum(completeness) # we see from this that 437 columns have NAs or Nulls 

sum(complete.cases(alldata)) # this is also zero, so row wise also no complete cases but maybe this is the fault of the pesticide data idk

alldata %>% select(where(is.logical)) # noticed that some are interpreted by R as logical rather than numeric



# read in another dataset

countydataraw <- readMat("./data/CountyMatrix_raw.mat")

# move county data into a workable format
colnames <- unlist(countydataraw[["col"]])

countydata <- as.data.frame(countydataraw$d)
countydata <-`colnames<-`(countydata, colnames)

countydata$FIPS <- countydataraw$fips

sum(complete.cases(countydata)) # here we got that for the county data there are 40 complete rows

completeness_county <- sapply(countydata, anyNA)

sum(completeness_county) # there are also 57 incomplete columns which makes me think that actually fips is the only complete column

complete_subset <- countydata[complete.cases(countydata),]

mysf <- st_as_sf(complete_subset, coords = c("Longitude", "Lattitude"))
mysf<- st_set_crs(crs = 4326)

ggplot(mysf)+ 
  geom_sf()


# read in pesticides data 
pesticidesraw <- readMat("./data/pesticides2010_kgperarea.mat") # the dims dont match need to follow up 


### New 30.6 


library(readr)
full_dataset_v1 <- read_csv("Desktop/Masters_Thesis/data/processed/full_dataset_v1.csv")
View(full_dataset_v1)

missing_by_col <- apply(is.na(full_dataset_v1), 2, sum)

sum(is.na(full_dataset_v1))/(nrow(full_dataset_v1)*ncol(full_dataset_v1))
# [1] 0.7085253

sum(is.na(full_dataset_v1))


library(readr)
pesticide_data_2010 <- read_csv("Desktop/Masters_Thesis/data/processed/pesticide_data_2010.csv")
View(pesticide_data_2010)


sum(is.na(pesticide_data_2010))/(nrow(pesticide_data_2010) * ncol(pesticide_data_2010))
# [1] 0.7815698



missing_fips <- setdiff(full_dataset_v1$fips, pesticide_data_2010$FIPS)


# how much missign without pesticides? 
sum(is.na(full_dataset_v1[,2:60]))/(3142*59)


m <- data.frame(Data = c("Full Dataset", 
                         "Pesticide Only", 
                         "Demographic and Disease Only"),
                `Percentage Missing` = c(sum(is.na(full_dataset_v1))/(nrow(full_dataset_v1)*ncol(full_dataset_v1)), 
                                        sum(is.na(pesticide_data_2010))/(nrow(pesticide_data_2010) * ncol(pesticide_data_2010)), 
                                        sum(is.na(full_dataset_v1[,2:60]))/(3142*59)))


library(stargazer)

stargazer(m, summary = FALSE, rownames = FALSE)

library(naniar)


vis_miss(full_dataset_v1[,2:58], warn_large_data = FALSE,show_perc_col = FALSE )


## I want to make a missingness graph but for the generated data 

library(readr)
missing_data <- read_csv("Desktop/Masters_Thesis/Py_Code/gen_data/pres1/five_percent_missing.csv", col_names = FALSE)
View(presentation1_2)

missing_data = missing_data %>% rename(
  A = X2, 
  B= X3, 
  C= X4, 
  D =X5
)
missing_data = missing_data[2:1001,2:5 ]

vis_miss(missing_data)

sum(complete.cases(missing_data))/1000


## interesting visualization of compelte cases 
p_missing = seq(0,1, by =0.05)
P_complete_cases = (1000*(1-p_missing)^4)/1000
> plot(p_missing, P_complete_cases)



