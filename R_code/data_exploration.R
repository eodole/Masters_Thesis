
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

