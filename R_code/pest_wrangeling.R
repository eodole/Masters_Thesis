library(dplyr)
library(tidyr)

pest = read.delim("./data/EPest.county.estimates.2010.txt",colClasses = c("factor", "numeric", "character", "character", "numeric", "numeric"))

# change the fips codes to concatinate the state and county codes 
pest$FIPS = paste0( pest$STATE_FIPS_CODE ,pest$COUNTY_FIPS_CODE )

length(unique(pest$FIPS))
#3063 weirdly some are missing i guess? 
pest$FIPS = as.numeric(pest$FIPS)


length(unique(pest$COMPOUND)) 
# [1] 385

length(unique(pest$FIPS))
# [1] 3063

unique(pest$STATE_FIPS_CODE)
# [1] "01" "04" "05" "06" "08" "09" "10" "12" "13" "16" "17" "18" "19" "20"
# [15] "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34"
# [29] "35" "36" "37" "38" "39" "40" "41" "42" "44" "45" "46" "47" "48" "49"
# [43] "50" "51" "53" "54" "55" "56"

length(unique(pest$STATE_FIPS_CODE))
# [1] 48



pest2 = pest %>% select(c("COMPOUND", "FIPS", "EPEST_LOW_KG")) %>%
  reshape(direction = "wide", timevar = "COMPOUND", idvar = "FIPS") %>% 
  rename_with(~gsub("EPEST_LOW_KG.", "", .x))

getwd()
setwd("./data")
getwd()

write.csv(pest2, file = "pesticide_data_2010")
