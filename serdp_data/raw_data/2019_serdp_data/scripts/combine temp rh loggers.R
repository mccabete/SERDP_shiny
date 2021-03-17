# Combine individual temp/rh logger files into single datagrame ####

library(plyr)
library(dplyr)
library(readr)


# setwd("~/Desktop/TSA")

# Logger1a=read.csv("~/Desktop/TSA/temprh01-20381844-20180827.csv")
# Logger1b=read.csv("~/Desktop/TSA/temprh01-20381844-20181025.csv")
# (Logger1a)

pd = "data/raw_data/2019_serdp_data/2019-logger-sampling/final_round_8_6_2019/" # parent directory

file_list <- list.files("data/raw_data/2019_serdp_data/2019-logger-sampling/final_round_8_6_2019/", pattern = "csv")

# df <- ldply(file_list, read_csv)
# names(df) <- col_names

# length(stringr::str_sub(f, start = 7, end = 8))

col_names <- c("date","time","tempC","RH")# column names
df1 <- data.frame()# empty dataframe

## Function to loop through and combine files
for(f in file_list){
  n = read_csv(paste(pd,f,sep = ""),skip =1)
  n = n[,c(1:4)]
  names(n) <- col_names
  n$logger_id = paste(stringr::str_sub(f, start = 1, end = 2))
  df1 <- rbind(df1, n)
}

head(df1)
tail(df1)
unique(df1$logger_id)
View(df1)
summary(df1)
#tail(filter(df1, date == "2019-05-23"))
df1 <- filter(df1, is.na(tempC)==FALSE)# remove NA rows from data

write_csv(df1, "data/processed_data/2019-final-round-loggers-serdp.csv")

#cool dude