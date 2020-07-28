
#upload path of the folder containing all of your csv files.
#In case you have just one file, you can directly add its path to variable names files.
#files = "C:/....../file.csv"
files <- list.files(path = "C:/Users/user/Desktop/multi", pattern = "*.csv", full.names = T) 
reader <- function(x) {
  read.csv(x,sep = "\t", fileEncoding="utf-16", stringsAsFactors = F)
}

library(lubridate)
library(tidyverse)


#This function row binds all the files that was in the folder
attendance <- sapply(files, reader, simplify=FALSE) %>% 
  bind_rows(.id = NULL)

#Duration of the meeting (in hours)
dur = 2

#Add name of all the hosts (as in the Teams CSV files) who have organized the meetings since there name will be on the top of the file.
prof = c("K S Rao (Dr.)", "Prashant Dhamale")


attendance$User.Action[attendance$User.Action == "Joined before"] = "Joined"

data = attendance %>%
  mutate(
    datetime = parse_date_time(Timestamp, "%m/%d/%Y %H:%M:%S %p", tz = "Asia/Kolkata" ),
    date = as.Date(datetime),
    change = ifelse(User.Action == "Joined", 1,-1),
    prof = ifelse(Full.Name %in% prof, Full.Name, NA)
  )%>%
  fill(prof, .direction = "down") %>%
  group_by(prof) %>%
  mutate(
    end = round_date(first(datetime),"hour") + hours(dur)
    ) %>%
  ungroup() %>%
  filter(datetime < end) %>%
  mutate(
    attended = round(difftime(end, datetime, units = "mins")*change)
  )%>%
  group_by(date, prof, Full.Name) %>%
  summarize(
    totalmins_attended = sum(attended)
  )

#Add the name by which you would like to save the new file.
write.csv(data, 'attendance.csv')
