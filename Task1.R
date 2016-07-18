require(magrittr)
require(dplyr)

first <- scan("S01E01", what = "character", quiet = T, quote = "")
first <- data.frame(first)
first %>%
  filter(!grepl("[0-9]+[:, ]", first))%>%
  filter(!grepl("[0-9]+", first))%>%
  filter(!grepl("-->",first))%>%
  filter(!grepl("-",first))%>%
  filter(!grepl("[A-Z]{2,}",first))%>%
  mutate(first=gsub("\\?","",first))%>%
  mutate(first=gsub("\\.","",first))%>%
  mutate(first=gsub("\\,","",first))-> tidy_text