library(magrittr)
library(dplyr)
library(tidytext)

# a. 
tidyText <- NULL
for (i in 1:6) {
  season <- paste0("S0",i)
  for (j in 1:10) {
    episode <- ifelse(j<10,paste0("E0",j), paste0("E",j))
    seaEp <- paste0(season, episode)
    
    file <- scan(seaEp, what = "character", quiet = T, quote = "")
    words <- data.frame(file)
    colnames(words) <- c("word")
    words %>%
      filter(!grepl("[0-9]{2,}[:, ]|[0-9]{3}", word))%>%
      filter(!grepl("-->",word))%>%
      filter(!grepl("^[0-9]+", word))%>%
      filter(!grepl("-",word))%>%
      filter(!grepl("[A-Z]{2,}",word))%>%
      mutate(word=gsub("\\?","",word))%>%
      mutate(word=gsub("\\.","",word))%>%
      mutate(word=gsub("^<i>","",word))%>%
      mutate(word=gsub("</i>$","",word))%>%
      filter(!grepl(">",word))%>%
      filter(!grepl("<",word))%>%
      filter(!grepl("&",word))%>%
      mutate(word=gsub("\\,","",word))-> words
    
    words <- cbind(words, season = rep(season,nrow(words)), episode = rep(episode,nrow(words)))
    
    tidyText <- rbind(tidyText, words)
  }
}

tidyText%>%
  count(word,sort=TRUE)
# Removing stop words
tidyText%>%
  anti_join(stop_words)%>%
  count(word,sort=TRUE)

stopWords <- stop_words
stopWords%>%
  filter(grepl("i'm",word))
# All words in the dictionary are in lower case
# change all words to lower case for improved 
# stop word removal

tidyText%>%
  mutate(word = tolower(word))%>%
  anti_join(stop_words)-> tidyText

tidyText%>%
  count(word,sort=TRUE)
# results have been improved
for (i in 1:6) {
  sea <- paste0("S0",i)
    tidyText%>%
      filter(grepl(sea, season))%>%
      count(word,sort=TRUE)%>%
      head(.,10)%>%
      as.data.frame(.)%>%
      assign(sea,.,inherits = TRUE)
}
# b.

# c.
# Merging Season and Episode to a single variable
tidyText%>%
  mutate(seaEp = paste0(season,episode))%>%
  bind_tf_idf(word,seaEp)