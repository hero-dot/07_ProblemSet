library(magrittr)
library(dplyr)
library(tidytext)
library(ggplot2)
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
      filter(!grepl("_",word))%>%
      mutate(word=gsub("\\,","",word))-> words
    
    words <- cbind(words, season = rep(season,nrow(words)), episode = rep(episode,nrow(words)), seaEp = rep(seaEp,nrow(words)))
    
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
  filter(grepl("the",word))
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
# word-level sentiment analysis with bing

sentiments%>%
  filter(lexicon=="bing",sentiment=="negative")-> bingnegative

tidyText %>%
  inner_join(bingnegative) -> negativeWordsInGoT
  
keeps <- c("word", "seaEp")
negativeWordsInGoT[keeps] -> negativeWordsInGoT

xtabs(~ seaEp, negativeWordsInGoT) -> countTableWords
  
as.data.frame(countTableWords) -> nrNegativeWordsPerSeaEP

nrNegativeWordsPerSeaEP%>%
  mutate(seaEp = as.character(.$seaEp))%>%
  mutate(season = sapply(.$seaEp, function(x) strsplit(x,"E")[[1]][1]))%>%
  mutate(episode = sapply(.$seaEp, function(x) strsplit(x,"E")[[1]][2]))->nrNegativeWordsPerSeaEP

nrNegativeWordsPerSeaEP%>%
  group_by(season)%>%
  ggplot(.,aes(x=episode, y=Freq)) + 
  geom_point() +
  ggtitle("Negative Words in GoT - BING") +
  xlab("Episoden") +
  facet_wrap(~season)
  
# word-level sentiment analysis with AFINN

sentiments%>%
  filter(lexicon=="AFINN")%>%
  select(word, afinn_score = score)%>%
  filter(afinn_score < 0)-> afinnNegative

tidyText %>%
  inner_join(afinnNegative) -> negativeWordsInGoT1

keeps <- c("word", "seaEp")
negativeWordsInGoT1[keeps] -> negativeWordsInGoT1

xtabs(~ seaEp, negativeWordsInGoT1) -> countTableWords1

as.data.frame(countTableWords1) -> nrNegativeWordsPerSeaEP1

nrNegativeWordsPerSeaEP1%>%
  mutate(seaEp = as.character(.$seaEp))%>%
  mutate(season = sapply(.$seaEp, function(x) strsplit(x,"E")[[1]][1]))%>%
  mutate(episode = sapply(.$seaEp, function(x) strsplit(x,"E")[[1]][2]))->nrNegativeWordsPerSeaEP1

nrNegativeWordsPerSeaEP1%>%
  group_by(season)%>%
  ggplot(.,aes(x=episode, y=Freq)) + 
  geom_point() +
  ggtitle("Negative Words in GoT - AFINN") +
  xlab("Episoden") +
  facet_wrap(~season)
