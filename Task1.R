require(magrittr)
require(dplyr)

allText <- NULL
for (i in 1:6) {
  season <- paste0("S0",i)
  for (j in 1:10) {
    episode <- ifelse(j<10,paste0("E0",j), paste0("E",j))
    seaEp <- paste0(season, episode)
    
    file <- scan(seaEp, what = "character", quiet = T, quote = "")
    words <- data.frame(file)
    colnames(words) <- c("word")
    words %>%
      filter(!grepl("[0-9]+[:, ]", word))%>%
      filter(!grepl("[0-9]+", word))%>%
      filter(!grepl("-->",word))%>%
      filter(!grepl("-",word))%>%
      filter(!grepl("[A-Z]{2,}",word))%>%
      mutate(word=gsub("\\?","",word))%>%
      mutate(word=gsub("\\.","",word))%>%
      mutate(word=gsub("\\,","",word))-> tidyText
    
    tidyText <- cbind(tidyText, season = rep(season,nrow(tidyText)), episode = rep(episode,nrow(tidyText)))
    
    allText <- rbind(allText, tidyText)
  }
}
