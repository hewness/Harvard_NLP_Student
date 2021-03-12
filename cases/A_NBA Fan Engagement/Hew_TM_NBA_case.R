#' Gordon Hew
#' NBA Case Study
#' Mar04
#' 
#' Below Code borrows heavily from scripts found at https://github.com/kwartler/Harvard_NLP_Student

library(tm)
library(qdap)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(rtweet)
library(mgsub)
library(wordcloud)
library(RColorBrewer)
library(lexicon)
library(tidyr)
library(tidytext)
library(echarts4r)

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Set the working directory
setwd("~/Documents/GitHub/Harvard_NLP_Student/cases/A_NBA Fan Engagement")

#' Load NBA Data
load_data <- function() {
  data_files <- list.files(path = './data', pattern='.csv')
  
  columns_to_drop <- c('created')
  
  dfs <- lapply(data_files, function(f) {
    file_path <- paste('./data/', f, sep='')
    df <- read.csv(file = file_path, header=TRUE)
    df$create_date <- date(ymd_hms(df$created))
    df$year <- year(df$create_date)
    df$quarter <- quarter(df$create_date)
    
    df <- df[,!(names(df) %in% columns_to_drop)]
    
    return(df)
  })
  
  agg_df <- do.call(rbind, dfs)
  return(agg_df)  
}

#' Attempts to lower the case of a value
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

#' Attempts to remove non-ascii characters
remove_non_ascii_characters <- function(x) {
  y = NA
  try_error = tryCatch(gsub("[^\x01-\x7F]", "", x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = gsub("[^\x01-\x7F]", "", x)
  return(y)
}

#' Attempts to convert emojis to their description
convert_emojis <- function(x) {
  y = NA
  try_error = tryCatch(mgsub(x, emojis$code, paste0(' ', emojis$description,' ')), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = mgsub(x, emojis$code, paste0(' ', emojis$description,' '))
  
  return(y)
}

#' Attempts to clean a corpus with a number of transformers and custom functions
cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(remove_non_ascii_characters))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(convert_emojis))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#' Creates a tdm
generate_tdm <- function(df, i_year, i_quarter, i_team, stops) {
  temp_copy <- data.frame(df)
  temp_copy <- temp_copy %>% filter(quarter == i_quarter, year == i_year, team == i_team) 
  temp_copy <- temp_copy[!duplicated(temp_copy$doc_id),]

  print(paste0(nrow(temp_copy), ' Rows found for ', i_year, ' Q', i_quarter, ' ', i_team))
  
  sample_size <- nrow(temp_copy) * 0.05
  
  sample_copy <- sample_n(temp_copy, sample_size)
  
  print(paste0('Sampled ', sample_size, ' (5%) of ', nrow(temp_copy), ' tweets'))
  
  txtCorpus <- VCorpus(DataframeSource(sample_copy))
  
  txtCorpus <- cleanCorpus(txtCorpus, stops)
  
  tweetTDM  <- TermDocumentMatrix(txtCorpus)
  
  return(tweetTDM)
}

#' plot word frequency
plot_word_frequency <- function(tdm, n, i_title) {
  tweet_tdm <- as.matrix(tdm)
  tweetSums <- rowSums(tweet_tdm)
  tweetFreq <- data.frame(word=names(tweetSums), frequency=tweetSums)
  
  topWords <- tweetFreq %>% top_n(n, frequency) %>% arrange(frequency)

  # Chg to factor for ggplot
  topWords$word <- factor(topWords$word, 
                          levels=unique(as.character(topWords$word))) 
  
  p <- ggplot(topWords, aes(x=word, y=frequency)) + 
    geom_bar(stat="identity", fill='orange3') + 
    coord_flip()+ theme_gdocs() +
    geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)  
  
  p + labs(title = paste0(i_title, ' Word Frequency'))
}

#' plot word cloud
plot_word_cloud <- function(tdm, n) {
  tweet_tdm <- as.matrix(tdm)
  tweetSums <- rowSums(tweet_tdm)
  tweetFreq <- data.frame(word=names(tweetSums), frequency=tweetSums)
  
  # Review all Palettes
  #display.brewer.all()
  
  pal <- brewer.pal(8, "Blues")
  pal <- pal[-(1:2)]

  wordcloud(tweetFreq$word,
            tweetFreq$frequency,
            max.words    = n,
            random.order = FALSE,
            colors       = pal,
            scale        = c(2,1))
}

#' plot word association
plot_word_association <- function(tdm, word) {
  associations <- findAssocs(tdm, word, 0.30)
  
  assocDF <- data.frame(terms=names(associations[[1]]),
                        value=unlist(associations))
  assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
  rownames(assocDF) <- NULL
  assocDF
  
  
  # Make a dot plot
  p <- ggplot(assocDF, aes(y=terms)) +
    geom_point(aes(x=value), data=assocDF, col='#c00c00') +
    theme_gdocs() + 
    geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=4) 
  
  p + labs(title = paste0('Word Association with ', word))
}

#' plot emotions radar chart
plot_emotion_radar <- function(tidy_data, title) {
  nrc <- nrc_emotions
  nrc <- nrc %>% pivot_longer(-term, names_to = "emotion", values_to = "freq")
  nrc <-subset(nrc, nrc$freq>0 )
  
  nrcSent <- inner_join(tidy_data,nrc, by=c('term' = 'term'))
  
  print(nrcSent)
  
  print(table(nrcSent$emotion))
  
  emos <- data.frame(table(nrcSent$emotion))
  names(emos) <- c('emotion', 'termsCt')
  emos %>% 
    e_charts(emotion) %>% 
    e_radar(termsCt, max = max(emos$termsCt), name = title) %>%
    e_tooltip(trigger = "item") %>% e_theme("roma")  
}

#' get sentiment
compute_aggregate_sentiment <- function(tidy_data) {
  bing <- get_sentiments(lexicon = c("bing"))
  tidy_data <- inner_join(tidy_data, bing, by=c('term' = 'word'))
  return(aggregate(count~sentiment, tidy_data, sum))
}

nba_data <- load_data()

print(unique(nba_data$team))

head(nba_data)

stops <- c(stopwords('english'), 'knicks', 'york', 'new', 'also', 'amp')

# 2019 Q4
knicks_2019_q4_tweet_tdm <- generate_tdm(nba_data, 2019, 4, 'New York Knicks', stops)

plot_word_frequency(knicks_2019_q4_tweet_tdm, 30, '2019 Q4 New York Knicks Tweets')
plot_word_cloud(knicks_2019_q4_tweet_tdm, 30)
plot_word_association(knicks_2019_q4_tweet_tdm, 'bullocks')

knicks_2019_q4_tweet_tidy <- tidy(knicks_2019_q4_tweet_tdm)
compute_aggregate_sentiment(knicks_2019_q4_tweet_tidy)
plot_emotion_radar(knicks_2019_q4_tweet_tidy, '2019 Q4 New York Knicks Tweet Emotions')

# 2020 Q1
knicks_2020_q1_tweet_tdm <- generate_tdm(nba_data, 2020, 1, 'New York Knicks', stops)
plot_word_frequency(knicks_2020_q1_tweet_tdm, 30, '2020 Q1 New York Knicks Tweets')
plot_word_cloud(knicks_2020_q1_tweet_tdm, 30)
plot_word_association(knicks_2020_q1_tweet_tdm, 'kobe')

knicks_2020_q1_tweet_tidy <- tidy(knicks_2020_q1_tweet_tdm)
compute_aggregate_sentiment(knicks_2020_q1_tweet_tidy)
plot_emotion_radar(knicks_2020_q1_tweet_tidy, '2020 Q1 New York Knicks Tweet Emotions')

# 2020 Q2
knicks_2020_q2_tweet_tdm <- generate_tdm(nba_data, 2020, 2, 'New York Knicks', stops)
plot_word_frequency(knicks_2020_q2_tweet_tdm, 30, '2020 Q2 New York Knicks Tweets')
plot_word_cloud(knicks_2020_q1_tweet_tdm, 30)
plot_word_association(knicks_2020_q1_tweet_tdm, 'sasburneracct')

knicks_2020_q2_tweet_tidy <- tidy(knicks_2020_q2_tweet_tdm)
compute_aggregate_sentiment(knicks_2020_q2_tweet_tidy)
plot_emotion_radar(knicks_2020_q2_tweet_tidy, '2020 Q2 New York Knicks Tweet Emotions')

# 2020 Q3
knicks_2020_q3_tweet_tdm <- generate_tdm(nba_data, 2020, 3, 'New York Knicks', stops)
plot_word_frequency(knicks_2020_q3_tweet_tdm, 30, '2020 Q3 New York Knicks Tweets')
plot_word_cloud(knicks_2020_q3_tweet_tdm, 30)
plot_word_association(knicks_2020_q3_tweet_tdm, 'thibodeau')

knicks_2020_q3_tweet_tidy <- tidy(knicks_2020_q3_tweet_tdm)
compute_aggregate_sentiment(knicks_2020_q3_tweet_tidy)
plot_emotion_radar(knicks_2020_q3_tweet_tidy, '2020 Q3 New York Knicks Tweet Emotions')

# 2020 Q4
knicks_2020_q4_tweet_tdm <- generate_tdm(nba_data, 2020, 4, 'New York Knicks', stops)
plot_word_frequency(knicks_2020_q4_tweet_tdm, 30, '2020 Q4 New York Knicks Tweets')
plot_word_cloud(knicks_2020_q4_tweet_tdm, 30)
plot_word_association(knicks_2020_q4_tweet_tdm, 'fck')

knicks_2020_q4_tweet_tidy <- tidy(knicks_2020_q4_tweet_tdm)
compute_aggregate_sentiment(knicks_2020_q4_tweet_tidy)
plot_emotion_radar(knicks_2020_q4_tweet_tidy, '2020 Q4 New York Knicks Tweet Emotions')


temp_copy2 <- data.frame(nba_data)
temp_copy2 <- temp_copy2 %>% filter(team == 'New York Knicks', year==2020, quarter==4) 
temp_sample_size <- nrow(temp_copy2) * 0.05
temp_copy2 <- sample_n(temp_copy2, temp_sample_size)
temp_copy2$text <- tolower(temp_copy2$text)
#temp_copy2 <- temp_copy2 %>% filter(grepl('fck', text)) 
