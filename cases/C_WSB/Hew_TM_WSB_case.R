#' Gordon Hew
#' WSB Case Study
#' May07
#' 
#' Below Code borrows heavily from scripts found at https://github.com/kwartler/Harvard_NLP_Student

library(plotly)
library(quantmod)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tm)
library(qdap)
library(pbapply)
library(lda)
library(LDAvis)
library(dplyr)
library(treemap)
library(skmeans)
library(tidytext)
library(clue)
library(cluster)
library(wordcloud)
library(lexicon)
library(plyr)
library(mgsub)
#library(ggplot2)
#library(ggthemes)
#library(RColorBrewer)

setwd("~/Documents/GitHub/Harvard_NLP_Student/cases/C_WSB")
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
set.seed(1234)

# Load Reddit Data
#cases_gme_df <- read.csv(file = 'CASE_gme.csv', header=TRUE, quote = "\"'", encoding="UTF-8", sep=",")
cases_gme_df <- readr::read_csv('CASE_gme.csv')
cases_gme_df$post_date_ymd <- date(ymd(cases_gme_df$post_date))  
cases_gme_df$comm_date_ymd <- date(ymd(cases_gme_df$comm_date))

#TODO: data cleaning

posts <- distinct(cases_gme_df %>% select(post_date_ymd, title, post_text, post_score, upvote_prop, link))
names(posts)[1] <- 'post_date'
posts <- posts[order(posts$post_date),]

posts_by_day <- posts %>% group_by(post_date) %>% summarise_at(vars(title), funs(n()))
names(posts_by_day)[2] <- 'post_count'
posts_by_day <- posts_by_day[order(posts_by_day$post_date),]

comments <- cases_gme_df  %>% select(id, structure, comm_date_ymd, upvote_prop, comment)
names(comments)[3] <- 'comment_date'

comments_by_day = comments %>% group_by(comment_date) %>% summarise_at(vars(id), funs(n()))
names(comments_by_day)[1] <- 'comment_date'
names(comments_by_day)[2] <- 'comment_count'
comments_by_day <- comments_by_day[order(comments_by_day$comment_date),]

fig1_box_posts <- plot_ly(posts, x = ~post_date, y = ~posts$post_score, type = "box", name='Post Score') 
#fig1_box_posts <- fig1_box_posts %>% layout(xaxis = list(title="Date"), yaxis=list(title="Post Score"))
fig1_box_posts <- fig1_box_posts %>% layout(xaxis = list(type = "category", title="Date"), yaxis=list(title="Post Score"))

fig2_posts <- posts_by_day %>% plot_ly(x=~post_date, y=~posts_by_day$post_count, type='bar', name = "Post Volume") 
fig2_posts <- fig2_posts %>% layout(yaxis = list(title = "Post Volume"), xaxis = list(type = "category", title="Date"))
#posts_fig2 <- posts_fig2 %>% layout(yaxis = list(title = "Post Volume"), xaxis = list(title="Date"))

comments_by_day_filtered <- comments_by_day %>% filter(comment_date %in% posts_by_day$post_date)
fig3_commments <- comments_by_day_filtered %>% plot_ly(x=~comment_date, y=~comments_by_day_filtered$comment_count, type='bar', name = "Comment Volume") 
fig3_commments <- fig3_commments %>% layout(yaxis = list(title = "Comment Volume"), xaxis = list(type = "category", title="Date"))
#fig3_commments <- fig3_commments %>% layout(yaxis = list(title = "Comment Volume"), xaxis = list(title="Date"))
fig3_commments

posts_fig <- subplot(fig1_box_posts, fig2_posts, fig3_commments, heights = c(0.6,0.2, 0.2), nrows=3,
                               shareX = TRUE, titleY = TRUE)

posts_fig <- posts_fig %>% layout(title = paste("Reddit GME Post Data",Sys.Date()),
                                                      xaxis = list(title='Date'),
                                                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                                                    xanchor = 'center', yref = 'paper',
                                                                    font = list(size = 10),
                                                                    bgcolor = 'transparent'))
posts_fig




# find top 10 most popular posts
top_posts <- posts[order(posts$post_score, decreasing = TRUE),]
top_posts <- top_posts[1:10,]


pal2 <- brewer.pal(8,"Dark2")
wordcloud(posts$title,
          sources_df$report_count,
          max.words    = 50,
          random.order = FALSE,
          colors=pal2,
          scale=c(4,.5))



# overall word cloud

# word frequency

# bigrams

#Perform Topic analysis on days leading up to the uptick

#Perform LDA analysis on days leading up to the uptick






# Load Market Data
gme_market_data_df <- read.csv(file = 'gme_HLOC.csv', header=TRUE)

# Plot Market Data - candlestick chart + volume
gme_market_data_fig <- gme_market_data_df %>% plot_ly(x = ~date, type="candlestick",
                      open = ~gme_market_data_df$GME.Open, close = ~gme_market_data_df$GME.Close,
                      high = ~gme_market_data_df$GME.High, low = ~gme_market_data_df$GME.Low, name = 'GME Price Data') 
gme_market_data_fig <- gme_market_data_fig %>% layout(yaxis = list(title = "Price"))

gme_market_data_fig2 <- gme_market_data_df 
gme_market_data_fig2 <- gme_market_data_fig2 %>% plot_ly(x=~date, y=~gme_market_data_df$GME.Volume, type='bar', name = "Trading Volume",
                         colors = c('#17BECF','#7F7F7F')) 
gme_market_data_fig2 <- gme_market_data_fig2 %>% layout(yaxis = list(title = "Volume"))

gme_market_data_fig <- subplot(gme_market_data_fig, gme_market_data_fig2, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE)

gme_market_data_fig <- gme_market_data_fig %>% layout(title = paste("GME Market Data",Sys.Date()),
                      xaxis = list(title='Date'),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'))

gme_market_data_fig


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


generate_tdm <- function(df, stops) {
  temp_copy <- data.frame(df)
  #temp_copy <- temp_copy[!duplicated(temp_copy$doc_id),]
  
  #print(paste0(nrow(temp_copy), ' Rows found'))
  
  #sample_size <- nrow(temp_copy) * 0.05
  
  #sample_copy <- sample_n(temp_copy, sample_size)
  
  #print(paste0('Sampled ', sample_size, ' (5%) of ', nrow(temp_copy), ' tweets'))
  
  sample_copy <- temp_copy
  
  txtCorpus <- VCorpus(DataframeSource(sample_copy))
  
  txtCorpus <- cleanCorpus(txtCorpus, stops)
  tdm_matrix  <- TermDocumentMatrix(txtCorpus)

  return(tdm_matrix)
}

plot_word_cloud <- function(tdm, n) {
  tdm <- as.matrix(tdm)
  sums <- rowSums(tdm)
  freq <- data.frame(word=names(sums), frequency=sums)

  pal <- brewer.pal(8, "Blues")
  pal <- pal[-(1:2)]
  
  wordcloud(freq$word,
            freq$frequency,
            max.words    = n,
            random.order = FALSE,
            colors       = pal,
            scale        = c(2,1))
}


stops <- c(stopwords('english'))
post_titles <- data.frame('doc_id' = c(1:nrow(posts)), text = posts$title)
posts_title_tdm <- generate_tdm(post_titles, stops)
plot_word_cloud(posts_title_tdm, 200)

stops <- c(stopwords('english'))
post_text <- data.frame('doc_id' = c(1:nrow(posts)), text = posts$post_text)
post_text_tdm <- generate_tdm(post_text, stops)
plot_word_cloud(post_text_tdm, 200)





# TODO: Find out Date Range Range

# TODO: Find out volume of posts per date

# TODO: Find out volume of comments per date

# TODO: Find out average number of comments per post

# TODO: can i do somethign popularity? - maybe zoom in on an interesting day?

# TODO: assemble each post into separate documents?

# TODO: Find volume of posts that contain GME

# TODO: Positivity / Sentiment