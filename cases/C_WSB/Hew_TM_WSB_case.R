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
library(rtweet) # contains emojis
library(ggplot2)
library(ggthemes)
library(tidyr)
library(tidytext)
library(echarts4r)
library(RColorBrewer)

setwd("~/Documents/GitHub/Harvard_NLP_Student/cases/C_WSB")
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
set.seed(1234)


#' Plot price action and trade volume market data
#' 
#' @param market_data_df data frame that contains market data
plot_market_data <- function(market_data_df) {

  # Plot Market Data - candlestick chart + volume
  market_data_fig <- market_data_df %>% plot_ly(x = ~date, type="candlestick",
                        open = ~market_data_df$GME.Open, close = ~market_data_df$GME.Close,
                        high = ~market_data_df$GME.High, low = ~market_data_df$GME.Low, name = 'GME Price Data') 
  market_data_fig <- market_data_fig %>% layout(yaxis = list(title = "Price"))
  
  market_data_fig2 <- market_data_df %>% plot_ly(x=~date, y=~market_data_df$GME.Volume, type='bar', name = "Trading Volume",
                           colors = c('#17BECF','#7F7F7F')) 
  market_data_fig2 <- market_data_fig2 %>% layout(yaxis = list(title = "Volume"))
  
  market_data_fig <- subplot(market_data_fig, market_data_fig2, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE)
  
  market_data_fig <- market_data_fig %>% layout(title = paste("GME Market Data",Sys.Date()),
                        xaxis = list(title='Date'),
                        legend = list(orientation = 'h', x = 0.5, y = 1,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10),
                                      bgcolor = 'transparent'))
  
  market_data_fig
}

#' Plots reddit time series data
#' 
#' @param posts_df dataframe of post data
#' @param posts_by_day_df dataframe of post data grouped by day
#' @param comments_by_day_df dataframe of comments data grouped by day
plot_reddit_data <- function(posts_df, posts_by_day_df, comments_by_day_df) {
  fig1_box_posts <- plot_ly(posts_df, x = ~post_date, y = ~posts_df$post_score, type = "box", name='Post Score') 
  fig1_box_posts <- fig1_box_posts %>% layout(xaxis = list(type = "category", title="Date"), yaxis=list(title="Post Score"))
  
  fig2_posts <- posts_by_day_df %>% plot_ly(x=~post_date, y=~posts_by_day_df$post_count, type='bar', name = "Post Volume") 
  fig2_posts <- fig2_posts %>% layout(yaxis = list(title = "Post Volume"), xaxis = list(type = "category", title="Date"))
  
  comments_by_day_filtered <- comments_by_day_df %>% filter(comment_date %in% posts_by_day_df$post_date)
  fig3_commments <- comments_by_day_filtered %>% plot_ly(x=~comment_date, y=~comments_by_day_filtered$comment_count, type='bar', name = "Comment Volume") 
  fig3_commments <- fig3_commments %>% layout(yaxis = list(title = "Comment Volume"), xaxis = list(type = "category", title="Date"))
  
  posts_fig <- subplot(fig1_box_posts, fig2_posts, fig3_commments, heights = c(0.6,0.2, 0.2), nrows=3,
                       shareX = TRUE, titleY = TRUE)
  
  posts_fig <- posts_fig %>% layout(title = paste("Reddit GME Post Data",Sys.Date()),
                                    xaxis = list(title='Date'),
                                    legend = list(orientation = 'h', x = 0.5, y = 1,
                                                  xanchor = 'center', yref = 'paper',
                                                  font = list(size = 10),
                                                  bgcolor = 'transparent'))
  posts_fig
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

#' Cleans the corpus of characters and words
#' 
#' @param corpus
#' @param customStopwords list of additional stop words to use
cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(remove_non_ascii_characters))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#' Creates a term document matrix
#' 
#' @param df a dataframe with doc_id and text columns
#' @param stops the stop words to use
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

  data <- list("tdm" = tdm_matrix, "corpus" = txtCorpus)
  
  return(data)
}

#' Plot the word cloud
#' 
#' @param tdm a term document matrix
#' @param n the limit on the number of words to display
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

#' Plot word frequency
#' 
#' @param tdm term document matrix
#' @param i_title the title of the chart
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

#' Plot word association
#' 
#' @param tdm a term document matrix
#' @param word the word used to find associations
#' @corlimit the lower correlation limit
plot_word_association <- function(tdm, word, corlimit) {
  associations <- findAssocs(tdm, word, corlimit)
  
  assocDF <- data.frame(terms=names(associations[[1]]),
                        value=unlist(associations))
  assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
  rownames(assocDF) <- NULL

  # Make a dot plot
  p <- ggplot(assocDF, aes(y=terms)) +
    geom_point(aes(x=value), data=assocDF, col='#c00c00') +
    theme_gdocs() + 
    geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=4) 
  
  p + labs(title = paste0('Word Association with ', word))
}

#' Plot emotions on a radar chart
#' 
#' @param tiny_data tdm data in tidy format
#' @param title title of the chart
plot_emotion_radar <- function(tidy_data, title) {
  nrc <- nrc_emotions
  nrc <- nrc %>% pivot_longer(-term, names_to = "emotion", values_to = "freq")
  nrc <-subset(nrc, nrc$freq>0 )
  
  nrcSent <- inner_join(tidy_data,nrc, by=c('term' = 'term'))
  
  emos <- data.frame(table(nrcSent$emotion))
  names(emos) <- c('emotion', 'termsCt')
  emos %>% 
    e_charts(emotion) %>% 
    e_radar(termsCt, max = max(emos$termsCt), name = title) %>%
    e_tooltip(trigger = "item") %>% e_theme("roma")  
}

#' Creates cluster topic plots with emotional sentiment
#' Adapted from https://github.com/kwartler/Harvard_NLP_Student/blob/master/lessons/G_clustering/scripts/F_clustering_sentiment.R
#'
#' @param data_df incident data
#' @param file_name name of the file
#' @param clusters number of clusters to generate
emotion_by_topic_cluster <- function(data_df, file_name, clusters=5, nruns=5) {
  allInfo <- data.frame(doc_id = 1:nrow(data_df),
                        text   = data_df$text,
                        source = 1)
  
  stops  <- c(stopwords('SMART'),'chars') 
  allInfo <- VCorpus(DataframeSource(allInfo))
  allInfo <- cleanCorpus(allInfo, stops)
  allInfoDTM <-  DocumentTermMatrix(allInfo)
  allInfoDTM <- as.matrix(allInfoDTM)
  allInfoDTM <- subset(allInfoDTM, rowSums(allInfoDTM) > 0)
  
  #### Perform a Spherical K Means Clustering
  txtSKMeans <- skmeans(allInfoDTM,
                        clusters,
                        m = 1,
                        control = list(nruns = nruns, verbose = T))
  
  # ID protypical terms
  protoTypical           <- t(cl_prototypes(txtSKMeans))
  colnames(protoTypical) <- paste0('cluster_',1:ncol(protoTypical))
  
  #### Perform an NRC Sentiment Inner Join
  tidyCorp <- tidy(DocumentTermMatrix(allInfo))
  
  # Let's understand the meta data of new source
  (sourceID <- unique(meta(allInfo)))
  breaks = seq(0,100*(length(sourceID[,1])),100)
  tidyCorp <- as.data.frame(tidyCorp)
  tidyCorp$source <- cut(as.numeric(tidyCorp$document),
                         breaks = breaks,
                         labels = sourceID[,1])
  
  # In B_sentimentAnalysis.R we reshaped the NRC now we just load it
  nrc <- get_sentiments(lexicon = c("nrc"))
  
  # Perform the inner join
  nrcSent <- inner_join(tidyCorp, nrc, by=c('term' = 'word'))
  
  # Intersect the Clusters and Sentiment; ID outlier sources
  clusterProp <- table(data.frame(txtSKMeans$cluster,
                                  clusterSource = cut(1:length(txtSKMeans$cluster),
                                                      breaks = breaks,
                                                      labels = sourceID[,1])))
  # What is the proportion of documents for a cluster by it's source
  # cluster1 is mostly brietbart, while cluster 3 is mostly Was post etc
  clusterProp <- prop.table(clusterProp, margin = 1)
  
  # Intersect the Clusters and Sentiment; join the clusters
  docCluster <- data.frame(document = names(txtSKMeans$cluster),
                           clusterAssignment = txtSKMeans$cluster)
  combinedData <- left_join(nrcSent, docCluster)
  oneSource <- aggregate(count~sentiment+clusterAssignment, combinedData, sum)
  # Intersect the Clusters and Sentiment; plot the results, recode the topics 1-4 to the most frequent words like "trump" using which.max
  levelKey                    <- rownames(protoTypical)[apply(protoTypical,2,which.max)]
  names(levelKey)             <- c("1","2","3","4","5")
  oneSource$clusterAssignment <- recode(as.character(oneSource$clusterAssignment), !!!levelKey)
  ggplot(oneSource, aes(sentiment, as.factor(clusterAssignment), size = count, alpha=count)) + theme_calc()+ scale_colour_calc() + geom_point() + ggtitle('Emotion by Topic Cluster') + ylab("")
  ggsave(file_name)
}

# In some cases, blank documents and words are created bc of preprocessing.  This will remove them.
blankRemoval<-function(x){
  x <- unlist(strsplit(x,' '))
  x <- subset(x,nchar(x)>0)
  x <- paste(x,collapse=' ')
}

#' LDA analysis
#' Adapted from https://github.com/kwartler/Harvard_NLP_Student/blob/master/lessons/G_clustering/scripts/A_topicModeling.R
#' 
#' @param k number of topics
#' @param numIter number of reviews, it performs random word sampling each time
#' @param alpha - there is a distribution of the probabilities of how similar they are to each other, are dice similar in size/shape/weight?
#' @param eta   - there is also a distribution of probabilities for the number of topics inside a single document, are dice 6 sided or other?
lda_analysis <-  function(text, output_dir, k = 5, numIter = 25, alpha = 0.02, eta = 0.02) {
  stops <- c(stopwords('SMART'))
  
  # String clean up
  text$text <- iconv(text$text, "latin1", "ASCII", sub="")
  text$text <- gsub('http\\S+\\s*', '', text$text ) #rm URLs; qdap has rm_url same outcome.
  text$text <- bracketX(text$text , bracket="all") #rm strings in between parenteses, and other brackets
  text$text <- replace_abbreviation(text$text) # replaces a.m. to AM etc
  
  # Instead of DTM/TDM, just clean the vector w/old functions
  txt <- VCorpus(VectorSource(text$text))
  txt <- cleanCorpus(txt, stops)
  
  # Extract the clean text
  txt <- unlist(pblapply(txt, content))
  
  # Remove any blanks, happens sometimes w/tweets bc small length & stopwords
  txt <- pblapply(txt, blankRemoval)
  
  # Lexicalize
  txtLex <- lexicalize(txt)
  
  # Corpus stats
  txtWordCount  <- word.counts(txtLex$documents, txtLex$vocab)
  txtDocLength  <- document.lengths(txtLex$documents)
  fit <- lda.collapsed.gibbs.sampler(documents      = txtLex$documents,
                                     K              = k,
                                     vocab          = txtLex$vocab,
                                     num.iterations = numIter,
                                     alpha          = alpha,
                                     eta            = eta,
                                     initial        = NULL,
                                     burnin         = 0,
                                     compute.log.likelihood = TRUE)
  
  # Prototypical Document
  top.topic.documents(fit$document_sums,2) #top 2 docs (rows) * topics(cols)
  
  # explore some of the results
  fit$document_sums #topics by articles
  head(t(fit$topics)) #words by topics
  
  # LDAvis params
  # normalize the article probabilites to each topic
  theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x))) # topic probabilities within a doc will sum to 1
  
  # normalize each topic word's impact to the topic
  phi  <- t(pbapply(fit$topics + eta, 1, function(x) x/sum(x)))
  ldaJSON <- createJSON(phi = phi,
                        theta = theta,
                        doc.length = txtDocLength,
                        vocab = txtLex$vocab,
                        term.frequency = as.vector(txtWordCount))
  #serVis(ldaJSON, out.dir=output_dir, open.browser = FALSE)
  serVis(ldaJSON)
}

################################################################################
#' Main portion of Processing Code
################################################################################

# Load and Plot Market Data
gme_market_data_df <- read.csv(file = 'gme_HLOC.csv', header=TRUE)
plot_market_data(gme_market_data_df)

# Load Reddit Data
cases_gme_df <- readr::read_csv('CASE_gme.csv')
cases_gme_df$post_date_ymd <- date(ymd(cases_gme_df$post_date))  
cases_gme_df$comm_date_ymd <- date(ymd(cases_gme_df$comm_date))

# Extract Post Data
posts <- distinct(cases_gme_df %>% select(post_date_ymd, title, post_text, post_score, upvote_prop, link))
names(posts)[1] <- 'post_date'
posts <- posts[order(posts$post_date),]

# Summarize post data grouped by day
posts_by_day <- posts %>% group_by(post_date) %>% summarise_at(vars(title), funs(n()))
names(posts_by_day)[2] <- 'post_count'
posts_by_day <- posts_by_day[order(posts_by_day$post_date),]

# Extrat comments
comments <- cases_gme_df  %>% select(id, structure, comm_date_ymd, upvote_prop, comment)
names(comments)[3] <- 'comment_date'

# Summarize comment data grouped by day
comments_by_day = comments %>% group_by(comment_date) %>% summarise_at(vars(id), funs(n()))
names(comments_by_day)[1] <- 'comment_date'
names(comments_by_day)[2] <- 'comment_count'
comments_by_day <- comments_by_day[order(comments_by_day$comment_date),]

# find top 10 most popular posts
top_posts <- posts[order(posts$post_score, decreasing = TRUE),]
top_posts <- top_posts[1:10,]

plot_reddit_data(posts, posts_by_day, comments_by_day)

stops <- c(stopwords('english'))
post_titles <- data.frame('doc_id' = c(1:nrow(posts)), text = posts$title)
posts_title_data <- generate_tdm(post_titles, stops)
posts_title_tdm <- posts_title_data$tdm
plot_word_cloud(posts_title_tdm, 200)

stops <- c(stopwords('english'))
post_text <- data.frame('doc_id' = c(1:nrow(posts)), text = posts$post_text)
post_text_data <- generate_tdm(post_text, stops)
post_text_tdm <- post_text_data$tdm
plot_word_cloud(post_text_tdm, 200)

stops <- c(stopwords('english'))
comment_text <- data.frame('doc_id' = c(1:nrow(comments)), text = comments$comment)
comment_text_data <- generate_tdm(comment_text, stops)
comment_text_tdm <- comment_text_data$tdm
plot_word_cloud(comment_text_tdm, 200)


plot_word_frequency(comment_text_tdm, n=30, 'Reddit r/wsb GME Comment')
plot_word_frequency(post_text_tdm, n=30, 'Reddit r/wsb GME Post')

plot_word_association(post_text_tdm, 'gme', 0.80)
plot_word_association(comment_text_tdm, 'gme', 0.15)

plot_emotion_radar(tidy(comment_text_tdm), 'Reddit r/wsb GME Emotions')
emotion_by_topic_cluster(post_text, 'posts_sentiment.png', nruns=1)

lda_analysis(post_text, 'lda')


# austen_bigrams <- austen_books() %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2)


# polarity distribution

# comment word count distribution comments / posts


# TODO: Find out Date Range Range

# TODO: Positivity / Sentiment

# TODO bigrams