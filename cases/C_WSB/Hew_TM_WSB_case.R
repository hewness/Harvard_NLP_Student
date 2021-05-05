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
library(plyr)
library(dplyr)
library(treemap)
library(skmeans)
library(tidytext)
library(clue)
library(cluster)
library(wordcloud)
library(lexicon)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(tidytext)
library(echarts4r)
library(RColorBrewer)
library(igraph)
library(ggraph)

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
  
  market_data_fig <- market_data_fig %>% layout(title = paste("GME Market Data"),
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
  
  posts_fig <- posts_fig %>% layout(title = paste("Reddit r/wallstreetbets GME Post Data"),
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
plot_word_frequency <- function(tdm, min, i_title, name) {
  topWords <- tidy(tdm)
  topWords <- dplyr::rename(topWords, word = term)

  topWords <- topWords %>%
    dplyr::count(word, sort = TRUE) %>%
    ungroup()
  topWords <- topWords %>% filter(n > min)

  fig_word_count <- plot_ly(x = ~topWords$n, y = ~topWords$word,
                          type = 'bar',
                          orientation = 'h',
                          alpha=0.6,
                          name = name)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "black")

  fig_word_count <- fig_word_count %>% layout(yaxis = list(title = 'Word',
                                                           categoryorder = 'array',
                                                           categoryarray = topWords$n,
                                                           tickmode = 'linear'),
                                              xaxis = list('title' = 'Word Frequency'))

  return(fig_word_count)
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
  text$text <- gsub('http\\S+\\s*', '', text$text) #rm URLs; qdap has rm_url same outcome.
  text$text <- bracketX(text$text , bracket="all") #rm strings in between parenteses, and other brackets
  text$text <- replace_abbreviation(text$text) # replaces a.m. to AM etc
  text$text <- gsub("[^0-9A-Za-z///' ]", "", text$text)
  
  # Instead of DTM/TDM, just clean the vector w/old functions
  txt <- VCorpus(VectorSource(text$text))
  txt <- cleanCorpus(txt, stops)
  
  print(txt)
  
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
  print(head(t(fit$topics))) #words by topics
  
  # LDAvis params
  # normalize the article probabilites to each topic
  theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x))) # topic probabilities within a doc will sum to 1
  
  # normalize each topic word's impact to the topic
  phi  <- t(pbapply(fit$topics + eta, 1, function(x) x/sum(x)))
  
  print(length(phi))
  print(length(theta))
  
  ldaJSON <- createJSON(phi = phi,
                        theta = theta,
                        doc.length = txtDocLength,
                        vocab = txtLex$vocab,
                        term.frequency = as.vector(txtWordCount))
  serVis(ldaJSON, out.dir=output_dir, open.browser = FALSE)
  #serVis(ldaJSON)
}

#' Find bigrams for a text
#' @param text find text
find_bigrams <-  function(text) {
  bigrams_text <- text %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
  
  bigrams_separated <- bigrams_text %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigram_stop_words = c(stopwords('SMART'), 'amp', 'https', 'www.reddit.com', '<NA>',
                        'utm_source', 'utm_medium', 'utm_name', 'png', 'webp',
                        'comments')
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% bigram_stop_words) %>%
    filter(!is.na(word1)) %>%
    filter(!word2 %in% bigram_stop_words) %>%
    filter(!is.na(word2)) 
  
  # new bigram counts:
  bigram_counts <- bigrams_filtered %>% dplyr::count(word1, word2, sort = TRUE)
  
  return(bigram_counts)
}

#' Visualize bigrams
#' @param count_bigrams bigram dataframe
visualize_bigrams <- function(count_bigrams, min=30) {
  bigram_graph <- count_bigrams %>% filter(n > min) %>% graph_from_data_frame()
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) 
}

#' Plot bigram counts
#' @param count_bigrams
plot_bigram_counts <- function(count_bigrams, title, min=50) {
  
  # create a united bigram
  bigrams_united <- count_bigrams %>% unite(bigram, word1, word2, sep = " ")
  
  top_bigrams_united <- bigrams_united %>% filter(n > min)
  
  fig <- plot_ly(x = top_bigrams_united$n,
                 y = top_bigrams_united$bigram, type = 'bar', orientation = 'h',
                 alpha = 0.6)
  
  fig <- fig %>% layout(title=paste(title, "(Word Count >", min, ")"),
                        xaxis = list('title' = 'Count'),
                        yaxis = list(title='Bigram', categoryorder = "array",
                                     categoryarray = top_bigrams_united$n))
  
  return(fig)
}

#' Plot histograms
#' @param posts post data frame
#' @param comments comments data frame
plot_word_count_histograms <- function(posts, comments) {
  fig_post_title_word_count_hist <- plot_ly(posts, x=~title_word_count, type = "histogram", alpha=0.6, name='Post Title Text')
  
  fig_post_word_count_hist <- plot_ly(posts, x=~word_count, type = "histogram", alpha=0.6, name='Post Body Text')
  
  
  fig_comments_word_count_hist <- plot_ly(comments, x=~word_count, type = "histogram", alpha=0.6, name='Comment Text')
  
  fig_word_count_hist <- subplot(fig_post_title_word_count_hist, fig_post_word_count_hist, fig_comments_word_count_hist, titleY = TRUE)
  fig_word_count_hist <- fig_word_count_hist  %>% layout(title='Reddit r/wallstreetbets GME Word Count Histograms',
                                                         xaxis = list('title' = '# of Words'),
                                                         yaxis = list('title' = 'Word Count'))
  return(fig_word_count_hist)
}

#' Plot polarity word counts
#
#' @param tdm text document matrix
#' @param min minimum word count
plot_polarity_word_counts <- function(tdm, title, min = 2) {
  
  tidy_posts <- tidy(tdm)
  tidy_posts <- dplyr::rename(tidy_posts, word = term)
  
  sentiment_word_counts <- tidy_posts %>%
    inner_join(get_sentiments("bing")) %>%
    dplyr::count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  sentiment_count <- tidy_posts %>%
    inner_join(get_sentiments("bing")) %>%
    dplyr::count(sentiment, sort = TRUE) %>%
    ungroup()
  
  postive_sentiment_count <- sentiment_count %>% filter(sentiment == 'positive') 
  negative_sentiment_count <- sentiment_count %>% filter(sentiment == 'negative') 
  
  print(postive_sentiment_count$n[1])
  
  positive_word_counts <- sentiment_word_counts %>% filter(sentiment == 'positive') 
  positive_word_counts <- positive_word_counts %>% filter(n > min)

  negative_word_counts <- sentiment_word_counts %>% filter(sentiment == 'negative')
  sentiment_word_counts <- sentiment_word_counts %>% filter(n > min)
  
  fig_positive <- plot_ly(x = positive_word_counts$n, y = positive_word_counts$word,
                          type = 'bar', orientation = 'h', alpha=0.6, name = 'Positive Words')
  fig_positive <- fig_positive %>% layout(yaxis = list(categoryorder = "array", categoryarray = positive_word_counts$n))
  
  fig_negative <- plot_ly(x = negative_word_counts$n, y = negative_word_counts$word,
                          type = 'bar', orientation = 'h', alpha=0.6, name = 'Negative Words')
  fig_negative <- fig_negative %>% layout(yaxis = list(categoryorder = "array", categoryarray = negative_word_counts$n))
  
  fig_sentiment <- subplot(fig_positive, fig_negative, titleY = TRUE, titleX= TRUE)
  fig_sentiment <- fig_sentiment  %>% layout(title = paste(title, "(Word Count >",min,
                                                           ", Postive Word Count:", postive_sentiment_count$n[1],
                                                           ", Negative Word Count:", negative_sentiment_count$n[1],")"),
                                             xaxis = list('title' = 'Word Count'))

  return(fig_sentiment)
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
posts$word_count <-  lapply(strsplit(posts$post_text, " "), length) 
posts$title_word_count <- lapply(strsplit(posts$title, " "), length) 

# Summarize post data grouped by day
posts_by_day <- posts %>% group_by(post_date) %>% summarise_at(vars(title), funs(n()))
names(posts_by_day)[2] <- 'post_count'
posts_by_day <- posts_by_day[order(posts_by_day$post_date),]

# Extrat comments
comments <- cases_gme_df  %>% select(id, structure, comm_date_ymd, upvote_prop, comment)
names(comments)[3] <- 'comment_date'
comments$word_count <-  lapply(strsplit(comments$comment, " "), length) 

# Summarize comment data grouped by day
comments_by_day = comments %>% group_by(comment_date) %>% summarise_at(vars(id), funs(n()))
names(comments_by_day)[1] <- 'comment_date'
names(comments_by_day)[2] <- 'comment_count'
comments_by_day <- comments_by_day[order(comments_by_day$comment_date),]

# find top 10 most popular posts
top_posts <- posts[order(posts$post_score, decreasing = TRUE),]
top_posts <- top_posts[1:10,]

plot_reddit_data(posts, posts_by_day, comments_by_day)

# Prepare posts and comments into TDMs
stops <- c(stopwords('SMART'))
post_titles <- data.frame('doc_id' = c(1:nrow(posts)), text = posts$title)
posts_title_data <- generate_tdm(post_titles, stops)
posts_title_tdm <- posts_title_data$tdm
plot_word_cloud(posts_title_tdm, 200)

stops <- c(stopwords('SMART'))
post_text <- data.frame('doc_id' = c(1:nrow(posts)), text = posts$post_text)
post_text <- post_text %>% filter(!is.na(text))
post_text_data <- generate_tdm(post_text, stops)
post_text_tdm <- post_text_data$tdm

stops <- c(stopwords('SMART'))
comment_text <- data.frame('doc_id' = c(1:nrow(comments)), text = comments$comment)
comment_text$text <- lapply(comment_text$text, remove_non_ascii_characters)
comment_text <- comment_text %>% filter(!is.na(text))
comment_text_data <- generate_tdm(comment_text, stops)
comment_text_tdm <- comment_text_data$tdm

# Word Clouds
plot_word_cloud(post_text_tdm, 200)
plot_word_cloud(comment_text_tdm, 200)

# Word Frequencies
fig_post_text_word_count <- plot_word_frequency(post_text_tdm, 10, 'Reddit r/wallstreetbets GME Post Word Count (Word Count > 10)', 'Post Body Text')
fig_comment_word_count <- plot_word_frequency(comment_text_tdm, 500, 'Reddit r/wallstreetbets GME Comment Word Count (Word Count > 500)', 'Comment Text')
fig_word_count <- subplot(fig_post_text_word_count, fig_comment_word_count, titleY = TRUE, titleX= TRUE)
fig_word_count <- fig_word_count  %>% layout(title = "Reddit r/wallstreetbets GME Comment Word Count", xaxis = list('title' = 'Word Frequency'))
fig_word_count

# Word Associations
plot_word_association(post_text_tdm, 'gme', 0.80)
plot_word_association(comment_text_tdm, 'gme', 0.15)

# Comments Bigrams
comments_bigrams <- find_bigrams(comment_text)

visualize_bigrams(comments_bigrams)

fig_comments_bigram_counts <- plot_bigram_counts(comments_bigrams, 'Reddit GME r/wallstreetbets Comments Text Bigram Frequencies')
fig_comments_bigram_counts

# Post Bigrams
post_bigrams <- find_bigrams(post_text)

visualize_bigrams(post_bigrams, min=3)

fig_post_bigram_counts <- plot_bigram_counts(post_bigrams, 'Reddit r/wallstreetbets Post Text Bigram Frequencies', min=6)
fig_post_bigram_counts

# Word Count Histograms
fig_word_count_hist <- plot_word_count_histograms(posts, comments)
fig_word_count_hist

# LDA and sentiment analysis
plot_emotion_radar(tidy(comment_text_tdm), 'Reddit r/wallstreetbets GME Emotions')
#emotion_by_topic_cluster(post_text, 'posts_sentiment.png', nruns=1)
lda_analysis(comment_text, 'comment_lda', k = 3)


# polarity distribution
fig_posts_sentiment <- plot_polarity_word_counts(post_text_tdm, 'Reddit r/wsb GME Post Text Word Sentiment Count')
fig_posts_sentiment

fig_comments_sentiment <- plot_polarity_word_counts(comment_text_tdm,
                                                    'Reddit r/wsb GME Comments Word Sentiment Count',
                                                    min = 75)
fig_comments_sentiment