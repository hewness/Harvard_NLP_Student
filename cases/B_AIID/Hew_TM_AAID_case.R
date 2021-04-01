#' Gordon Hew
#' NBA Case Study
#' Mar30
#'
#' Below Code borrows heavily from scripts found at https://github.com/kwartler/Harvard_NLP_Student
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
library(ggplot2)
library(ggthemes)
setwd("~/Documents/GitHub/Harvard_NLP_Student/cases/B_AIID")
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')
set.seed(1234)
# Bring in our supporting functions
source('~/Documents/GitHub/Harvard_NLP_Student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# In some cases, blank documents and words are created bc of preprocessing.  This will remove them.
blankRemoval<-function(x){
  x <- unlist(strsplit(x,' '))
  x <- subset(x,nchar(x)>0)
  x <- paste(x,collapse=' ')
}

# Each term is assigned to a topic, so this will tally for a document & assign the most frequent as membership
docAssignment<-function(x){
  x <- table(x)
  x <- as.matrix(x)
  x <- t(x)
  x <-max.col(x)
}

#' LDA analysis
#' @param k number of topics
#' @param numIter number of reviews, it performs random word sampling each time
#' @param alpha - there is a distribution of the probabilities of how similar they are to each other, are dice similar in size/shape/weight?
#' @param eta   - there is also a distribution of probabilities for the number of topics inside a single document, are dice 6 sided or other?
lda_analysis <-  function(text, output_dir, k = 5, numIter = 25, alpha = 0.02, eta = 0.02) {
  stops <- c(stopwords('SMART'))
  
  # String clean up
  text$body <- iconv(text$body, "latin1", "ASCII", sub="")
  text$body <- gsub('http\\S+\\s*', '', text$body ) #rm URLs; qdap has rm_url same outcome.
  text$body <- bracketX(text$body , bracket="all") #rm strings in between parenteses, and other brackets
  text$body <- replace_abbreviation(text$body ) # replaces a.m. to AM etc
  
  # Instead of DTM/TDM, just clean the vector w/old functions
  txt <- VCorpus(VectorSource(text$body))
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
  serVis(ldaJSON, out.dir=output_dir, open.browser = FALSE)
}

emotion_by_topic_cluster <- function(data_df, file_name, clusters=5) {
  allInfo <- data.frame(doc_id = 1:nrow(data_df),
                        text   = data_df$body,
                        #source = data_df$source)
                        source = 1)
  # Now the TM
  stops  <- c(stopwords('SMART'),'chars') # API truncation "[+3394 chars]"
  allInfo <- VCorpus(DataframeSource(allInfo))
  allInfo <- cleanCorpus(allInfo, stops)
  allInfoDTM <-  DocumentTermMatrix(allInfo)
  allInfoDTM <- as.matrix(allInfoDTM)
  allInfoDTM <- subset(allInfoDTM, rowSums(allInfoDTM) > 0)
  #dim(allInfoDTM)
  
  #### Perform a Spherical K Means Clustering
  txtSKMeans <- skmeans(allInfoDTM,
                        clusters,
                        m = 1,
                        control = list(nruns = 5, verbose = T))
  
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

#' Returns back a dataframe of incidents for a time period
#'
#' @param year_data_df data frame containing incidents for a given year
#' @param incident_data_df data containing incidents keyed by incident_id
#' @param time_period vector of time periods to filter down
incident_data_by_time_period <- function(year_data_df, incident_data_df, time_period) {
  data_year <- data.frame(year_data_df)
  data_year <- data_year %>% filter(year %in% time_period)
  data_in_year_df <- data.frame(incident_data_df)
  data_in_year_df <- data_in_year_df%>% filter(incident_id %in% data_year$incident_id)
  return(data_in_year_df)
}

# Load Data
incident_dates_df <- read.csv(file = 'incident_dates.csv', header=TRUE)
incident_dates_df$incident_date <- date(mdy(incident_dates_df$Incident.Date))
incident_dates_df$year <- year(incident_dates_df$incident_date)
names(incident_dates_df)[1] <- 'incident_id'
names(incident_dates_df)[4] <- 'report_count'
columns_to_drop <- c('Incident.Date')
incident_dates_df <- incident_dates_df[,!(names(incident_dates_df) %in% columns_to_drop)]
incident_dates_df <- incident_dates_df[, c(1,4,5,3,2)]

incidents_by_year = aggregate(incident_dates_df$incident_id, by=list(Category=incident_dates_df$year), FUN=length)
names(incidents_by_year)[1] <- 'year'
names(incidents_by_year)[2] <- 'incident_count'

reports_by_year = aggregate(incident_dates_df$report_count, by=list(Category=incident_dates_df$year), FUN=sum)
names(reports_by_year)[1] <- 'year'
names(reports_by_year)[2] <- 'total_report_count'
incident_data_by_year <- merge(incidents_by_year, reports_by_year,by='year')
incident_data_by_year$avg_reports_per_incident <- incident_data_by_year$total_report_count / incident_data_by_year$incident_count

# Plot time series of data
par(mfrow=c(1,3))
plot(incident_data_by_year$year, incident_data_by_year$incident_count, type = "b", col = "red", xlab = "Year",
     ylab = "Unique AI Incidents", main="Unique AI Incidents vs Year")
plot(incident_data_by_year$year, incident_data_by_year$avg_reports_per_incident, type = "b", col = "blue", xlab = "year",
     ylab = "Average Peports per AI Incident", main='Average Peports per AI Incident vs Year')
plot(incident_data_by_year$year, incident_data_by_year$total_report_count, type = "b", col = 'green', xlab = "Year",
     ylab = "Total AI Incident Reports", main='Total AI Incident Reports vs Year')

incidents_df <- read.csv(file = 'incidents.csv', header=TRUE)
names(incidents_df)[1] <- 'incident_id'
names(incidents_df)[3] <- 'source'
names(incidents_df)[4] <- 'body'
incidents_df$source <- word(incidents_df$source)
years = sort(unique(incident_dates_df$year))

#DARPA Challenges, DEEP BLUE, Intelligent Agents, STATS, afterwards # Deep Learning, Big Data
period_1 = years[years < 2011]
period_1_data = incident_data_by_time_period(incident_dates_df, incidents_df, period_1)
emotion_by_topic_cluster(period_1_data, 'period_1_topic_sentiment.png')
lda_analysis(period_1_data, 'period_1')

period_2 = years[years >= 2011 & years <= 2015]
period_2_data = incident_data_by_time_period(incident_dates_df, incidents_df, period_2)
emotion_by_topic_cluster(period_2_data, 'period_2_topic_sentiment.png')
lda_analysis(period_2_data, 'period_2')

period_3 = years[years > 2015]
period_3_data = incident_data_by_time_period(incident_dates_df, incidents_df, period_3)
emotion_by_topic_cluster(period_3_data, 'period_3_topic_sentiment.png')
lda_analysis(period_3_data, 'period_3')
