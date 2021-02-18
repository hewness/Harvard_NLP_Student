#' Gordon Hew
#' Extra Credit
#' Feb11
#' 

# Set the working directory
setwd("~/Documents/GitHub/Harvard_NLP_Student/extraCredit_DueFeb18")

# Libs: tm, qdap, ggplot2, ggthemes, ggdendro
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(ggdendro)

# Options & Functions; stringsAsFactors = FALSE; Sys.setlocale('LC_ALL','C')
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Add try to lower custom function
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# add CleanCorpus with removeNumbers, removePunctuation, stripWhitespace, content_transformer(tryTolower), [removeWords, customStopwords], content_transformer(qdapRegex::rm_url), content_transformer(replace_contraction)
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create custom stop words with the SMART lexicon and 'amp','PR','iphone' in an objeect called stops
stops <- c(stopwords('english'), 'amp', 'PR', 'iphone')

# Read in Data, clean & organize with read.csv, VCorpus, VectorSource using text$completeTxt, cleanCorpus with stops, and make a TDM

text      <- read.csv('iphone_PRs.csv', header=TRUE)

names(text)[1] <- 'doc_id' 
names(text)[7] <- 'text' 

txtCorpus <- VCorpus(DataframeSource(text))

txtCorpus <- cleanCorpus(txtCorpus, stops)

txtCorpus[[4]]
meta(txtCorpus[4])
content(txtCorpus[[4]])

textTDM  <- TermDocumentMatrix(txtCorpus)

# Create word associations for "android" with a 0.5 cutoff in an object called associations
associations <- findAssocs(textTDM, 'android', 0.5)

# Examine associations object
associations


# Reduce TDM with removeSparseTerms and a sparse value =0.75 in an object reducedTDM
reducedTDM <- removeSparseTerms(textTDM, sparse=0.75)

# Examine the reducedTDM
reducedTDM

# Change the reducedTDM object with as.matrix inside of as.data.frame
reducedTDMm <- as.matrix(reducedTDM)


# Calculate the reducedTDMdistances in an object called hc
reducedTDMdistances <- dist(reducedTDM)

# Use hclust to get hierarchical clusters for hc, re-declaring the object as hc
hc <- hclust(reducedTDMdistances)

# Plot HC
plot(hc, yaxt='n')

# End

