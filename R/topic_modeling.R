library(tm)
library(topicmodels)
library(lda)
library(tidyverse)
library(formattable)

textdata <- data.frame(doc_id = corpus$Code, text = corpus$Abstract)

stopwe <- readLines("stop.txt", encoding = "UT8")

corpus <- Corpus(DataframeSource(textdata))

text <- stopwe
Encoding(text) <- "UTF-8"
text <- iconv(text, "UTF-8", "UTF-8",sub='')

# Preprocessing chain
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, stopwe)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

f <- content_transformer({function(txt, words, n = 30000L) {
  l <- cumsum(nchar(words)+c(0, rep(1, length(words)-1)))
  groups <- cut(l, breaks = seq(1,ceiling(tail(l, 1)/n)*n+1, by = n))
   regexes <- sapply(split(words, groups), function(words) sprintf("(*UCP)\\b(%s)\\b", paste(sort(words, decreasing = TRUE), collapse = "|")))
 for (regex in regexes)  txt <- gsub(regex, "", txt, perl = TRUE)
   return(txt)
}})
processedCorpus <- tm_map(processedCorpus, f, text) 

#compute document term matrix with terms >= minimumFrequency
minimumFrequency <- 10
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))

dim(DTM)

#remove emty row
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

# number of topics
K <- 10

set.seed(9161)
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))

tmResult <- posterior(topicModel)
attributes(tmResult)
beta <- tmResult$terms
dim(beta)

rowSums(beta) 

nDocs(DTM)  

theta <- tmResult$topics 
dim(theta) 

rowSums(theta)[1:10] 

top_terms <- terms(topicModel, 20)

top_terms <- as.data.frame(top_terms)

formattable(top_terms)

exampleTermData <- terms(topicModel, 5)
exampleTermData[, 1:10]

topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

#CTM

K = 6
topicModelctm <- CTM(DTM, K, method = "VEM")
tmResult <- posterior(topicModelctm)
attributes(tmResult)

beta <- tmResult$terms  
dim(beta)               

rowSums(beta) 

nDocs(DTM)  

theta <- tmResult$topics 
dim(theta) 

rowSums(theta)[1:10] 

top_terms <- terms(topicModelctm, 20)

top_terms <- as.data.frame(top_terms)

formattable(top_terms)
