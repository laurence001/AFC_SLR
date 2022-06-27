library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(highcharter)
library(tidytext)
library(tidyverse)

abstracts <- as.data.frame(corpus$Abstract)

colnames(abstracts) <- "txt"

stop <- as.data.frame(stop_words)

df <- tibble (txt = abstracts$txt)

text_df <- df %>%
  unnest_tokens(word, txt)

lexicon <- text_df %>%
  anti_join (stop, by ="word")

frequency <- lexicon %>%
  count(word, sort = TRUE) 

library(highcharter)

frequency %>%
  filter(n > 100) %>%
  hchart(
    'bar', hcaes(x = word, y = n)
  )  %>%
  hc_colors("#4e5755") %>%
  hc_xAxis(title = list(text = "Words")) %>% 
  hc_yAxis(title = list(text = "N")) %>%
  hc_title(text = "Word Frequency") %>%
  hc_exporting(
    enabled = TRUE,
    filename = "abstracts-frq"
  )


#Bigrams

dfb <- abstracts

colnames(dfb) <- "word"

bigrams <- dfb %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop$word,
         !word2 %in% stop$word) %>%
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united %>%
  filter(n > 19) %>%
  hchart(
    'bar', hcaes(x = bigram, y = n)
  )  %>%
  hc_colors("#a3334e") %>%
  hc_yAxis(title = list(text = "N")) %>%
  hc_title(text = "Bigrams") %>%
  hc_colors(c('#2f4858',  '#006a7a', '#006a7a', '#cb8891', '#d7a3a9', '#e3bec1', '#eed9d9', '#fff3ef'))



#Create trigrams
trigrams <- dfb %>%
  unnest_tokens(trigram, word, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop$word,
         !word2 %in% stop$word,
         !word3 %in% stop$word) %>%
  count(word1, word2,word3,sort = TRUE)

trigrams_united <- trigrams %>%
  unite(trigram, word1, word2,word3, sep = " ")

trigrams_united %>%
  filter(n > 5) %>%
  hchart(
    'bar', hcaes(x = trigram, y = n)
  )  %>%
  hc_colors("#a3334e") %>%
  hc_xAxis(title = list(text = "Trigrams")) %>% 
  hc_yAxis(title = list(text = "N"))  %>%
  hc_colors(c('#2f4858','#FFCD02',  '#006a7a', '#006a7a', '#cb8891', '#d7a3a9', '#e3bec1', '#eed9d9', '#fff3ef'))

  

#Wordcloud

docs <- Corpus(VectorSource(lexicon))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 10, max.words = 50, random.order=TRUE, colors = c("#2f4858","#185969","#006a7a","#2c8491","#ffcd02","#ffda41"))

#QUANTEDA

library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(readtext)
library(udpipe)

#Create the tokenized corpus
colnames(abstracts) <- "text"
mt <- tokens(abstracts$text, remove_punct = TRUE)
mtb <- dfm(mt)

#Remove stopwords from a .txt file
stop_en <- readLines("stop.txt", encoding = "UTF-8")
mydfm <- dfm_remove(mtb, stop_en)
mydfm <- dfm_trim(mydfm, min_termfreq = 30)

#Frequencies
myfrequencies <- textstat_frequency(mydfm)
myfrequencies

library(formattable)
formattable(myfrequencies)

#Textplot Network
sim <- textstat_proxy(mydfm, margin = "features")
textplot_network(quanteda:::as.fcm(as(sim, "dgTMatrix")), min_freq = 0.97)

#WordCloud
textplot_wordcloud(mydfm)
textplot_wordcloud(mydfm,min_count = 10)
