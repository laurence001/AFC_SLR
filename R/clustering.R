library(textmineR)
library(tidyverse)
library(factoextra)

corpus <- as.data.frame(main_corpus)
colnames(corpus) <- c("Code",	"Title","Abstract","Field","Type","Year","Journalism","Use","Citations","APA")

corpus_hc <- corpus %>%
  filter(Use == "Y" | Journalism == "Y")

dtm <- CreateDtm(doc_vec = corpus_hc$Abstract,
                 doc_names = corpus_hc$Code,
                 ngram_window = c(1, 2), 
                 stopword_vec = c(stopwords::stopwords("en"),
                                  stopwords::stopwords(source = "smart")),
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 verbose = FALSE,
                 cpus = 2)


tf_mat <- TermDocFreq(dtm)

# TF-IDF and cosine similarity
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)

hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 10)

plot(hc, main = "Hierarchical clustering of subcorpus1",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 10, border = "red")

#Cluster content

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)

cluster_summary

formattable(cluster_summary)

wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 200, 
                     random.order = TRUE, 
                     colors = c("#2F4858","#B8E6E2"),
                     main = "Top words")


#Source: https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html


#Different ways to visualize (getting insights)

hclust_avg <- hclust(cdist, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 3)

plot(hclust_avg)



rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 3, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)


library(ape)

plot(as.phylo(hc), type = "fan")

plot(as.phylo(hc), type = "cladogram", cex = 0.6,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "steelblue")

colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)


library(dendextend)
library(circlize)

hc <- as.dendrogram(hclust(cdist))
hc <- hc %>%
  color_branches(k = 3) %>%
  set("branches_lwd", 2) %>%  # Line width
  set("branches_lty", 2) %>%  # Line type
  color_labels(k = 3)

# Line styling of the dendrogram
circlize_dendrogram(hc,
                    labels_track_height = NA,
                    dend_track_height = 0.5)  

plot(as.phylo(hc), type = "radial")

#KMEANS CLUSTERING

kmeans6 <- kmeans(DTM, 6)
kmeans6$betweenss
d <- DTM[["dimnames"]][["Terms"]]
kw_with_cluster <- as.data.frame(d, kmeans6$cluster)
names(kw_with_cluster) <- c("Terms", "kmeans6")

str(kmeans6)

fviz_cluster(kmeans6, data = DTM)
