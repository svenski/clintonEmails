library(data.table)
library(wordcloud)
library(tm)

main <- function() {
  emails <- data.table(read.csv("~/Downloads/output/Emails.csv"))
  persons <- data.table(read.csv("~/Downloads/output/Persons.csv"))
  emails.with.person <- merge(emails, persons[, list(SenderPersonId = Id)], by = "SenderPersonId", all.x = TRUE)

  emails.to.hill <- emails[MetadataTo == "H"]
  emails.from.hill <- emails[MetadataFrom == "H"]

  freq.to.h = wordFrequencyFor(emails.to.hill)
  freq.from.h = wordFrequencyFor(emails.from.hill)

  printWordCloudFor(freq.from.h)
  printWordCloudFor(freq.to.h, min.freq = 100)




  #   in.from.but.not.in.to <- merge(freq.from.h, freq.to.h, by = "word", all.x = TRUE, all.y = TRUE)
  #   all.merge = in.from.but.not.in.to 
  #   setnames(all.merge, c("freq.x", "freq.y"), c("from.clinton", "to.clinton"))
  # 
  #   comparison.cloud(all.merge, max.words = 40, random.order = FALSE)
#   all.merge[, from.h := freq.x/sum(freq.x, na.rm = TRUE)]
  #   all.merge[, from.h := rank(-freq.x)]
  #   all.merge[, to.h := freq.y/sum(freq.y, na.rm = TRUE)]
  #   all.merge[, to.h := rank(freq.y)]
  # 
  #   head(all.merge[order(from.h, decreasing = TRUE)], n = 10)
  #   head(all.merge[order(to.h, decreasing = TRUE)], n = 10)
  # in.from.but.not.in.to[order(freq.x, decreasing = TRUE)]
  # in.from.but.not.in.to[order(freq.y, decreasing = TRUE)]
}
printWordCloudFor <- function(freqs, min.freq = 50) {
 wordcloud(freqs$word, freqs$freq, min.freq = min.freq, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
}

wordFrequencyFor <- function(text) {
  em <- Corpus(VectorSource(text$ExtractedBodyText))
  orig <- copy(em)
  
  em <- tm_map(em, stripWhitespace)
  em <- tm_map(em, content_transformer(tolower))
  em <- tm_map(em, removePunctuation)
  em <- tm_map(em, removeNumbers)
  em <- tm_map(em, stemDocument)
  # em <- tm_map(em, stemCompletion, dictionary = orig)
  my.stop <- c(stopwords("english"), "also", "will", "hrodclintonemailcom")
  em <- tm_map(em, removeWords, my.stop)
  
  dtm <- TermDocumentMatrix(em)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.table(word = names(v),freq=v)
  d
}
