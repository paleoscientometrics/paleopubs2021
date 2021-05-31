# Load packages -----------------------------------------------------------
library(wordcloud)
library(tm)
library(dplyr)
library(RColorBrewer)

# Load data ---------------------------------------------------------------
nature <- readxl::read_excel(file.path("data", "nature_all_paleo.xlsx"))
science <- readxl::read_excel(file.path("data", "science_all_paleo.xlsx"))
text <- c(nature$title, science$title)

# Make wordcloud ----------------------------------------------------------

# a. Create a corpus ------------------------------------------------------
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english")) #remove some words e.g. and, is, are

# b. Get frequency of each word -------------------------------------------
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

# c. Plot and save figure -------------------------------------------------
set.seed(1234) # for reproducibility 

svg(file.path("figs", "Fig02_wordcloud.svg"), w=6, h=5)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,           
          max.words=100, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
dev.off()