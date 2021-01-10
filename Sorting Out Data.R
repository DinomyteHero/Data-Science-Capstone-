library(tm)
library(tidytext)
library(slam)
library(ngram)
library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(RWeka)
library(tibble)
library(readr)
library(stringr)
library(ngram)
library(data.table)
library(quanteda)
twitter_data <- readLines("en_US.twitter.txt", skipNul = TRUE)
blog_data <- readLines("en_US.blogs.txt", skipNul = TRUE)
news_data <- readLines("en_US.news.txt", skipNul = TRUE)
# Get Sample Sentences
Smpl <- 0.03
blog_smpl <- sample(seq_len(length(blog_data)),length(blog_data)*Smpl)
news_smpl <- sample(seq_len(length(news_data)),length(news_data)*Smpl)
twitter_smpl <- sample(seq_len(length(twitter_data)),length(twitter_data)*Smpl)

blog_data <- blog_data[blog_smpl[]]
news_data <- news_data[news_smpl[]]
twitter_data <- twitter_data[twitter_smpl[]]

blog_data <- iconv(blog_data, "latin1", "ASCII", sub = "")
news_data <- iconv(news_data, "latin1", "ASCII", sub = "")
twitter_data <- iconv(twitter_data, "latin1", "ASCII", sub = "")
all_data <- c(twitter_data, blog_data, news_data)
# Remove Swear Words
swear_words <- readLines("swearwords.txt")
swear_words <- iconv(swear_words, "latin1", "ASCII", sub = "")
all_data <- removeWords(all_data, swear_words)

# Get Rid of any other variables
# Numbers
all_data <- gsub("[0-9](?:st|nd|rd|th)", "", all_data, ignore.case = FALSE, perl = TRUE)
# Hashtags, URL, Emails, Twitter names
all_data <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", all_data, ignore.case = FALSE, perl = TRUE)
all_data <- gsub("\\S+[@]\\S+", "", all_data, ignore.case = FALSE, perl = TRUE)
all_data <- gsub("@[^\\s]+", "", all_data, ignore.case = FALSE, perl = TRUE)
all_data <- gsub("#[^\\s]+", "", all_data, ignore.case = FALSE, perl = TRUE)

# Change Punctuation

all_data <- gsub("[^\\p{L}'\\s]+", "", all_data, ignore.case = FALSE, perl = TRUE)

all_data <- gsub("[.\\-!]", " ", all_data, ignore.case = FALSE, perl = TRUE)
# Remove the awkward spacing
all_data <- gsub("^\\s+|\\s+$", "", all_data)
all_data <- stripWhitespace(all_data)


corp <- corpus(all_data)

master_Tokens <- tokens(x = tolower(corp))
# Must avoid this
stemed_words <- tokens_wordstem(master_Tokens, language = "english")

bigram <- tokens_ngrams(master_Tokens, n = 2)
trigram <- tokens_ngrams(master_Tokens, n = 3)
quadgram <- tokens_ngrams(master_Tokens, n = 4)

unigram_DFM <- dfm(master_Tokens)
bigram_DFM <- dfm(bigram)
trigram_DFM <- dfm(trigram)
quadgram_DFM <- dfm(quadgram)
rm(bigram, trigram, quadgram)
# Make Data Frames based on frequencies
unigram_freq <- data.frame(freq=colSums(unigram_DFM))
bigram_freq <- data.frame(freq=colSums(bigram_DFM))
trigram_freq <- data.frame(freq=colSums(trigram_DFM))
quadgram_freq <- data.frame(freq=colSums(quadgram_DFM))


unigram_freq$Word <- rownames(unigram_freq)
rownames(unigram_freq) <- NULL

bigram_freq$ngram <- rownames(bigram_freq)
rownames(bigram_freq) <- NULL
bigram_freq$ngram <- gsub("_", " ", bigram_freq$ngram)
bigram_freq$Prev <- gsub("^(\\w+|<s>) .*$", "\\1", bigram_freq$ngram)
bigram_freq$Next <-  gsub("^.* (\\w+|<e>)$", "\\1", bigram_freq$ngram)

trigram_freq$ngram <- rownames(trigram_freq)
rownames(trigram_freq) <- NULL
trigram_freq$ngram <- gsub("_", " ", trigram_freq$ngram)
trigram_freq$Prev <- gsub("^((\\w+\\W+){1}\\w+).*$", "\\1", trigram_freq$ngram)
trigram_freq$Next <-  gsub("^.* (\\w+|<e>)$", "\\1", trigram_freq$ngram)

quadgram_freq$ngram <- rownames(quadgram_freq)
rownames(quadgram_freq) <- NULL
quadgram_freq$ngram <- gsub("_", " ", quadgram_freq$ngram)
quadgram_freq$Prev <- gsub("^((\\w+\\W+){2}\\w+).*$", "\\1", quadgram_freq$ngram)
quadgram_freq$Next <-  gsub("^.* (\\w+|<e>)$", "\\1", quadgram_freq$ngram)
# Generate Quadgram Probabilities
quadgram_prob <- inner_join(quadgram_freq, trigram_freq, by=c("Prev"="ngram"))
quadgram_prob <- quadgram_prob[,1:5]
names(quadgram_prob) <- c("QuadgramFreq", "Quadgram", "Trigram", "Next", "TrigramFreq")
quadgram_prob$MLEProb <- quadgram_prob$QuadgramFreq/quadgram_prob$TrigramFreq
# Generate Bigram Probabilities
bigram_prob <- inner_join(bigram_freq, unigram_freq, by=c("Prev"="Word"))
names(bigram_prob) <- c("BigramFreq", "Bigram", "Prev", "Next", "PrevFreq")
bigram_prob$MLEProb <- bigram_prob$BigramFreq/bigram_prob$PrevFreq
# Generate Unigram Probabilities ( might need to put after bigram prob)
uni_prob <- select(unigram_freq, Word, freq) %>% mutate(MLEProb = freq/sum(unigram_freq$freq))
PrevWordCount <- group_by(bigram_prob, Next) %>% summarize(PrevCount=n()) %>% arrange(desc(PrevCount))
unigram_prob <- left_join(uni_prob, PrevWordCount, by=c("Word"="Next"))
unigram_prob$KNProb <- unigram_prob$PrevCount/nrow(bigram_prob)
names(unigram_prob) <- c( "Next", "freq", "MLEProb", "PrevCount", "KNProb")

# Generate Trigram Probabilities
trigram_prob <- inner_join(trigram_freq, bigram_freq, by=c("Prev"="ngram"))
trigram_prob <- trigram_prob[,1:5]
names(trigram_prob) <- c("TrigramFreq", "Trigram", "Bigram", "Next", "BigramFreq")
trigram_prob$MLEProb <- trigram_prob$TrigramFreq/trigram_prob$BigramFreq

write.csv(quadgram_prob, "./QuadgramProb.csv", quote=FALSE)
write.csv(bigram_prob, "./BigramProb.csv", quote=FALSE)
write.csv(unigram_prob, "./UnigramProb.csv", quote=FALSE)
write.csv(trigram_prob, "./TrigramProb.csv", quote=FALSE)
rm(list = ls())

