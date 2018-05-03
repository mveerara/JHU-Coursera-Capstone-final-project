library(beepr)
library(data.table)
library(tau)
library(plyr)

source('E:/Mukund back up June 2017/mukund files/MUKUND BACKUP/My learning/Coursera Data scientist/10 - Capstone project - Text mining NLP/Other example capstone project/Thiele/makeNgrams.R')

en_Twitter <- readLines("./CapstoneprojectData/final/en_US/en_US.twitter.txt", encoding = "UTF-8")
en_News <- readLines("./CapstoneprojectData/final/en_US/en_US.news.txt", encoding = "UTF-8")
en_Blogs <- readLines("./CapstoneprojectData/final/en_US/en_US.blogs.txt", encoding = "UTF-8")


# Filter profanity ----------------------------------------------------------
badwords <- readLines("./CapstoneprojectData/final/en_US/profanity-words.txt")
badwords <- c(badwords, "fucking")

en_Twitter <- readLines("./CapstoneprojectData/final/en_US/en_US.twitter.txt", encoding = "UTF-8")
badwordIndexTwitter <- sapply(en_Twitter, function(text){
  any(sapply(X = badwords, function(x) grepl(x, text)))
})
save(badwordIndexTwitter, file = "./CapstoneprojectData/final/en_US/badwordIndexTwitter.RData")
badwordIndexTwitter <- as.logical(badwordIndexTwitter)
rm(en_Twitter)

en_News <- readLines("./CapstoneprojectData/final/en_US/en_US.news.txt", encoding = "UTF-8")
badwordIndexNews <- sapply(en_News, function(text){
  any(sapply(X = badwords, function(x) grepl(x, text)))
})
save(badwordIndexNews, file = "./CapstoneprojectData/final/en_US/badwordIndexNews.RData")
badwordIndexNews <- as.logical(badwordIndexNews)
rm(en_News)

en_Blogs <- readLines("./CapstoneprojectData/final/en_US/en_US.blogs.txt", encoding = "UTF-8")
badwordIndexBlogs <- sapply(en_Blogs, function(text){
  any(sapply(X = badwords, function(x) grepl(x, text)))
})
save(badwordIndexBlogs, file = "./CapstoneprojectData/final/en_US/badwordIndexBlogs.RData")
badwordIndexBlogs <- as.logical(badwordIndexBlogs)
rm(en_Blogs)

# Make n grams and skip n grams --------------------------------------------
# Twitter
load("./CapstoneprojectData/final/en_US/badwordIndexTwitter.RData")
en_Twitter_clean <- en_Twitter[!badwordIndexTwitter]
# seed vergessen aber gespeichert
twitterTrainIndices <- sample(seq_along(en_Twitter_clean),
                              size = round(0.6 * length(en_Twitter_clean)),
                              replace = F)
tokensTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                            ngram = 1,
                            markSentences = F)
tokensTwitter <- tokensTwitter[count > 1]
bigramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                             ngram = 2,
                             markSentences = F)
bigramsTwitter <- bigramsTwitter[count > 1]
trigramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                              ngram = 3,
                              markSentences = F)
save(trigramsTwitter, file = "./CapstoneprojectData/final/en_US/trigramsTwitter_clean.RData")
rm(trigramsTwitter)
gc()
fourgramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                               ngram = 4,
                               markSentences = F)
save(fourgramsTwitter, file = "./CapstoneprojectData/final/en_US/fourgramsTwitter_clean.RData")
rm(fourgramsTwitter, en_Twitter_clean)
gc()

# News
load("badwordIndexNews.RData")
en_News <- readLines("./CapstoneprojectData/final/en_US/en_US.news.txt", encoding = "UTF-8")
en_News_clean <- en_News[!badwordIndexNews]
rm(en_News)
gc()

set.seed(1234)
newsTrainIndices <- sample(seq_along(en_News_clean),
                           size = round(0.6 * length(en_News_clean)),
                           replace = F)
tokensNews <- makeNgrams(en_News_clean[newsTrainIndices],
                         ngram = 1,
                         markSentences = F)
save(tokensNews, file = "./CapstoneprojectData/final/en_US/tokensNews_clean")
rm(tokensNews)
gc()
bigramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                          ngram = 2,
                          markSentences = F)
save(bigramsNews, file = "./CapstoneprojectData/final/en_US/bigramsNews_clean")
rm(bigramsNews)
gc()
trigramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                           ngram = 3,
                           markSentences = F)
save(trigramsNews, file = "./CapstoneprojectData/final/en_US/trigramsNews_clean")
rm(trigramsNews)
gc()
fourgramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                            ngram = 4,
                            markSentences = F)
save(fourgramsNews, file = "./CapstoneprojectData/final/en_US/fourgramsNews_clean")
rm(fourgramsNews, en_News_clean)
gc()


# Blogs
load("./CapstoneprojectData/final/en_US/badwordIndexBlogs.RData")
en_Blogs <- readLines("./CapstoneprojectData/final/en_US/en_US.blogs.txt", encoding = "UTF-8")
en_Blogs_clean <- en_Blogs[!badwordIndexBlogs]
rm(en_Blogs)
gc()

set.seed(1234)
blogsTrainIndices <- sample(seq_along(en_Blogs_clean),
                            size = round(0.6 * length(en_Blogs_clean)),
                            replace = F)
tokensBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                          ngram = 1,
                          markSentences = F)
save(tokensBlogs, file = "./CapstoneprojectData/final/en_US/tokensBlogs_clean")
rm(tokensBlogs)
gc()
bigramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                           ngram = 2,
                           markSentences = F)
save(bigramsBlogs, file = "./CapstoneprojectData/final/en_US/bigramsBlogs_clean")
rm(bigramsBlogs)
gc()
trigramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                            ngram = 3,
                            markSentences = F)
save(trigramsBlogs, file = "./CapstoneprojectData/final/en_US/trigramsBlogs_clean")
rm(trigramsBlogs)
gc()
fourgramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                             ngram = 4,
                             markSentences = F)
save(fourgramsBlogs, file = "./CapstoneprojectData/final/en_US/fourgramsBlogs_clean")
rm(fourgramsBlogs)
gc()


# Skip-n-grams -------------------------------------------------------
# Twitter
load("./CapstoneprojectData/final/en_US/badwordIndexTwitter.RData")
en_Twitter <- readLines("./CapstoneprojectData/final/en_US/en_US.twitter.txt", encoding = "UTF-8")
en_Twitter_clean <- en_Twitter[!badwordIndexTwitter]
rm(en_Twitter)
gc()
load("./CapstoneprojectData/final/en_US/twitterTrainIndices.RData")
skipFiveGramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                                   skip = T,
                                   ngram = 5,
                                   markSentences = F)
save(skipFiveGramsTwitter, file = "./CapstoneprojectData/final/en_US/skipFiveGramsTwitter_clean.RData")
rm(skipFiveGramsTwitter)
gc()
skipSixGramsTwitter <- makeNgrams(en_Twitter_clean[twitterTrainIndices],
                                  skip = T,
                                  ngram = 6,
                                  markSentences = F)
save(skipSixGramsTwitter, file = "./CapstoneprojectData/final/en_US/skipSixGramsTwitter_clean.RData")
rm(skipSixGramsTwitter)
gc()

# News
load("./CapstoneprojectData/final/en_US/badwordIndexNews.RData")
en_News <- readLines("./CapstoneprojectData/final/en_US/en_US.news.txt", encoding = "UTF-8")
en_News_clean <- en_News[!badwordIndexNews]
rm(en_News)
gc()
skipFiveGramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                                skip = T,
                                ngram = 5,
                                markSentences = F)
save(skipFiveGramsNews, file = "./CapstoneprojectData/final/en_US/skipFiveGramsNews_clean.RData")
rm(skipFiveGramsNews)
gc()
skipSixGramsNews <- makeNgrams(en_News_clean[newsTrainIndices],
                               skip = T,
                               ngram = 6,
                               markSentences = F)
save(skipSixGramsNews, file = "./CapstoneprojectData/final/en_US/skipSixGramsNews_clean.RData")
rm(skipSixGramsNews)
gc()

# Blogs
load("./CapstoneprojectData/final/en_US/badwordIndexBlogs.RData")
badwordIndexBlogs <- as.logical(badwordIndexBlogs)
en_Blogs <- readLines("./CapstoneprojectData/final/en_US/en_US.blogs.txt", encoding = "UTF-8")
en_Blogs_clean <- en_Blogs[!badwordIndexBlogs]
rm(en_Blogs)
gc()
set.seed(1234)
blogsTrainIndices <- sample(seq_along(en_Blogs_clean),
                            size = round(0.6 * length(en_Blogs_clean)),
                            replace = F)
skipFiveGramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                                 skip = T,
                                 ngram = 5,
                                 markSentences = F)
save(skipFiveGramsBlogs, file = "./CapstoneprojectData/final/en_US/skipFiveGramsBlogs_clean.RData")
rm(skipFiveGramsBlogs)
gc()
skipSixGramsBlogs <- makeNgrams(en_Blogs_clean[blogsTrainIndices],
                                skip = T,
                                ngram = 6,
                                markSentences = F)
save(skipSixGramsBlogs, file = "./CapstoneprojectData/final/en_US/skipSixGramsBlogs_clean.RData")
rm(skipSixGramsBlogs)
gc()


# Combine n-grams of the different sources ------------------------------------
load("./CapstoneprojectData/final/en_US/tokensBlogs_clean.RData")
load("./CapstoneprojectData/final/en_US/tokensNews_clean.RData")
load("./CapstoneprojectData/final/en_US/tokensTwitter_clean.RData")
allTokens <- rbind.fill(tokensBlogs, tokensNews, tokensTwitter)
allTokens <- data.table(allTokens)
allTokens <- allTokens[, lapply(.SD, sum), by = ngram]
save(allTokens, file = "./CapstoneprojectData/final/en_US/allTokens_clean.RData")
rm(tokensBlogs, tokensTwitter, tokensNews)

load("./CapstoneprojectData/final/en_US/bigramsBlogs_clean.RData")
load("./CapstoneprojectData/final/en_US/bigramsNews_clean.RData")
load("./CapstoneprojectData/final/en_US/bigramsTwitter_clean.RData")
allBigrams <- rbind.fill(bigramsBlogs, bigramsNews, bigramsTwitter)
allBigrams <- data.table(allBigrams)
allBigrams <- allBigrams[, lapply(.SD, sum), by = ngram]
save(allBigrams, file = "./CapstoneprojectData/final/en_US/allBigrams_clean.RData")
rm(bigramsBlogs, bigramsTwitter, bigramsNews)

load("./CapstoneprojectData/final/en_US/trigramsBlogs_clean.RData")
load("./CapstoneprojectData/final/en_US/trigramsNews_clean.RData")
load("./CapstoneprojectData/final/en_US/trigramsTwitter_clean.RData")
allTrigrams <- rbind.fill(trigramsBlogs, trigramsNews, trigramsTwitter)
allTrigrams <- data.table(allTrigrams)
allTrigrams <- allTrigrams[, lapply(.SD, sum), by = ngram]
save(allTrigrams, file = "./CapstoneprojectData/final/en_US/allTrigrams_clean.RData")
rm(trigramsBlogs, trigramsTwitter, trigramsNews)

load("./CapstoneprojectData/final/en_US/fourgramsBlogs_clean.RData")
load("./CapstoneprojectData/final/en_US/fourgramsNews_clean.RData")
load("./CapstoneprojectData/final/en_US/fourgramsTwitter_clean.RData")
allFourgrams <- rbind.fill(fourgramsBlogs, fourgramsNews, fourgramsTwitter)
rm(fourgramsBlogs, fourgramsNews, fourgramsTwitter)
gc()
allFourgrams <- data.table(allFourgrams)
allFourgrams <- allFourgrams[, lapply(.SD, sum), by = ngram]
save(allFourgrams, file = "./CapstoneprojectData/final/en_US/allFourgrams_clean.RData")
rm(fourgramsBlogs, fourgramsTwitter, fourgramsNews)

load("./CapstoneprojectData/final/en_US/skipFiveGramsBlogs_clean.RData")
load("./CapstoneprojectData/final/en_US/skipFiveGramsNews_clean.RData")
load("./CapstoneprojectData/final/en_US/skipFiveGramsTwitter_clean.RData")
allSkipFiveGrams <- rbind.fill(skipFiveGramsBlogs, skipFiveGramsNews,
                               skipFiveGramsTwitter)
rm(skipFiveGramsBlogs, skipFiveGramsNews, skipFiveGramsTwitter)
gc()
allSkipFiveGrams <- data.table(allSkipFiveGrams)
allSkipFiveGrams <- allSkipFiveGrams[, lapply(.SD, sum), by = ngram]
save(allSkipFiveGrams, file = "./CapstoneprojectData/final/en_US/allSkipFiveGrams_clean.RData")

load("./CapstoneprojectData/final/en_US/skipSixGramsBlogs_clean.RData")
load("./CapstoneprojectData/final/en_US/skipSixGramsNews_clean.RData")
load("./CapstoneprojectData/final/en_US/skipSixGramsTwitter_clean.RData")
allSkipSixGrams <- rbind.fill(skipSixGramsBlogs, skipSixGramsNews,
                              skipSixGramsTwitter)
rm(skipSixGramsBlogs, skipSixGramsNews, skipSixGramsTwitter)
gc()
allSkipSixGrams <- data.table(allSkipSixGrams)
allSkipSixGrams <- allSkipSixGrams[, lapply(.SD, sum), by = ngram]
save(allSkipSixGrams, file = "./CapstoneprojectData/final/en_US/allSkipSixGrams_clean.RData")



