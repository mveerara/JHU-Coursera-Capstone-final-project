library(beepr)
library(data.table)
library(tau)
library(plyr)

source('~/makeNgrams.R')
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



