# Create Ngrams


library(downloader)
library(plyr);
library(dplyr)
library(knitr)
#library(tm)
require(stringi)
library(tidyverse)
library(dtplyr)
library(data.table)
library(ggthemes)
library(wordcloud)
library(ngram)
library(slam)
library(doParallel)
library(quanteda)
library(ggplot2)

#sorurce("~/getSkipNgrams.R")

doParallel::registerDoParallel(cores = 2)
options(mc.cores = 2)


## Step 1: Download the dataset and unzip folder
## Check if directory already exists?
if(!file.exists("./CapstoneprojectData")){
  dir.create("./CapstoneprojectData")
}

DataURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

## Check if zip file has already been downloaded in CapstoneprojectData directory
if(!file.exists("./CapstoneprojectData/Coursera-SwiftKey.zip")){
  download.file(DataURL,destfile="./CapstoneprojectData/Coursera-SwiftKey.zip",mode = "wb")
}

## Check if zip has already been unzipped?
if(!file.exists("./CapstoneprojectData/final")){
  unzip(zipfile="./CapstoneprojectData/Coursera-SwiftKey.zip",exdir="./CapstoneprojectData")
}

## List all the files of /final/en_US Dataset folder
path <- file.path("./CapstoneprojectData/final" , "en_US")
files<-list.files(path, recursive=TRUE)

# pull out individual files
fileBlog <- paste(path,"en_US.blogs.txt",sep="/")
fileNews <- paste(path,"en_US.news.txt",sep="/")
fileTwitter <- paste(path,"en_US.twitter.txt",sep="/")

## Make file connections for all txt files
conBlog <- file(fileBlog)
conNews <- file(fileNews)
conTwitter <- file(fileTwitter)

# Size of individual files
blogs_size   <- file.size(fileBlog) / (2^20)
news_size    <- file.size(fileNews) / (2^20)
twitter_size <- file.size(fileTwitter) / (2^20)


## Read lines into a vector for each file
lineBlog<-readLines(conBlog,encoding = "UTF-8")
lineNews <- readLines(conNews,encoding = "UTF-8")
lineTwitter <- readLines(conTwitter,encoding = "UTF-8")


# Count Number  of Lines per file
blogs_lines   <- length(lineBlog)
news_lines    <- length(lineNews)
twitter_lines <- length(lineTwitter)
total_lines   <- blogs_lines + news_lines + twitter_lines

# Count number of charecters  per line, by file
blogs_nchar   <- nchar(lineBlog)
news_nchar    <- nchar(lineNews)
twitter_nchar <- nchar(lineTwitter)

# Plot number of charecters for each type of file

#boxplot(blogs_nchar, news_nchar, twitter_nchar, log = "y",
      #  names = c("blogs", "news", "twitter"),
       # ylab = "log(Number of Characters)", xlab = "File Name") 
#title("Comparing Distributions of Chracters per Line")


# Total characters per file
blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

# Total words per file
blogs_words <- wordcount(lineBlog, sep = " ")
news_words  <- wordcount(lineNews,  sep = " ")
twitter_words <- wordcount(lineTwitter, sep = " ")

# Create a summary of all counts collected so far in a data drame and add percentages
repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))

repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
# To be used while creating R markdwon Report
kable(repo_summary)


# Sample the data and save the sample
# Compute sample sizes in terms of lines
Train_sample_pct = 0.020
Test_sample_pct = 0.080
set.seed(1001)

Train_blogs_size   <- blogs_lines * Train_sample_pct
Train_news_size    <- news_lines * Train_sample_pct
Train_twitter_size <- twitter_lines * Train_sample_pct

Test_blogs_size   <- blogs_lines * Test_sample_pct
Test_news_size    <- news_lines * Test_sample_pct
Test_twitter_size <- twitter_lines * Test_sample_pct


# Create  Random samples
Train_blogs_sample   <- sample(lineBlog, Train_blogs_size)
Train_news_sample    <- sample(lineNews, Train_news_size)
Train_twitter_sample <- sample(lineTwitter, Train_twitter_size)

Test_blogs_sample   <- sample(lineBlog, Test_blogs_size)
Test_news_sample    <- sample(lineNews, Test_news_size)
Test_twitter_sample <- sample(lineTwitter, Test_twitter_size)

## partial Cleaning
## Remove punctuations; keep apostophes; keep dash and underscore
Train_twitter_sample <- gsub("[^[:alnum:]['-_]", " ", Train_twitter_sample)
Train_twitter_sample <- gsub("[-.?!@:;)({}[]/#,^.]+", " ", Train_twitter_sample)
## Remove contigous leters or numbers like aaa bbb 77 999 etc.
Train_twitter_sample <- gsub("+([a-zA-z]+) +\1 +", " ",Train_twitter_sample)

Train_blogs_sample <- gsub("[^[:alnum:]['-_]", " ", Train_blogs_sample)
Train_blogs_sample <- gsub("[-.?!@:;)({}[]/#,^.]+", " ", Train_blogs_sample)
Train_blogs_sample <- gsub("+([a-zA-z]+) +\1 +", " ",Train_blogs_sample)

Train_news_sample <- gsub("[^[:alnum:]['-_]", " ", Train_news_sample)
Train_news_sample <- gsub("[-.?!@;)({}[]:/#,^.]+", " ", Train_news_sample)
#Train_news_sample <- gsub("[[:punct:][^']", " ", Train_news_sample, perl=T)
Train_news_smaple <- gsub("+([a-zA-z]+) +\1 +", " ",Train_news_sample)

gsub("[-.?!@/#,^.]+", " ", gsub("(?!@/#[-.?!@/#])[[:punct:] ]+", " ", Test_twitter_sample, perl=T))
gsub("[-.?!@/#,^.]+", " ", gsub("(?!@/#[-.?!@/#])[[:punct:] ]+", " ", Test_blogs_sample, perl=T))
gsub("[-.?!@/#,^.]+", " ", gsub("(?!@/#[-.?!@/#])[[:punct:] ]+", " ", Test_news_sample, perl=T))


Train_repo_sample    <- c(Train_blogs_sample, Train_news_sample, Train_twitter_sample)
Test_repo_sample    <- c(Test_blogs_sample, Test_news_sample, Test_twitter_sample)

#' Save sample
writeLines(Train_repo_sample, "./CapstoneprojectData/final/en_US/Train.en_US.repo_sample.txt")
save(Train_repo_sample, file = "./CapstoneprojectData/final/en_US/Train_repo_sample.RData" )

writeLines(Test_repo_sample, "./CapstoneprojectData/final/en_US/Test.en_US.repo_sample.txt")
save(Test_repo_sample, file = "./CapstoneprojectData/final/en_US/Test_repo_sample.RData" )

#' ## Clean the Train sample data
#'
#' Use `quanteda` to create the corpus
clean_sample <- corpus(Train_repo_sample)
#clean_sample <- tm_map(clean_sample, remove.noneng)
print(as.character(clean_sample[[1]]))
#' Save clean corpus  
save(clean_sample, file = "./CapstoneprojectData/final/en_US/clean_sample.RData" )

# Generic function for parallelizing any task (when possible)
parallelizeTask <- function(task, ...) {
  # Calculate the number of cores
  ncores <- detectCores() - 1
  # Initiate cluster
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  print("Starting task")
  r <- task(...)
  print("Task done")
  stopCluster(cl)
  r
}

# Returns a vector of profanity words
getProfanityWords <- function(corpus){
  profanityFileName <- "./CapstoneprojectData/final/en_US/profanity-words.txt"
  if (!file.exists(profanityFileName)) {
    profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    download.file(profanity.url, destfile = profanityFileName, mode = "wb")
  }
  if (sum(ls() == "profanity") < 1) {
    profanity <- read.csv(profanityFileName, header = FALSE, stringsAsFactors = FALSE)
    profanity <- profanity$V1
    profanity <- profanity[1:length(profanity)-1]
  }
  profanity
}

# Break the Corpus and make sentences
makeSentences <- function(input) {
  output <- quanteda::tokens(input, what = "word", remove_numbers = TRUE,
                     remove_punct = TRUE, remove_separators = TRUE,remove_symbols=TRUE,
                     remove_twitter = FALSE, remove_hyphens = TRUE,remove_url = TRUE,skip = 0L,concatenator = " ")
  #output <- tokens_remove(output,stopwords("english"))
  output <- tokens_remove(output, getProfanityWords())
  #unlist(lapply(output, function(a) paste('#s#', tolower(a), '#e#')))
}

# Make Tokens given vectors into N-gram
makeTokens <- function(input, n = 1L) {
  quanteda::tokens(input, what = "word", remove_numbers = TRUE,
           remove_punct = TRUE, remove_separators = TRUE, remove_symbols = TRUE,
           remove_twitter = TRUE, remove_hyphens = TRUE,remove_url = TRUE,concatenator = " ",
           ngrams = n, simplify = TRUE)
  #tokens_remove(toks,stopwords("english"))
}
startFunction <- Sys.time()
sentences <- parallelizeTask(makeSentences, clean_sample)

ngram1 <- parallelizeTask(makeTokens, sentences, 1)
ngram2 <- parallelizeTask(makeTokens, sentences, 2)
ngram3 <- parallelizeTask(makeTokens, sentences, 3)
ngram4 <- parallelizeTask(makeTokens, sentences, 4)
ngram5 <- parallelizeTask(makeTokens, sentences, 5)
ngram6 <- parallelizeTask(makeTokens, sentences, 6)

# Create Document feature matrix for each N grams and remove single letter, two letters and three letter words
dfm1 <- parallelizeTask(dfm, ngram1)
dfm_select(dfm1, "\\b[a-zA-Z0-9]{1,3}\\b", selection = "remove", valuetype = "regex")
dfm2 <- parallelizeTask(dfm, ngram2)
dfm_select(dfm2, "\\b[a-zA-Z0-9]{1,3}\\b", selection = "remove", valuetype = "regex")
dfm3 <- parallelizeTask(dfm, ngram3)
dfm_select(dfm3, "\\b[a-zA-Z0-9]{1,3}\\b", selection = "remove", valuetype = "regex")
dfm4 <- parallelizeTask(dfm, ngram4)
dfm_select(dfm4, "\\b[a-zA-Z0-9]{1,3}\\b", selection = "remove", valuetype = "regex")
dfm5 <- parallelizeTask(dfm, ngram5)
dfm_select(dfm5, "\\b[a-zA-Z0-9]{1,3}\\b", selection = "remove", valuetype = "regex")
dfm6 <- parallelizeTask(dfm, ngram6)
dfm_select(dfm6, "\\b[a-zA-Z0-9]{1,3}\\b", selection = "remove", valuetype = "regex")

# Store DFM in Data table, with feature and Sum of count for each each feature in DFM. Store this as frequency data
dt1 <- data.table(ngram = dfm1@Dimnames$features, count = colSums(dfm1), key = "ngram")
save(dt1, file="./CapstoneprojectData/final/en_US/wordsfreq.RData")
dt2 <- data.table(ngram = dfm2@Dimnames$features, count = colSums(dfm2), key = "ngram")
save(dt2, file="./CapstoneprojectData/final/en_US/bigramfreq.RData")
dt3 <- data.table(ngram = dfm3@Dimnames$features, count = colSums(dfm3), key = "ngram")
save(dt3, file="./CapstoneprojectData/final/en_US/trigramfreq.RData")
dt4 <- data.table(ngram = dfm4@Dimnames$features, count = colSums(dfm4), key = "ngram")
save(dt4, file="./CapstoneprojectData/final/en_US/quadgramfreq.RData")
dt5 <- data.table(ngram = dfm5@Dimnames$features, count = colSums(dfm5), key = "ngram")
save(dt5, file="./CapstoneprojectData/final/en_US/allSkipFiveGrams_clean.RData")
dt6 <- data.table(ngram = dfm6@Dimnames$features, count = colSums(dfm6), key = "ngram")
save(dt6, file="./CapstoneprojectData/final/en_US/allSkipSixGrams_clean.RData")

# Prepare n-gram frequencies order descending  and write to a file
#getFreq <- function(x){
freq1 <- dt1[order(-dt1$count),]
save(freq1, file = "./CapstoneprojectData/final/en_US/Wordsfreq.RData" )
freq2 <- dt2[order(-dt2$count),]
save(freq2, file = "./CapstoneprojectData/final/en_US/bigramfreq.RData" )
freq3 <- dt3[order(-dt3$count),]
save(freq3, file = "./CapstoneprojectData/final/en_US/trigramfreq.RData" )
freq4 <- dt4[order(-dt4$count),]
save(freq4, file = "./CapstoneprojectData/final/en_US/quadgramfreq.RData" )
freq5 <- dt5[order(-dt5$count),]
save(freq5, file = "./CapstoneprojectData/final/en_US/allSkipFiveGrams_clean.RData" )
freq6 <- dt6[order(-dt6$count),]
save(freq6, file = "./CapstoneprojectData/final/en_US/allSkipSixGrams_clean.RData" )

#rm(dt1)
#rm(dt2)
#rm(dt3)
#rm(dt4)


## "plotting ngrams frequency


g1 <- ggplot(data=freq1[1:10,], aes(x = ngram, y = count))
g2 <- g1 + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequent Words")
g3 <- g2 + geom_text(data = freq1[1:10,], aes(x = freq1[1:10]$ngram, y = freq1[1:10]$count, label = freq1[1:10]$count), hjust=-1, position = "identity")
print(g3)

#bigram <- readRDS("./CapstoneprojectData/final/en_US/bigramfreq.RData")
g1 <- ggplot(data=freq2[1:10,], aes(x = ngram, y = count))
g2 <- g1 + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequent bigrams")
g3 <- g2 + geom_text(data = freq2[1:10,], aes(x = freq2[1:10]$ngram, y = freq2[1:10]$count, label = freq2[1:10]$count), hjust=-1, position = "identity")
print(g3)

#trigram <- readRDS("./CapstoneprojectData/final/en_US/trigramfreq.RData")
g1 <- ggplot(data=freq3[1:10,], aes(x = ngram, y = count))
g2 <- g1 + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequent trigrams")
g3 <- g2 + geom_text(data = freq3[1:10,], aes(x = freq3[1:10]$ngram, y = freq3[1:10]$count, label = freq3[1:10]$count), hjust=-1, position = "identity")
print(g3)

#quadgram <- readRDS("./CapstoneprojectData/final/en_US/quadgramfreq.RData")
g1 <- ggplot(data=freq4[1:10,], aes(x = ngram, y = count))
g2 <- g1 + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequent trigrams")
g3 <- g2 + geom_text(data = freq4[1:10,], aes(x = freq4[1:10]$ngram, y = freq4[1:10]$count, label = freq4[1:10]$count), hjust=-1, position = "identity")
print(g3)



#' Create ngram cloud
wordcloud(freq1$ngram, freq1$count, max.words = 100,
       colors = brewer.pal(6, 'Dark2'), random.color = FALSE,random.order = FALSE)  
#dev.new(width=6, height=10)
wordcloud(freq2$ngram, freq2$count, max.words =  100,
     colors = brewer.pal(6, 'Dark2'), random.color = FALSE, random.order = FALSE)

wordcloud(freq3$ngram, freq3$count, max.words = 100,
       colors = brewer.pal(6, 'Dark2'), random.order = FALSE)  

wordcloud(freq4$ngram, freq4$count, max.words = 100,
      colors = brewer.pal(6, 'Dark2'), random.order = FALSE)
message("Finished after:")
Elapsedtime <- Sys.time() - startFunction
print(Elapsedtime)


# Close the connection handle when you are done
close(conBlog)
close(conTwitter)
close(conNews)
