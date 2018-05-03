#library(beepr)
library(data.table)
library(tau)
library(plyr)



# Ngrams has been created in ngram.R"

#-----------------------------------------------------------------------------

# Simple function to predict the next word based on unsmoothed n-gram probabilities
load("./CapstoneprojectData/final/en_US/bigramfreq.RData")
load("./CapstoneprojectData/final/en_US/quadgramfreq.RData")
load("./CapstoneprojectData/final/en_US/Wordsfreq.RData")
load("./CapstoneprojectData/final/en_US/trigramfreq.RData")

allWords2 <- freq1[count >= 2]
save(allWords2, file = "./CapstoneprojectData/final/en_US/allWords2.RData" )
rm(Words)
allBigrams2 <- freq2[count >= 1]
rm(Bigram)
allTrigrams2 <- freq3[count >= 1]
rm(Trigram)
allQuadgrams2 <- freq4[count >= 1]
rm(Quadgram)

# Split ngram into first word(s) and the next word

# 1. Bigrams

bigrams <- do.call(rbind, strsplit(allBigrams2$ngram, split = " "))
bigrams <- cbind(apply(bigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),
                 bigrams[, 2])
allBigrams2$ngram <- bigrams[, 1]
allBigrams2$nextword <- bigrams[, 2]
# Exclude bigrams in which ' is a word
CleanBigram <- grep(pattern = "'", allBigrams2$ngram)
allBigrams2 <- allBigrams2[-CleanBigram, ]
save(allBigrams2, file = "./CapstoneprojectData/final/en_US/allBigrams2.RData" )
rm(bigrams)

 
# 2. Trigrams
trigrams <- do.call(rbind, strsplit(allTrigrams2$ngram, split = " "))
trigrams <- cbind(apply(trigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),
                 trigrams[, 3])
allTrigrams2$ngram <- trigrams[, 1]
allTrigrams2$nextword <- trigrams[, 2]
# Exclude trigrams in which ' is a word
CleanTrigram <- grep(pattern = "'", allTrigrams2$ngram)
allTrigrams2 <- allTrigrams2[-CleanTrigram, ]
save(allTrigrams2, file = "./CapstoneprojectData/final/en_US/allTrigrams2.RData" )
rm(trigrams)

# 3. QuadGrams
quadgrams <- do.call(rbind, strsplit(allQuadgrams2$ngram, split = " "))
quadgrams <- cbind(apply(quadgrams[, 1:3], 1, function(x) paste(x, collapse = " ")),
                  quadgrams[, 4])
allQuadgrams2$ngram <- quadgrams[, 1]
allQuadgrams2$nextword <- quadgrams[, 2]
# Exclude bigrams in which ' is a word
CleanQuadgram <- grep(pattern = "'", allQuadgrams2$ngram)
allQuadgrams2 <- allQuadgrams2[-CleanQuadgram, ]
save(allQuadgrams2, file = "./CapstoneprojectData/final/en_US/allQuadgrams2.RData" )
rm(quadgrams)


setkey(allWords2, "count", "ngram")
setkey(allBigrams2, "count", "ngram")
setkey(allTrigrams2, "count", "ngram")
setkey(allQuadgrams2, "count","ngram")

#rm(allWords2);rm(allBigrams2);rm(allTrigrams2);rm(allQuadgrams2)

predictSimple <- function(input, word,bigram, trigram,quadgram){
  input <- tolower(input)
  splitInput <- unlist(strsplit(input, split = " "))
  if (length(splitInput) > 2){
    input2 <- paste(tail(splitInput, 2), collapse = " ")
  } 
  else {
    input2 <- paste(splitInput, collapse = " ")
  }
  trigramPred <- tail(trigram[ngram == input2]$nextword, 10)
  if (length(trigramPred > 0)){
    print("trigramPred")
    return(trigramPred)
  } 
    else {
    input2 <- tail(splitInput, 1)
    bigramPred <- tail(bigram[ngram == input2]$nextword, 10)
    if (length(bigramPred) > 0){
      print("bigramPred")
      return(bigramPred)
      } 
      else {
      quadgramPred <- tail(quadgram[ngram == input2]$nextword, 10)
      if (length(quadgramPred) > 0){
         print("QuadgramPred")
         return(QuadgramPred)
      }
      else {
        unigramPred <- tail(word[ngram == input2]$nextword, 10)
        if (length(unigramPred) > 0){
          print("unigramPred")
          return(unigramPred)
        }
        else{
        tokenPred <- tail(word$ngram, 20)
        print("tokenPred")
        return(tokenPred)
        }
      }
    }
  }
}


predictSimple("Adam Sandler is the only", allWords2, allBigrams2, allTrigrams2,allQuadgrams2)




