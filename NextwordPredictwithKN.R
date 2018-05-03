#----------------------------------------------------------------------------
# Function to predict the next word based on Kneser-Ney n-gram probabilities
# or simple counts using a backoff model. The Kneser-Ney model backs off to
# token probabilities if the n-gram is not known

#library(beepr)
library(data.table)
#library(tau)
library(plyr)

# Save all counts in probabilities in one list of data.tables
# English
load("./CapstoneprojectData/final/en_US/allWords2.RData")
load("./CapstoneprojectData/final/en_US/allBigrams2.RData")
load("./CapstoneprojectData/final/en_US/allTrigrams2.RData")
load("./CapstoneprojectData/final/en_US/allQuadgrams2.RData")
##load("allTokens_clean.RData")
##allWords_en <- allWords2[count > 3]
##allBigrams_en <- allBigrams2[count > 3]
##allTrigrams_en <- allTrigrams2[count > 3]
##allFourrams_en <-  allQuadgrams2[count > 2]
##rm(allTokens, allBigrams, allTrigrams)
##load("allFourgrams_clean_pruned.RData")
##allFourgrams_en <- allFourgrams[count > 2]
##rm(allFourgrams)

# Split ngram into first word(s) and the next word
# for the data.tables of simple counts
##fourgrams <- do.call(rbind, strsplit(allFourgrams_en$ngram, split = " "))
##fourgrams <- cbind(apply(fourgrams[, 1:3], 1, function(x) paste(x, collapse = " ")),
                 ##  fourgrams[, 4])
##allFourgrams_en$ngram <- fourgrams[, 1]
##allFourgrams_en$nextword <- fourgrams[, 2]
# Exclude fourgrams in which ' is a word
delete <- grep(pattern = "^'+", allFourgrams_en$ngram)
allFourgrams_en <- allFourgrams_en[-delete, ]
delete <- grep(pattern = "^'+", allFourgrams_en$nextword)
allFourgrams_en <- allFourgrams_en[-delete, ]

##trigrams <- do.call(rbind, strsplit(allTrigrams_en$ngram, split = " "))
##trigrams <- cbind(apply(trigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),
       ##           trigrams[, 3])
##allTrigrams_en$ngram <- trigrams[, 1]
##allTrigrams_en$nextword <- trigrams[, 2]
# Exclude trigrams in which ' is a word
delete <- grep(pattern = "^'+", allTrigrams_en$ngram)
allTrigrams_en <- allTrigrams_en[-delete, ]
delete <- grep(pattern = "^'+", allTrigrams_en$nextword)
allTrigrams_en <- allTrigrams_en[-delete, ]

##bigrams <- do.call(rbind, strsplit(allBigrams_en$ngram, split = " "))
##allBigrams_en$ngram <- bigrams[, 1]
##allBigrams_en$nextword <- bigrams[, 2]
# Exclude bigrams in which ' is a word
delete <- grep(pattern = "^'+", allBigrams_en$ngram)
allBigrams_en <- allBigrams_en[-delete, ]
delete <- grep(pattern = "^'+", allBigrams_en$nextword)
allBigrams_en <- allBigrams_en[-delete, ]

# KN smoothed english
load("allFourgramsKN.RData")
load("allTrigramsKN.RData")
allTrigramsKN_en <- allTrigramsKN[, .(ngram, nextword, count)]
allFourgramsKN_en <- allFourgramsKN[, .(ngram, nextword, count)]
rm(allTrigramsKN, allFourgramsKN)
load("allBigramsKN.RData")
allBigramsKN_en <- allBigramsKN[, .(ngram, nextword, count)]
rm(allBigramsKN)
# Exclude n-grams that start with '
delete <- grep(pattern = "^'+", allBigramsKN_en$ngram)
delete <- c(delete, grep(pattern = "^'+", allBigramsKN_en$nextword))
allBigramsKN_en <- allBigramsKN_en[-delete, ]
delete <- grep(pattern = "^'+", allTrigramsKN_en$ngram)
delete <- c(delete, grep(pattern = "^'+", allTrigramsKN_en$nextword))
allTrigramsKN_en <- allTrigramsKN_en[-delete, ]
delete <- grep(pattern = "^'+", allFourgramsKN_en$ngram)
delete <- c(delete, grep(pattern = "^'+", allFourgramsKN_en$nextword))
allFourgramsKN_en <- allFourgramsKN_en[-delete, ]

rm(bigrams, trigrams, fourgrams)
setkey(allTokens_en, "count", "ngram")
setkey(allBigrams_en, "count", "ngram")
setkey(allTrigrams_en, "count", "ngram")
setkey(allFourgrams_en, "count", "ngram")
setkey(allBigramsKN_en, "count", "ngram")
setkey(allTrigramsKN_en, "count", "ngram")
setkey(allFourgramsKN_en, "count", "ngram")



rm(allTokens, allBigrams, allTrigrams, allFourgrams)


### Skip-n-grams German and English
load("allSkipFiveGrams_clean.RData")
bigrams <- do.call(rbind, strsplit(allSkipFiveGrams$ngram, split = " "))
allSkipFiveGrams$ngram <- bigrams[, 1]
allSkipFiveGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipFiveGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipFiveGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipFiveGrams$nextword))
allSkipFiveGrams_en <- allSkipFiveGrams[-unique(delete), ]
load("allSkipSixGrams_clean.RData")
bigrams <- do.call(rbind, strsplit(allSkipSixGrams$ngram, split = " "))
allSkipSixGrams$ngram <- bigrams[, 1]
allSkipSixGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipSixGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipSixGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipSixGrams$nextword))
allSkipSixGrams_en <- allSkipSixGrams[-unique(delete), ]
load("allSkipFiveGrams_clean_de.RData")
bigrams <- do.call(rbind, strsplit(allSkipFiveGrams$ngram, split = " "))
allSkipFiveGrams$ngram <- bigrams[, 1]
allSkipFiveGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipFiveGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipFiveGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipFiveGrams$nextword))
allSkipFiveGrams_de <- allSkipFiveGrams[-unique(delete), ]
load("allSkipSixGrams_clean_de.RData")
bigrams <- do.call(rbind, strsplit(allSkipSixGrams$ngram, split = " "))
allSkipSixGrams$ngram <- bigrams[, 1]
allSkipSixGrams$nextword <- bigrams[, 2]
delete <- grep(pattern = "^'+", allSkipSixGrams$ngram)
delete <- c(delete, grep(pattern = "^'+", allSkipSixGrams$nextword))
delete <- c(delete, grep(pattern = "<", allSkipSixGrams$nextword))
allSkipSixGrams_de <- allSkipSixGrams[-unique(delete), ]
rm(allSkipFiveGrams, allSkipSixGrams)

# Rename Token data
setnames(allTokens_de, c("nextword", "count"))
setnames(allTokens_en, c("nextword", "count"))

allData <- list(allBigrams_de, allBigrams_en, allBigramsKN_de,
                allBigramsKN_en, allFourgrams_de, allFourgrams_en,
                allFourgramsKN_de, allFourgramsKN_en, allTokens_de,
                allTokens_en, allTrigrams_de, allTrigrams_en,
                allTrigramsKN_de, allTrigramsKN_en,
                allSkipFiveGrams_de, allSkipFiveGrams_en,
                allSkipSixGrams_de, allSkipSixGrams_en)

names(allData) <- c("allBigrams_de", "allBigrams_en", "allBigramsKN_de",
                    "allBigramsKN_en", "allFourgrams_de", "allFourgrams_en",
                    "allFourgramsKN_de", "allFourgramsKN_en", "allTokens_de",
                    "allTokens_en", "allTrigrams_de", "allTrigrams_en",
                    "allTrigramsKN_de", "allTrigramsKN_en",
                    "allSkipFiveGrams_de", "allSkipFiveGrams_en",
                    "allSkipSixGrams_de", "allSkipSixGrams_en")

save(allData, file = "allData.RData")

#----------------------------------------------------------------------------
load("allData.RData")
load("Nextword//data//allData2.RData")
# Further pruning and cleaning (after testing and benchmarking)
allData2 <- allData
allData2 <- lapply(allData2, function(x){
  x[nextword != "<"]
})
# Drop 50% of all n-grams in all tables
allData2 <- lapply(allData2, function(x){
  len <- nrow(x)
  return(x[round(len / 2) : len, ])
})
# allData2 = 330 MB

# Drop all German nextwords that consist of more than one upper case letter
germanIndices <- grep("_de", names(allData2))
allData2[germanIndices] <- lapply(allData2[germanIndices], function(x){
  if(length(x$nextword) <= 0) warning("nextwords not found")
  delete <- grep("[A-Z]{2,}", x$nextword)
  return(x[-delete, ])
})
# about 1MB less

# Quantiles of the counts, for confidence score
minmax <- lapply(allData2, function(x){
  data.table(mininum = min(x$count),
             maximum = max(x$count))
})

predictNextword_en <- function(input = NULL, ngramData, useKN = T, nPred = 3){
  if (is.null(input)) return(NULL)
  confidence <- NA
  input <- tolower(input)
  splitInput <- unlist(strsplit(input, split = " "))
  # Restrict to trigram input
  if (length(splitInput) > 2){
    input2 <- paste(tail(splitInput, 3), collapse = " ")
  } else {
    input2 <- paste(splitInput, collapse = " ")
  }
  if (useKN){
    fourgramPred <- rev(tail(ngramData$allFourgramsKN_en[ngram == input2]$nextword,
                             nPred))
    if(length(fourgramPred) > 0 & is.na(confidence)){
      count <- tail(ngramData$allFourgramsKN_en[ngram == input2]$count, 1)
      confidence <- quantile((count - minmax$allFourgramsKN_en$mininum) /
                               (minmax$allFourgramsKN_en$maximum -
                                  minmax$allFourgramsKN_en$mininum) , 1)
    }
  } else {
    fourgramPred <- rev(tail(ngramData$allFourgrams_en[ngram == input2]$nextword,
                             nPred))
    if(length(fourgramPred) > 0 & is.na(confidence)){
      count <- tail(ngramData$allFourgrams_en[ngram == input2]$count, 1)
      confidence <- quantile((count - minmax$allFourgrams_en$mininum) /
                               (minmax$allFourgrams_en$maximum -
                                  minmax$allFourgrams_en$mininum) , 1)
    }
  }
  if (length(fourgramPred) >= nPred){
    return(data.table(pred = fourgramPred, confidence = confidence))
  } else {
    predComb <- fourgramPred
    # Input is a bigram or smaller or no fourgrampred was found
    if (length(splitInput) >= 2){
      input2 <- paste(tail(splitInput, 2), collapse = " ")
    } else {
      input2 <- paste(splitInput, collapse = " ")
    }
    if (useKN){
      trigramPred <- rev(ngramData$allTrigramsKN_en[ngram == input2]$nextword)
      if(length(trigramPred) > 0 & is.na(confidence)){
        count <- tail(ngramData$allTrigramsKN_en[ngram == input2]$count, 1)
        confidence <- quantile((count - minmax$allTrigramsKN_en$mininum) /
                                 (minmax$allTrigramsKN_en$maximum -
                                    minmax$allTrigramsKN_en$mininum) , 1)
      }
    } else {
      trigramPred <- rev(ngramData$allTrigrams_en[ngram == input2]$nextword)
      if(length(trigramPred) > 0 & is.na(confidence)){
        count <- tail(ngramData$allTrigrams_en[ngram == input2]$count, 1)
        confidence <- quantile((count - minmax$allTrigrams_en$mininum) /
                                 (minmax$allTrigrams_en$maximum -
                                    minmax$allTrigrams_en$mininum) , 1)
      }
    }
    predComb <- c(predComb, trigramPred)
    if (length(unique(predComb)) >= nPred){
      return(data.table(pred = unique(predComb)[1:nPred],
                        confidence = confidence))
    } else {
      # Input is a unigram or no trigrampred was found
      input2 <- paste(tail(splitInput, 1), collapse = " ")
      if (useKN){
        bigramPred <- rev(ngramData$allBigramsKN_en[ngram == input2]$nextword)
        if(length(bigramPred) > 0 & is.na(confidence)){
          count <- tail(ngramData$allBigramsKN_en[ngram == input2]$count, 1)
          confidence <- quantile((count - minmax$allBigramsKN_en$mininum) /
                                   (minmax$allBigramsKN_en$maximum -
                                      minmax$allBigramsKN_en$mininum) , 1)
        }
      } else {
        bigramPred <- rev(ngramData$allBigrams_en[ngram == input2]$nextword)
        if(length(bigramPred) > 0 & is.na(confidence)){
          count <- tail(ngramData$allBigrams_en[ngram == input2]$count, 1)
          confidence <- quantile((count - minmax$allBigrams_en$mininum) /
                                   (minmax$allBigrams_en$maximum -
                                      minmax$allBigrams_en$mininum) , 1)
        }
      }
      predComb <- c(predComb, bigramPred)
      if (length(unique(predComb)) >= nPred){
        return(data.table(pred = unique(predComb)[1:nPred],
                          confidence = confidence))
      } else {
        # Input is an unknown n-gram / token
        input2 <- paste(tail(splitInput, 1), collapse = " ")
        skipfivegramPred <- rev(ngramData$allSkipFiveGrams_en[ngram == input2]$nextword)
        if(length(skipfivegramPred) > 0 & is.na(confidence)){
          count <- tail(ngramData$allSkipFiveGrams_en[ngram == input2]$count, 1)
          confidence <- quantile((count - minmax$allSkipFiveGrams_en$mininum) /
                                   (minmax$allSkipFiveGrams_en$maximum -
                                      minmax$allSkipFiveGrams_en$mininum) , 1)
        }
        predComb <- c(predComb, skipfivegramPred)
        if (length(unique(predComb)) >= nPred){
          return(data.table(pred = unique(predComb)[1:nPred],
                            confidence = confidence))
        } else {
          # Try skip-6-grams
          skipsixgramPred <- rev(ngramData$allSkipSixGrams_en[ngram == input2]$nextword)
          if(length(skipsixgramPred) > 0 & is.na(confidence)){
            count <- tail(ngramData$allSkipSixGrams_en[ngram == input2]$count, 1)
            confidence <- quantile((count - minmax$allSkipSixGrams_en$mininum) /
                                     (minmax$allSkipSixGrams_en$maximum -
                                        minmax$allSkipSixGrams_en$mininum) , 1)
          }
          predComb <- c(predComb, skipsixgramPred)
          if (length(unique(predComb)) >= nPred){
            return(data.table(pred = unique(predComb)[1:nPred],
                              confidence = confidence))
          } else {
            # No matching (skip) n-grams found
            tokenPred <- rev(tail(ngramData$allTokens_en$nextword, nPred))
            predComb <- c(predComb, tokenPred)
            return(data.table(pred = unique(predComb)[1:nPred],
                              confidence = confidence))
          }
        }
      }
    }
  }
}
