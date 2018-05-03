
source ("~/StoreNgramProbwithKN.R")
load("./CapstoneprojectData/final/en_US/allSkipFiveGrams_clean.RData")
bigrams <- plyr::ldply(strsplit(freq5$ngram, split = " "))
#bigrams <- do.call(plyr::ldply(), strsplit(allskipfivegrams$ngram, split = " "))
freq5$ngram <- bigrams[, 1]
freq5$nextword <- bigrams[, 3]
#delete <- grep(pattern = "^'+", freq5$ngram)
#delete <- c(delete, grep(pattern = "^'+", freq5$nextword))
#delete <- c(delete, grep(pattern = "<", freq5$nextword))
allSkipFiveGrams_en <- freq5

load("./CapstoneprojectData/final/en_US/allSkipSixGrams_clean.RData")
#bigrams <- do.call(rbind, strsplit(dt6$ngram, split = " "))
bigrams <- plyr::ldply(strsplit(freq6$ngram, split = " "))
freq6$ngram <- bigrams[, 1]
freq6$nextword <- bigrams[, 2]
#delete <- grep(pattern = "^'+", allSkipSixGrams$ngram)
#delete <- c(delete, grep(pattern = "^'+", allSkipSixGrams$nextword))
#delete <- c(delete, grep(pattern = "<", allSkipSixGrams$nextword))
allSkipSixGrams_en <- freq6


allData <- list(allBigrams_en, allBigramsKN_en, allFourgrams_en,
                allFourgramsKN_en, allTokens_en, allTrigrams_en,
                allTrigramsKN_en, allSkipFiveGrams_en,allSkipSixGrams_en)

names(allData) <- c("allBigrams_en","allBigramsKN_en","allFourgrams_en",
                    "allFourgramsKN_en", "allTokens_en","allTrigrams_en",
                     "allTrigramsKN_en","allSkipFiveGrams_en","allSkipSixGrams_en")

save(allData, file = "./CapstoneprojectData/final/en_US/allData.RData")

#----------------------------------------------------------------------------
load("./CapstoneprojectData/final/en_US/allData.RData")
#load("./CapstoneprojectData/final/en_US/Test_repo_sample1.RData")
# Further pruning and cleaning (after testing and benchmarking)
allData2 <- allData
#allData2 <- lapply(allData2, function(x){
#  x[nextword != "<"]
#})
# Drop 50% of all n-grams in all tables
allData2 <- lapply(allData2, function(x){
  len <- nrow(x)
  return(x[round(len / 2) : len, ])
})
# allData2 = 330 MB

# Drop all German nextwords that consist of more than one upper case letter
#germanIndices <- grep("_de", names(allData2))
#allData2[germanIndices] <- lapply(allData2[germanIndices], function(x){
#  if(length(x$nextword) <= 0) warning("nextwords not found")
#  delete <- grep("[A-Z]{2,}", x$nextword)
#  return(x[-delete, ])
#})
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

predWrapper <- function(x){
  predictNextword_en(input = x, ngramData = allData2, useKN = T, nPred = 3)$pred
}

predictNextword_en(input = "Adam Sandler is", ngramData = allData2, useKN = T)


