#----------------------------------------------------------------------------
# Function to predict the next word based on Kneser-Ney n-gram probabilities
# or simple counts using a backoff model. The Kneser-Ney model backs off to
# token probabilities if the n-gram is not known

# Save all counts in probabilities in one list of data.tables
load("./CapstoneprojectData/final/en_US/Wordsfreq.RData")
load("./CapstoneprojectData/final/en_US/bigramfreq.RData")
load("./CapstoneprojectData/final/en_US/trigramfreq.RData")
allTokens_en <- freq1[count > 3]
allBigrams_en <- freq2[count > 3]
allTrigrams_en <- freq3[count > 3]
#rm(allTokens, allBigrams, allTrigrams)
load("./CapstoneprojectData/final/en_US/quadgramfreq.RData")
allFourgrams_en <- freq4[count > 3]
#rm(allFourgrams)

# Split ngram into first word(s) and the next word
# for the data.tables of simple counts
fourgrams <- do.call(rbind, strsplit(allFourgrams_en$ngram, split = " "))
fourgrams <- cbind(apply(fourgrams[, 1:3], 1, function(x) paste(x, collapse = " ")),
                   fourgrams[, 4])
allFourgrams_en$ngram <- fourgrams[, 1]
allFourgrams_en$nextword <- fourgrams[, 2]
# Exclude fourgrams in which ' is a word
#delete <- grep(pattern = "^'+", allFourgrams_en$ngram)
#allFourgrams_en <- allFourgrams_en[-delete, ]
#delete <- grep(pattern = "^'+", allFourgrams_en$nextword)
#allFourgrams_en <- allFourgrams_en[-delete, ]

trigrams <- do.call(rbind, strsplit(allTrigrams_en$ngram, split = " "))
trigrams <- cbind(apply(trigrams[, 1:2], 1, function(x) paste(x, collapse = " ")),
                  trigrams[, 3])
allTrigrams_en$ngram <- trigrams[, 1]
allTrigrams_en$nextword <- trigrams[, 2]
# Exclude trigrams in which ' is a word
#delete <- grep(pattern = "^'+", allTrigrams_en$ngram)
#allTrigrams_en <- allTrigrams_en[-delete, ]
#delete <- grep(pattern = "^'+", allTrigrams_en$nextword)
#allTrigrams_en <- allTrigrams_en[-delete, ]

bigrams <- do.call(rbind, strsplit(allBigrams_en$ngram, split = " "))
allBigrams_en$ngram <- bigrams[, 1]
allBigrams_en$nextword <- bigrams[, 2]
# Exclude bigrams in which ' is a word
#delete <- grep(pattern = "^'+", allBigrams_en$ngram)
#allBigrams_en <- allBigrams_en[-delete, ]
#delete <- grep(pattern = "^'+", allBigrams_en$nextword)
#allBigrams_en <- allBigrams_en[-delete, ]

# KN smoothed english
#load("allFourgramsKN.RData")
#load("allTrigramsKN.RData")
allTrigramsKN_en <- allTrigramsKN[, .(ngram, nextword, count)]
allFourgramsKN_en <- allFourgramsKN[, .(ngram, nextword, count)]
#rm(allTrigramsKN, allFourgramsKN)
#load("allBigramsKN.RData")
allBigramsKN_en <- allBigramsKN[, .(ngram, nextword, count)]
#rm(allBigramsKN)
# Exclude n-grams that start with '
#delete <- grep(pattern = "^'+", allBigramsKN_en$ngram)
#delete <- c(delete, grep(pattern = "^'+", allBigramsKN_en$nextword))
#allBigramsKN_en <- allBigramsKN_en[-delete, ]
#delete <- grep(pattern = "^'+", allTrigramsKN_en$ngram)
#delete <- c(delete, grep(pattern = "^'+", allTrigramsKN_en$nextword))
#allTrigramsKN_en <- allTrigramsKN_en[-delete, ]
#delete <- grep(pattern = "^'+", allFourgramsKN_en$ngram)
#delete <- c(delete, grep(pattern = "^'+", allFourgramsKN_en$nextword))
#allFourgramsKN_en <- allFourgramsKN_en[-delete, ]

#rm(bigrams, trigrams, fourgrams)
setkey(allTokens_en, "count", "ngram")
setkey(allBigrams_en, "count", "ngram")
setkey(allTrigrams_en, "count", "ngram")
setkey(allFourgrams_en, "count", "ngram")
setkey(allBigramsKN_en, "count", "ngram")
setkey(allTrigramsKN_en, "count", "ngram")
setkey(allFourgramsKN_en, "count", "ngram")





