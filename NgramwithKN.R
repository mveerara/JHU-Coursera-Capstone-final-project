#--------------------------------------------------------------------------
# Kneser-Ney smoothing. Convert "tables" of counts to Kneser-Ney-smoothed
# probabilities
#source("~/predictNGramSample.R")
load("~/CapstoneprojectData/final/en_US/AllWords2.RData")
### Bigrams
load("./CapstoneprojectData/final/en_US/AllBigrams2.RData")
allBigramsKN <- allBigrams2[count >= 1]
#rm(allBigrams2);gc()
bigrams <- do.call(rbind, strsplit(allBigramsKN$ngram, split = " "))
allBigramsKN$ngram <- bigrams[, 1]
allBigramsKN$nextword <- bigrams[, 2]
#rm(bigrams)
gc()
setkey(allBigramsKN, "count", "ngram")
# Discounting
allBigramsKN[, D := 0]
Y <- (nrow(allBigramsKN[count == 2]) /
        (nrow(allBigramsKN[count == 2]) + 2 * nrow(allBigramsKN[count == 3]) ))
# = 0.54
allBigramsKN[count == 1]$D <- 1 - 2 * Y * (nrow(allBigramsKN[count == 2]) /
                                             nrow(allBigramsKN[count == 1]))
# 0.7
allBigramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allBigramsKN[count == 3]) /
                                             nrow(allBigramsKN[count == 2]))
# 1.01
allBigramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allBigramsKN[count == 4]) /
                                            nrow(allBigramsKN[count == 3]))
# 1.4
allBigramsKN[, count := count - D]

# divide by c(w_{i-1}): Count of allBigramsKN$ngram (from allTokens)

load("./CapstoneprojectData/final/en_US/Wordsfreq.RData")
allBigramsKN <- join(allBigramsKN, freq1, by="ngram")
#rm(Words);gc()
setnames(allBigramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allBigramsKN[, count := count/ngramcount]
# calculate lambda
allBigramsKN[, lambda := D / ngramcount]
NNextwords <- allBigramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allBigramsKN <- merge(allBigramsKN, NNextwords, by = "ngram")
#rm(NNextwords)
allBigramsKN[, lambda := lambda * ngram.NNextwords]
# calculate P(continuation)
NNewCont <- allBigramsKN[, .(nextword.NNewCont = length(ngram)), by = nextword]
allBigramsKN <- merge(allBigramsKN, NNewCont, by = "nextword")
allBigramsKN[, Pcont := nextword.NNewCont / nrow(allBigramsKN)]
# add lambda * Pcont to the term for P_{KN}
allBigramsKN[, count := count + lambda * Pcont]
save(allBigramsKN, file = "./CapstoneprojectData/final/en_US/allBigramsKN.RData")

### Trigrams
load("./CapstoneprojectData/final/en_US/trigramfreq.RData")
allTrigramsKN <- freq3[count >= 1] # saves 1.5GB
#rm(allTrigrams2); gc()
trigrams <- do.call(rbind, strsplit(allTrigramsKN$ngram, split = " "))
allTrigramsKN$ngram <-  apply(trigrams[, 1:2], 1,
                            FUN = function(x) paste(x, collapse = " "))
#allTrigramsKN$ngram <- trigrams[, 1]
allTrigramsKN$nextword <- trigrams[, 3]
#rm(trigrams)
setkey(allTrigramsKN, "count", "ngram")
# Discounting
allTrigramsKN[, D := 0]
# using trigrams with count = 1 or 3
Y <- (nrow(allTrigramsKN[count == 2]) /
        (nrow(allTrigramsKN[count == 2]) + 2 * nrow(allTrigramsKN[count == 3])))
# = 0.6
allTrigramsKN[count == 1]$D <- 1 - 2 * Y * (nrow(allTrigramsKN[count == 2]) /
                                              nrow(allTrigramsKN[count == 1]))
allTrigramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allTrigramsKN[count == 3]) /
                                             nrow(allTrigramsKN[count == 2]))

allTrigramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allTrigramsKN[count == 4]) /
                                             nrow(allTrigramsKN[count == 3]))
allTrigramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allTrigramsKN$ngram (from allBigrams)
load("./CapstoneprojectData/final/en_US/bigramfreq.RData")
allTrigramsKN <- join(allTrigramsKN, freq2, by="ngram")

setnames(allTrigramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allTrigramsKN[, count := count / ngramcount]
#rm(Bigram); gc()
# calculate lambda
allTrigramsKN[, lambda := D / ngramcount]
NNextwords <- allTrigramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allTrigramsKN <- merge(allTrigramsKN, NNextwords, by = "ngram")
#rm(NNextwords)
allTrigramsKN[, lambda := lambda * ngram.NNextwords]
# P_{KN}(wi | w^{i-1}_{i-n+2}), i.e. P_{KN} of the last word of ngram and nextword
# from allBigramsKN
# 2. part of the n-gram for the table of bigrams
load("./CapstoneprojectData/final/en_US/allBigramsKN.RData")
lowerNgram <- as.character(sapply(allTrigramsKN$ngram,
                                  function(x) unlist(strsplit(x, split = " "))[2]))
allTrigramsKN <- cbind(allTrigramsKN, lowerNgram)
allBigramsKN2 <- allBigramsKN[, .(ngram, nextword, count)]
setnames(allBigramsKN2, c("lowerNgram", "nextword", "count"))
allTrigramsKN <- merge(allTrigramsKN, allBigramsKN2, by = c("lowerNgram", "nextword"))
setnames(allTrigramsKN, c("lowerNgram", "nextword", "ngram", "count",
                          "D", "ngramcount", "lambda", "ngram.NNextwords",
                          "PknLower"))
# add lambda * PknLower to the term for P_{KN}
allTrigramsKN[, count := count + lambda * PknLower]
save(allTrigramsKN, file = "./CapstoneprojectData/final/en_US/allTrigramsKN.RData")
#rm(allBigramsKN2); gc()

### Fourgrams
AllQuadgrams2 <- load("./CapstoneprojectData/final/en_US/quadgramfreq.RData")
allFourgrams <- freq4[count >1] # saves 1,8GB
allFourgramsKN <- allFourgrams
#rm(AllQuadrams2);rm(allFourgrams); gc()
fourgrams <- do.call(rbind, strsplit(allFourgramsKN$ngram, split = " "))
allFourgramsKN$ngram <-  apply(fourgrams[, 1:3], 1,
                               FUN = function(x) paste(x, collapse = " "))
allFourgramsKN$nextword <- fourgrams[, 3]
#rm(fourgrams)
setkey(allFourgramsKN, "count", "ngram")
# Discounting
allFourgramsKN[, D := 0]
Y <- (nrow(allFourgramsKN[count == 2]) /
        (nrow(allFourgramsKN[count == 2]) + 2 * nrow(allFourgramsKN[count == 3])))
# = 0.6
allFourgramsKN[count == 1]$D <- 1 - 2 *Y *(nrow(allFourgramsKN[count == 2]) /
                                             nrow(allFourgramsKN[count == 1]))
allFourgramsKN[count == 2]$D <- 2 - 3 * Y * (nrow(allFourgramsKN[count == 3]) /
                                               nrow(allFourgramsKN[count == 2]))
allFourgramsKN[count > 2]$D <- 3 - 4 * Y * (nrow(allFourgramsKN[count == 4]) /
                                              nrow(allFourgramsKN[count == 3]))
allFourgramsKN[, count := count - D]
# divide by c(w_{i-1}): Count of allFourgramsKN$ngram (from allTrigrams)
load("./CapstoneprojectData/final/en_US/Trigramfreq.RData")
allFourgramsKN <- join(allFourgramsKN, freq3, by = "ngram")

setnames(allFourgramsKN, c("ngram", "count", "nextword", "D", "ngramcount"))
allFourgramsKN[, count := count / ngramcount]
#rm(freq3); gc()
# lambda
allFourgramsKN[, lambda := D / ngramcount]
NNextwords <- allFourgramsKN[, .(ngram.NNextwords = length(nextword)), by = ngram]
allFourgramsKN <- join(allFourgramsKN, NNextwords, by = "ngram")
#rm(NNextwords)
allFourgramsKN[, lambda := lambda * ngram.NNextwords]
# P_{KN}(wi | w^{i-1}_{i-n+2}), i.e. P_{KN} of the last word of ngram and nextword
# from allTrigramsKN
# 2. part of the n-grams as n-gram for the table of Trigrams
load("./CapstoneprojectData/final/en_US/allTrigramsKN.RData")
lowerNgram <- as.character(sapply(allFourgramsKN$ngram,
                                  function(x){
                                    paste(unlist(strsplit(x, split = " "))[2:3],
                                          collapse = " ")}
))
allFourgramsKN <- cbind(allFourgramsKN, lowerNgram)
allTrigramsKN2 <- allTrigramsKN[, .(ngram, nextword, count)]
#rm(allTrigramsKN); gc()
setnames(allTrigramsKN2, c("lowerNgram", "nextword", "count"))
allFourgramsKN <- merge(allFourgramsKN, allTrigramsKN2,
                        by = c("lowerNgram", "nextword"),allow.cartesian=TRUE)
setnames(allFourgramsKN, c("lowerNgram", "nextword", "ngram", "count",
                           "D", "ngramcount", "lambda", "ngram.NNextwords",
                           "PknLower"))
# add lambda * PknLower to the term for P_{KN}
allFourgramsKN[, count := count + lambda * PknLower]
save(allFourgramsKN, file = "./CapstoneprojectData/final/en_US/allFourgramsKN.RData")

