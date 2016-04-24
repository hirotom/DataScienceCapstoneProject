# This script is written for
# Data Science Capstone course
# Prediction Model

# Initialize

   cat("Wecome.  This R Script processes data to create prediction model for Data Science Capstone.\n")
   
   cat("\n\nPlease choose the folder containing source csv files: en_US.blogs.txt, en_US.news.txt, en_US.twitter.txt\n")
   
   dirname <- choose.dir(caption="Select folder containing source text files: en_US.blogs.txt, en_US.news.txt, en_US.twitter.txt.")
   
   cat("   [ok] Folder information received from user.\n")
   
   filename.blog <- paste(dirname, "\\", "en_US.blogs.txt", sep="")
   filename.news <- paste(dirname, "\\", "en_US.news.txt", sep="")
   filename.twitter <- paste(dirname, "\\", "en_US.twitter.txt", sep="")

# Load required libraries

   memory.limit(size=16000)
   
   library(quanteda)  # required to perform n-gram tolkenization
   library(Matrix)    # required to convert TermDocumentMatrix into Matrix without memory allocation problems
   library(stringr)   # required to split phrases into individual words
   
   strEncoding="UTF-8"
   dl.blog    <- readLines(filename.blog, warn=FALSE, encoding=strEncoding)
   dl.news    <- readLines(filename.news, warn=FALSE, encoding=strEncoding)
   dl.twitter <- readLines(filename.twitter, warn=FALSE, encoding=strEncoding)
   cat("   [ok] Loaded data from the 3 files into memory.\n")
   
   samplepercent <- 0.05 # 45% is around the max my 8GB memory Win10 machine can handle
   samplesize <- as.integer(samplepercent*(length(dl.blog)+length(dl.news)+length(dl.twitter)))
   set.seed(1)
   
   dl <- sample(c(dl.blog, dl.news, dl.twitter), size=samplesize, replace=FALSE)
   cat("   [ok] Sampled raw data (sample size: ", samplesize, ").\n", sep="")
   
   mycorpus <- corpus(dl)
   rm(dl, dl.blog, dl.news, dl.twitter)

   ignore_words <- c("a","an","the","and","to","of","at","in","on","from")
   
   dfm.1gram <- dfm(mycorpus, ignoredFeatures=ignore_words, removeTwitter=TRUE, ngrams=1, concatenator=" ")
   df.1gram <- data.frame(Term1=features(dfm.1gram), 
                          Count=colSums(dfm.1gram),
                          row.names=NULL,
                          stringsAsFactors = FALSE)
   df.1gram <- df.1gram[order(df.1gram$Count, decreasing=TRUE),]
   rm(dfm.1gram)
   
   dfm.2gram <- dfm(mycorpus, ignoredFeatures=ignore_words, removeTwitter=TRUE, ngrams=2, concatenator=" ")
   df.Ngram <- data.frame(NGram=features(dfm.2gram), 
                          Count=colSums(dfm.2gram),
                          row.names=NULL,
                          stringsAsFactors = FALSE)
   df <- str_split_fixed(df.Ngram$NGram, " ", n=2)
   df.2gram <- data.frame(Term2=df[,1], Term1=df[,2], stringsAsFactors = FALSE)
   df.2gram$Count <- df.Ngram$Count
   df.2gram <- df.2gram[order(df.2gram$Count, decreasing=TRUE),]
   rm(dfm.2gram, df.Ngram, df)
   
   dfm.3gram <- dfm(mycorpus, ignoredFeatures=ignore_words, removeTwitter=TRUE, ngrams=3, concatenator=" ")
   df.Ngram <- data.frame(NGram=features(dfm.3gram), 
                          Count=colSums(dfm.3gram),
                          row.names=NULL,
                          stringsAsFactors = FALSE)
   df <- str_split_fixed(df.Ngram$NGram, " ", n=3)
   df.3gram <- data.frame(Term3=df[,1], Term2=df[,2], Term1=df[,3], stringsAsFactors = FALSE)
   df.3gram$Count <- df.Ngram$Count
   df.3gram <- df.3gram[order(df.3gram$Count, decreasing=TRUE),]
   rm(dfm.3gram, df.Ngram, df)
   
   dfm.4gram <- dfm(mycorpus, ignoredFeatures=ignore_words, removeTwitter=TRUE, ngrams=4, concatenator=" ")
   df.Ngram <- data.frame(NGram=features(dfm.4gram), 
                          Count=colSums(dfm.4gram),
                          row.names=NULL,
                          stringsAsFactors = FALSE)
   df <- str_split_fixed(df.Ngram$NGram, " ", n=4)
   df.4gram <- data.frame(Term4=df[,1], Term3=df[,2], Term2=df[,3], Term1=df[,4], stringsAsFactors = FALSE)
   df.4gram$Count <- df.Ngram$Count
   df.4gram <- df.4gram[order(df.4gram$Count, decreasing=TRUE),]
   rm(dfm.4gram, df.Ngram, df, mycorpus)
   
# Prediction Model
   
   # define prediction function
   
   discount <- function(count=0) {
      
      if (count<=0) {
         return(0)
      }
      else if (count==1) {
         return(0.448)
      }
      else {
         return(0.75)
      }
      
   }
   
   count1gram <- function(term1) {
      return(df.1gram$Count[df.1gram$Term1==term1])
   }
   
   count2gram <- function(term2, term1) {
      return(df.2gram$Count[(df.2gram$Term1==term1 &
                                 df.2gram$Term2==term2)])
   }
   
   count3gram <- function(term3, term2, term1) {
      return(df.3gram$Count[(df.3gram$Term1==term1 &
                                 df.3gram$Term2==term2 & 
                                 df.3gram$Term3==term3)])
   }
   
   count4gram <- function(term4, term3, term2, term1) {
      return(df.4gram$Count[(df.4gram$Term1==term1 & 
                                 df.4gram$Term2==term2 & 
                                 df.4gram$Term3==term3 & 
                                 df.4gram$Term4==term4)])
   }
   
   count2novel <- function(term1) {
      return(nrow(df.2gram[df.2gram$Term1==term1,]))
   }
   
   count3novel <- function(term2, term1) {
      return(nrow(df.3gram[df.3gram$Term1==term1 & df.3gram$Term2==term2,]))
   }
   
   # Precalculate record lenths for N-grams
   nrows.1gram <- nrow(df.1gram)
   nrows.2gram <- nrow(df.2gram)
   nrows.3gram <- nrow(df.3gram)
   nrows.4gram <- nrow(df.4gram)

   # Preload 3-gram counts for Term1&Term2&Term3 and Term2&Term3&Term4
   df.4gram <- merge(x=df.4gram, y=df.3gram[, c("Term3","Term2","Term1","Count")], 
                     by.x=c("Term1","Term2", "Term3"), by.y=c("Term1","Term2", "Term3"), all.x=TRUE)
   colnames(df.4gram)[colnames(df.4gram)=="Count.x"] <- "Count" 
   colnames(df.4gram)[colnames(df.4gram)=="Count.y"] <- "Count1" 
   df.4gram <- merge(x=df.4gram, y=df.3gram[, c("Term3","Term2","Term1","Count")], 
                     by.x=c("Term2","Term3", "Term4"), by.y=c("Term1","Term2", "Term3"), all.x=TRUE)
   colnames(df.4gram)[colnames(df.4gram)=="Count.x"] <- "Count" 
   colnames(df.4gram)[colnames(df.4gram)=="Count.y"] <- "Count2"
   df.4gram <- df.4gram[order(df.4gram$Count, df.4gram$Count2, df.4gram$Count1, decreasing=TRUE),]
   df.4gram <- df.4gram[c("Term4","Term3","Term2","Term1","Count","Count1","Count2")]
   cat("   [ok] Loaded 3-gram counts to 4-gram data frame\n")

   # Preload 2-gram counts for Term1&Term2 and Term2&Term3
   df.3gram <- merge(x=df.3gram, y=df.2gram, by.x=c("Term1","Term2"), by.y=c("Term1","Term2"), all.x=TRUE)
   colnames(df.3gram)[colnames(df.3gram)=="Count.x"] <- "Count" 
   colnames(df.3gram)[colnames(df.3gram)=="Count.y"] <- "Count1" 
   df.3gram <- merge(x=df.3gram, y=df.2gram, by.x=c("Term2","Term3"), by.y=c("Term1","Term2"), all.x=TRUE)
   colnames(df.3gram)[colnames(df.3gram)=="Count.x"] <- "Count" 
   colnames(df.3gram)[colnames(df.3gram)=="Count.y"] <- "Count2" 
   df.3gram <- df.3gram[order(df.3gram$Count, df.3gram$Count2, df.3gram$Count1, decreasing=TRUE),]
   df.3gram <- df.3gram[c("Term3","Term2","Term1","Count","Count1","Count2")]
   cat("   [ok] Loaded 2-gram counts to 3-gram data frame\n")
   
   # Preload 1-gram counts for Term1 and Term2
   df.2gram <- merge(x=df.2gram, y=df.1gram, by.x="Term1", by.y="Term1", all.x=TRUE)
   colnames(df.2gram)[colnames(df.2gram)=="Count.x"] <- "Count" 
   colnames(df.2gram)[colnames(df.2gram)=="Count.y"] <- "Count1"
   df.2gram <- merge(x=df.2gram, y=df.1gram, by.x="Term2", by.y="Term1", all.x=TRUE)
   colnames(df.2gram)[colnames(df.2gram)=="Count.x"] <- "Count" 
   colnames(df.2gram)[colnames(df.2gram)=="Count.y"] <- "Count2"
   df.2gram <- df.2gram[order(df.2gram$Count, df.2gram$Count2, df.2gram$Count1, decreasing=TRUE),]
   df.2gram <- df.2gram[c("Term2","Term1","Count","Count1","Count2")]
   cat("   [ok] Loaded 1-gram counts to 2-gram data frame\n")
   
   predict1KN_a <- function(n) {
      
      # This is a 1 gram probability calculator
      term1 <- df.1gram$Term1[n]
      
      # calculate discounted probability of highest N-gram
      count_n <- df.1gram$Count[n]
      count_d <- nrows.1gram
   
      Prob_KN <- count_n/count_d
      
      setTxtProgressBar(pb, n)
      return(Prob_KN)
      
   }
   
   predict2KN_a <- function(n) {
      
      # This is a 2 gram Kneser-Ney Smoothing
      term2 <- df.2gram$Term2[n]
      term1 <- df.2gram$Term1[n]
      
      # calculate discounted probability of highest N-gram
      count_n <- df.2gram$Count[n]
      disc <- discount(count_n)
      count_n_disc <- count_n - disc
      count_n_disc <- max(count_n_disc, 0) # numerator (discounted count) cannot be negative
      count_d <- df.2gram$Count2[n]
      prob_disc <- count_n_disc / count_d
      
      # calculated continuation probability of lower N-gram
      count_n <- nrow(df.2gram[df.2gram$Term1==term2,])
      normalizing_const <- discount(count_n)/df.2gram$Count1[n]
      
      count_d <- nrows.2gram
      prob_cont <- count_n / count_d
      
      Prob_KN <- prob_disc + (normalizing_const*prob_cont)

      setTxtProgressBar(pb, n)
      return(Prob_KN)

   }
   
   predict3KN_a <- function(n) {
      
      # This is a 3 gram Kneser-Ney Smoothing
      term3 <- df.3gram$Term3[n]
      term2 <- df.3gram$Term2[n]
      term1 <- df.3gram$Term1[n]
      
      # calculate discounted probability of highest N-gram
      count_n <- df.3gram$Count[n]
      disc <- discount(count_n)
      count_n_disc <- count_n - disc
      count_n_disc <- max(count_n_disc, 0) # numerator (discounted count) cannot be negative
      count_d <- df.3gram$Count2[n]
      prob_disc <- count_n_disc / count_d
      
      # calculated continuation probability of lower N-gram
      count_n <- nrow(df.3gram[df.3gram$Term1==term2&df.3gram$Term2==term3,])
      normalizing_const <- discount(count_n)/df.3gram$Count1[n]
      
      count_d <- nrows.3gram
      prob_cont <- count_n / count_d
      
      Prob_KN <- prob_disc + (normalizing_const*prob_cont)
      
      setTxtProgressBar(pb, n)
      return(Prob_KN)
   }
   
   predict4KN_a <- function(n) {
      
      # This is a 4 gram Kneser-Ney Smoothing
      term4 <- df.4gram$Term4[n]
      term3 <- df.4gram$Term3[n]
      term2 <- df.4gram$Term2[n]
      term1 <- df.4gram$Term1[n]
      
      # calculate discounted probability of highest N-gram
      count_n <- df.4gram$Count[n]
      disc <- discount(count_n)
      count_n_disc <- count_n - disc
      count_n_disc <- max(count_n_disc, 0) # numerator (discounted count) cannot be negative
      count_d <- df.4gram$Count2[n]
      prob_disc <- count_n_disc / count_d
      
      # calculated continuation probability of lower N-gram
      count_n <- nrow(df.4gram[df.4gram$Term1==term2&df.4gram$Term2==term3&df.4gram$Term3==term4,])
      normalizing_const <- discount(count_n)/df.4gram$Count1[n]
      
      count_d <- nrows.4gram
      prob_cont <- count_n / count_d
      
      Prob_KN <- prob_disc + (normalizing_const*prob_cont)
      
      setTxtProgressBar(pb, n)
      return(Prob_KN)
   }
   
# pre-calculate Kneser-Ney probability
   stop()
   # Process 1-gram
   cat("\nBegin calculating Kneser-Ney probablity for 1-Gram\n")
   pb <- txtProgressBar(min=1, max=nrow(df.1gram), style=3, width=50, char="+")
   row.names(df.1gram)<-seq(1:nrow(df.1gram))
   df.1gram$Prob_KN  <- mapply(FUN=predict1KN_a, n=seq(1:nrow(df.1gram)))
   close(pb)
   df.1gram$Prob_KN <- as.numeric(df.1gram$Prob_KN)
   df.1gram <- df.1gram[is.finite(df.1gram$Prob_KN)==TRUE,]
   df.1gram <- df.1gram[order(df.1gram$Prob_KN, df.1gram$Count, decreasing=TRUE),]
   saveRDS(df.1gram, file="NGram1.rds", compress=TRUE)
   
   # Process 2-gram
   cat("\nBegin calculating Kneser-Ney probablity for 2-Gram\n")
   pb <- txtProgressBar(min=1, max=nrow(df.2gram), style=3, width=50, char="+")
   row.names(df.2gram)<-seq(1:nrow(df.2gram))
   df.2gram$Prob_KN  <- mapply(FUN=predict2KN_a, n=seq(1:nrow(df.2gram)))
   close(pb)
   df.2gram$Prob_KN <- as.numeric(df.2gram$Prob_KN)
   df.2gram <- df.2gram[is.finite(df.2gram$Prob_KN)==TRUE,]
   df.2gram <- df.2gram[order(df.2gram$Prob_KN, df.2gram$Count, decreasing=TRUE),]
   saveRDS(df.2gram, file="NGram2.rds", compress=TRUE)
   
   # Process 3-gram
   cat("\nBegin calculating Kneser-Ney probablity for 3-Gram\n")
   pb <- txtProgressBar(min=1, max=nrow(df.3gram), style=3, width=50, char="+")
   row.names(df.3gram)<-seq(1:nrow(df.3gram))
   df.3gram$Prob_KN  <- mapply(FUN=predict3KN_a, n=seq(1:nrow(df.3gram)))
   close(pb)
   df.3gram$Prob_KN <- as.numeric(df.3gram$Prob_KN)
   df.3gram <- df.3gram[is.finite(df.3gram$Prob_KN)==TRUE,]
   df.3gram <- df.3gram[order(df.3gram$Prob_KN, df.3gram$Count, decreasing=TRUE),]
   saveRDS(df.3gram, file="NGram3.rds", compress=TRUE)
   
   # Process 4-gram
   cat("\nBegin calculating Kneser-Ney probablity for 4-Gram\n")
   pb <- txtProgressBar(min=1, max=nrow(df.4gram), style=3, width=50, char="+")
   row.names(df.4gram)<-seq(1:nrow(df.4gram))
   df.4gram$Prob_KN  <- mapply(FUN=predict4KN_a, n=seq(1:nrow(df.4gram)))
   close(pb)
   df.4gram$Prob_KN <- as.numeric(df.4gram$Prob_KN)
   df.4gram <- df.4gram[is.finite(df.4gram$Prob_KN)==TRUE,]
   df.4gram <- df.4gram[order(df.4gram$Prob_KN, df.4gram$Count, decreasing=TRUE),]
   saveRDS(df.4gram, file="NGram4.rds", compress=TRUE)
   
   