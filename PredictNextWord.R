

library(tm)        # required to tm_map function
library(stringr)   # required to split phrases into individual words

progress <- shiny::Progress$new()

progress$set(message="Initializing: ", value=0)
progress$inc(0/100, detail="Loading 1Gram")
df.1gram <- readRDS(file="NGram1.rds")
progress$inc(1/100, detail="Loading 2-Gram")
df.2gram <- readRDS(file="NGram2.rds")
progress$inc(30/100, detail="Loading 3-Gram")
df.3gram <- readRDS(file="NGram3.rds")
progress$inc(60/100, detail="Loading 4-Gram")
df.4gram <- readRDS(file="NGram4.rds")
progress$inc(91/100, detail="Loading prediction function")

ignore_words <- c("a","an","the","and","to","of","at","in","on","from")

fun.predictnext <- function(input, autocomplete = FALSE, n=5) {
   
   corpus <- Corpus(VectorSource(input))
   corpus <- tm_map(corpus, content_transformer(tolower))
   corpus <- tm_map(corpus, removeWords, ignore_words)
   
   input <- str_replace(gsub("\\s+", " ", str_trim(corpus[[1]]$content)), "B", "b")
   
   df.input <- data.frame(a=str_split(input, " "), stringsAsFactors = FALSE)
   colnames(df.input)="Term"
   
   df.predict <- data.frame('Term.3'    = character(),
                            'Term.2'    = character(),
                            'Term.1'    = character(),
                            'Term'      = character(),
                            Source      = integer(),
                            Probability = numeric(),
                            stringsAsFactors = FALSE)
   
   if (autocomplete == FALSE) {
      df.input <- rbind(df.input, data.frame(Term=as.character("*"), stringsAsFactors = FALSE))
   } else {
      df.input$Term[nrow(df.input)] <- paste("^", df.input$Term[nrow(df.input)], sep="")
   }
   
   # 4-gram prediction
   if (nrow(df.input) >=4) {
      df.4predict <- df.4gram[df.4gram$Term4==df.input$Term[nrow(df.input)-3] &
                              df.4gram$Term3==df.input$Term[nrow(df.input)-2] &
                              df.4gram$Term2==df.input$Term[nrow(df.input)-1],]
      if (autocomplete==TRUE) {
         df.4predict <- df.4predict[grepl(df.input$Term[nrow(df.input)], df.4predict$Term1),]
      }
      df.predict <- rbind(df.predict, data.frame('Term.3'    = df.4predict$Term4, 
                                                 'Term.2'    = df.4predict$Term3, 
                                                 'Term.1'    = df.4predict$Term2, 
                                                 'Term'      = df.4predict$Term1, 
                                                 Source      = rep("4-Gram", nrow(df.4predict)),
                                                 Probability = df.4predict$Prob_KN, stringsAsFactors = FALSE) )
   }
   
   # 3-gram prediction
   if (nrow(df.input) >=3 & nrow(df.predict) < (2*n)) {
      df.3predict <- df.3gram[df.3gram$Term3==df.input$Term[nrow(df.input)-2] &
                              df.3gram$Term2==df.input$Term[nrow(df.input)-1],]
      if (autocomplete==TRUE) {
         df.3predict <- df.3predict[grepl(df.input$Term[nrow(df.input)], df.3predict$Term1),]
      }
      df.3predict <- df.3predict[! df.3predict$Term1 %in% df.predict$Term,]
      df.predict <- rbind(df.predict, data.frame('Term.3'    = rep("", nrow(df.3predict)), 
                                                 'Term.2'    = df.3predict$Term3, 
                                                 'Term.1'    = df.3predict$Term2, 
                                                 'Term'      = df.3predict$Term1, 
                                                 Source      = rep("3-Gram", nrow(df.3predict)),
                                                 Probability = df.3predict$Prob_KN, stringsAsFactors = FALSE) )

   }
   
   # 2-gram prediction
   if (nrow(df.input) >=2  & nrow(df.predict) < (2*n)) {
      df.2predict <- df.2gram[df.2gram$Term2==df.input$Term[nrow(df.input)-1],]
      if (autocomplete==TRUE) {
         df.2predict <- df.2predict[grepl(df.input$Term[nrow(df.input)], df.2predict$Term1),]
      }
      df.2predict <- df.2predict[! df.2predict$Term1 %in% df.predict$Term,]
      df.predict <- rbind(df.predict, data.frame('Term.3'    = rep("", nrow(df.2predict)), 
                                                 'Term.2'    = rep("", nrow(df.2predict)), 
                                                 'Term.1'    = df.2predict$Term2, 
                                                 'Term'      = df.2predict$Term1, 
                                                 Source      = rep("2-Gram", nrow(df.2predict)),
                                                 Probability = df.2predict$Prob_KN, stringsAsFactors = FALSE) )
   }
   
   # 1-gram prediction (for autocomplete only)
   if (nrow(df.input) >=1  & nrow(df.predict) < (2*n) & autocomplete==TRUE) {
      df.1predict <- df.1gram[grepl(df.input$Term[nrow(df.input)], df.1gram$Term1),]
      df.1predict <- df.1predict[! df.1predict$Term1 %in% df.predict$Term,]
      df.predict <- rbind(df.predict, data.frame('Term.3'    = rep("", nrow(df.1predict)), 
                                                 'Term.2'    = rep("", nrow(df.1predict)), 
                                                 'Term.1'    = rep("", nrow(df.1predict)), 
                                                 'Term'      = df.1predict$Term1, 
                                                 Source      = rep("1-Gram", nrow(df.1predict)),
                                                 Probability = df.1predict$Prob_KN, stringsAsFactors = FALSE) )
   }
   
   return(df.predict)
}

on.exit(progress$close())