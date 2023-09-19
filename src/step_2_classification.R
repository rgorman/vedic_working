require(tidyverse)
require(stringr)
require(magrittr)
library(caret)
library(LiblineaR)

part_tibs <- function(tib, tokens) { # function to create n-fold partition of input tibble
  k_folds <- tib %>%
    nrow() %>%
    divide_by(tokens) %>%
    ceiling()
  
  p <- tib %>%
    nrow() %>%
    seq_len() %>%
    createFolds(k_folds)
  
  return(p)
  
}

# change working dir as appropriate before running code
files.v <- dir( pattern=".*RDS")


#
collected_chunks.l <- vector(mode = "list", length(files.v))
name_holder.v <- NULL

for (i in seq_along(files.v)) {
  
 
  
  working.df <- readRDS(file = files.v[i])
  working.df <- working.df %>%
    filter(upos != "PUNCT")
  
  a <- which(str_detect(colnames(working.df), "_Is_"))
  b <- which(str_detect(colnames(working.df), "_Are_"))
  
  c <- c( a, b)
  working.df <- working.df[, c]
  
  working.df <- working.df[sample(1:nrow(working.df), 20000), ]
  
  
  tib_part <- working.df %>%
    part_tibs(1000)
  
  folds <- length(tib_part)
  
  
  
  
  chunk.list <- vector(mode = "list", folds)
  
  file_name.v <- gsub( "_bin.*", "", files.v[i])
  chunk_names.v <- NULL
  
  for(j in seq_along(tib_part)) {
   
    a <-  working.df[tib_part[[j]], ]
    a <- a %>% 
      colMeans()
      
    
   chunk.list[[j]] <- a
    
    
   
   
    
    
  }
  
  
  chunk.df <- do.call(bind_rows, chunk.list)
  nomina.v <- paste0(file_name.v, "_chunk_", 1:nrow(chunk.df))
  
  chunk.df <- add_column(chunk.df, file_id = nomina.v, .before = TRUE)
  
  
  collected_chunks.l[[i]] <- chunk.df
  
  print(paste("finished with file ", i))

  
}




final.tib <- do.call(bind_rows, collected_chunks.l)




##

authors.v <- gsub("_.*", "", final.tib$file_id)  %>%
  as.factor()




final.tib <-  add_column(final.tib, authors = authors.v, .before = TRUE)

###################### figure probs

author_count <- nlevels(final.tib$authors)

author_prob <- 1 / author_count

combined_chunk_props <- NULL




for (i in seq_len(nlevels(final.tib$authors))) {
  
  
  chunk_count <- which(final.tib$authors == levels(final.tib$authors)[i]) %>%
    length()
  
  chunk_prob <- author_prob / chunk_count
  
  chunk_probs <- rep(chunk_prob, chunk_count)
  
  combined_chunk_props <- append(combined_chunk_props, chunk_probs)
  
  
}

final.tib <-  add_column(final.tib, probs = combined_chunk_props, .before = TRUE)




################## start here !

################## start here !




sparsity.l[[t]] <-  which(final.tib[, 4:ncol(final.tib)] > 0) %>%
  length()

sparsity2.l[[t]] <-  which(final.tib[, 4:ncol(final.tib)] == 0) %>%
  length()


type2.results.l <- vector(mode = "list", 1000)
type2.error.matrix.l <- vector(mode = "list", 1000)
truth.l <- vector(mode = "list", 1000)
test_chunks.l <- vector(mode = "list", 1000)
guesses.l <- vector(mode = "list", 1000)
falsity.l <- vector(mode = "list", 1000)
logistic.model.l <- vector(mode = "list", 1000)
test_preds.l <- vector(mode = "list", 1000)



print(Sys.time())
total_start_time <- Sys.time()
for (i in 1:100) {
  start_time <- Sys.time()
  #create vector of random integers = 10% of obs in smaller.df
  testing.index.v <- sample (seq (1, nrow(final.tib)), 150, prob=final.tib$probs)
  
  
  
  #create training and testing data matrices using testing.index.v and its inverse
  testing.data <- final.tib[testing.index.v, 4:ncol(final.tib)]
  training.data <- final.tib[-testing.index.v, 4:ncol(final.tib)]
  
  #create vectors of factors giving classes (here = authors) of each row in testing.data and training.data
  training.classes <- final.tib$authors[-testing.index.v] 
  testing.classes <- final.tib$authors[testing.index.v]
  
  
  
  
  
  start_time_2 <- Sys.time() 
  
  cost.v <- LiblineaR(final.tib[, 4:ncol(final.tib)], final.tib$authors, type =0, findC = TRUE )
  
  model_liblin <- LiblineaR(training.data, training.classes, type = 0, cost = cost.v)
  print(end_time_2 - start_time_2)
  
  logistic.model.l[[i]] <- model_liblin
  
  p <- predict(model_liblin, testing.data)
  #test_preds.l[[i]] <-    predict(model_liblin, test.files.df)
  
  type2.error.matrix.l[[i]] <- confusionMatrix(unlist(type2.results.l[[i]]$predictions), testing.classes, mode = 'everything')
  
  test_chunks.l[[i]] <- final.tib[testing.index.v, 3] %>%
    unlist() %>%
    as.character()
  
  guesses.l[[i]] <-  type2.results.l[[i]]   %>%
    unlist() %>%
    as.character()
  
  
  truth.l[[i]] <- testing.classes  %>%
    as.character()
  
  falsity.l[[i]] <- which(!(guesses.l[[i]] == truth.l[[i]]))
  
  
 
  
  end_time_2 <- Sys.time()
  print(end_time_2 - start_time_2)
  
  
  # store_iterations_tib <- bind_rows(store_iterations_tib, store_iterations_tib)
  
  # saveRDS(store_iterations_tib, file = "./size_and_classif/2018-6-11_omnibus_result_itbble.RDS")
  
  
  end_tme <- Sys.time()
  print(end_tme - start_time)
  print(paste("end of loop", i))
  
  
}

type2.error.matrix.l[[1]]

zz <- NULL
for (n in 1:1000) {
  zz <- append(zz, type2.error.matrix.l[[n]]$overall[1])
}

summary(zz)

