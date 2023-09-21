
require(xlsx)
require(tidyverse)
require(udpipe)

conllu.df <- readRDS("online_processed_test.RDS")

x <-  udpipe_read_conllu("processed.conllu")

write.xlsx(x[1:100, ], file = "ModelOfConllu.xlsx")


####

### create vectors of col names for morphology of target and parent tokens

working.df <- x

z <- working.df$feats %>%
  str_split(., "\\|", simplify = TRUE) # make a matrix of each name-value pair in the "feats" col of the input (e.g., "Case=Nom")

m <- ncol(z) # for loop: number of cols in z

morphs.v <- NULL # vector to store results of loop


for (j in seq_len(m)) { # loop through cols of z
  
  a <- z[, j] %>% 
    str_split(., "=") %>% # split name from value for each pair
    sapply(., magrittr::extract, 1) # extract and keep only names, not values
  
  morphs.v <- c(morphs.v, a) # put results in vector
  
  b <- z[, j] %>% 
    str_split(., "=") %>% # split name from value for each pair
    sapply(., magrittr::extract, 2) # extract and keep only vales, not names
  
  
  
}

morphs.v <-  unique(morphs.v) # remove duplicates

morphs.v <- morphs.v[- which(morphs.v == "")] # remove empty categories
morphs.v[which(is.na(morphs.v))] <- "Not_App" # rename NA: this is not an acceptable name for a col


working.df[, morphs.v] <- NA # add "self-" cols for morphology

z <- working.df$feats %>%
  str_split(., "\\|", simplify = TRUE) # make a matrix of each name-value pair in the "feats" col of the input (e.g., "Case=Nom")

z[which(z == "")] <- "Not_App"
z[is.na(z)] <- "Not_App"



for (j in seq_len(ncol(z))) {
  
  a <- apply(z[, j, drop=F], 1,   function(x) sub(".*=", "", x)    )
  b <- apply(z[, j, drop=F], 1,   function(x) sub("=.*", "", x)    )
  
  #b <- paste0("self_", b)
  
  for (n in seq_along(a)) {
    working.df[n, b[n]] <- a[n]
    
  }
}

###
working.df <- add_column(working.df, global_token_id = 1:nrow(working.df), .before = TRUE)

x <- working.df$sentence_id %>%
  unique() # vector with id number of each sentence

parent_holder.v <- NULL # vector to store result of loop
#dd_holder.v <- NULL # vector to store result of loop

for (n in seq_along(x)) { # loop to create vector of parent term_ids
  
  a <- working.df %>%
    filter(sentence_id == x[n]) # df with rows sentence by sentence
  
  b <- as.numeric(a$head_token_id) # vector with head_token_id for each row in sentence 
  
  b[which(b == 0)] <- NA # eliminate any head_token_id with value 0
  
  
  
  parent_holder.v <- c(parent_holder.v,  a$global_token_id[b] %>%
                         as.numeric() ) # add parent term_token_id values for current sentence to vector
  
}

working.df <- add_column(working.df, global_parent_id = parent_holder.v, .before = TRUE)
####

###
swap.df <- add_column(swap.df, global_token_id = 1:nrow(swap.df), .before = TRUE)

x <- swap.df$sentence_id %>%
  unique() # vector with id number of each sentence

parent_holder.v <- NULL # vector to store result of loop
#dd_holder.v <- NULL # vector to store result of loop

for (n in seq_along(x)) { # loop to create vector of parent term_ids
  
  a <- swap.df %>%
    filter(sentence_id == x[n]) # df with rows sentence by sentence
  
  b <- as.numeric(a$head_token_id) # vector with head_token_id for each row in sentence 
  
  b[which(b == 0)] <- NA # eliminate any head_token_id with value 0
  
  
  
  parent_holder.v <- c(parent_holder.v,  a$global_token_id[b] %>%
                         as.numeric() ) # add parent term_token_id values for current sentence to vector
  
}

swap.df <- add_column(swap.df, global_parent_id = parent_holder.v, .before = TRUE)



z <- working.df$sentence_id %>% unique()
z[1000:1473]


saveRDS(swap.df, file = "online_parse_eval.RDS")
saveRDS(working.df, file = "gold_parse_eval.RDS")
