require(tidyverse)
require(udpipe)

files.v <- dir(pattern = ".conllu")


drops.v <-  which(str_detect(files.v, "_parsed") == TRUE)
files.v <- files.v[-drops.v]

BookAndHymn.v <- files.v %>% str_extract("ṚV.*") %>%
  gsub("-.*", "", .) %>%
  gsub(", ", "_", .) %>%
  gsub("Ṛ", "R", .)

Book.v <- BookAndHymn.v %>%
  gsub("RV_", "", .) %>%
  gsub("_.*", "", .)

#####

working.df <- udpipe_read_conllu(files.v[1])

z <- working.df$feats %>%
  str_split(., "\\|", simplify = TRUE) # make a matrix of each name-value pair in the "feats" col of the input (e.g., "Case=Nom")

m <- ncol(z) # for loop: number of cols in z

morphs.v <- NULL # vector to store results of loop


for (j in seq_len(m)) { # loop through cols of z
  
  a <- z[, j] %>% 
    str_split(., "=") %>% # split name from value for each pair
    sapply(., magrittr::extract, 1) # extract and keep only names, not values
  
  morphs.v <- c(morphs.v, a) # put results in vector
 
}

morphs.v <-  unique(morphs.v) # remove duplicates

morphs.v <- morphs.v[- which(morphs.v == "")] # remove empty categories
morphs.v[which(is.na(morphs.v))] <- "Not_App" # rename NA: this is not an acceptable name for a col

self_names.v <- paste0("self_", morphs.v) # vector of col names for target word
parent_names_v <- paste0("parent_", morphs.v) # vector of col names for parent of target


######### add metadata, convert to df, record colnames for morphs
holder.l <- vector(mode = "list", length(files.v))
for (i in seq_along(files.v)) {
  working.df <- udpipe_read_conllu(files.v[i])
  working.df <- add_column(working.df, Book_and_Hymn = BookAndHymn.v[i], .before = TRUE)
  working.df <- add_column(working.df, Book_Id = Book.v[i], .before = TRUE)
  
  z <- working.df$feats %>%
    str_split(., "\\|", simplify = TRUE) # make a matrix of each name-value pair in the "feats" col of the input (e.g., "Case=Nom")
  
  m <- ncol(z) # for loop: number of cols in z
  
  morphs.v <- NULL # vector to store results of loop
  
  
  for (j in seq_len(m)) { # loop through cols of z
    
    a <- z[, j] %>% 
      str_split(., "=") %>% # split name from value for each pair
      sapply(., magrittr::extract, 1) # extract and keep only names, not values
    
    morphs.v <- c(morphs.v, a) # put results in vector
    
  }
  
  morphs.v <-  unique(morphs.v) # remove duplicates
  FileName.v <- paste0( BookAndHymn.v[i], ".RDS")
  
  FileAndPath.v <-  file.path(".", "data", FileName.v)
  
  holder.l[[i]] <- morphs.v
  
  saveRDS(working.df, FileAndPath.v)
  print(paste("completed file", i))
  
}


NamesForMorph.v <- unlist(holder.l) %>% unique()


NamesForMorph.v <- NamesForMorph.v[- which(NamesForMorph.v == "")] # remove empty categories
NamesForMorph.v[which(is.na(NamesForMorph.v))] <- "Not_App" # rename NA: this is not an acceptable name for a col

self_names.v <- paste0("self_", NamesForMorph.v)


#### loop to populate morpho-syntactic cols
# change to proper working directory

files.v <- dir(pattern = ".RDS")

working.df$misc

for (i in seq_along(files.v)) { # loop to make separate col for each category of morpho-syntactic data
  
  working.df <- readRDS(files.v[i]) # load file from dir; file should be data frame
  
 
  # create col with term_id of the parent of each target token
  # ditto for dependency distance of each target token
  
  x <- working.df$sentence_id %>%
    unique() # vector with id number of each sentence
 
 
  dd_holder.v <- NULL # vector to store result of loop
  
  for (n in seq_along(x)) { # loop to create vector of parent term_ids
    
    a <- working.df %>%
      filter(sentence_id == x[n]) # df with rows sentence by sentence
    
    b <- as.numeric(a$head_token_id) # vector with head_token_id for each row in sentence 
    
    b[which(b == 0)] <- NA # eliminate any head_token_id with value 0
    
    dd_holder.v <- c(dd_holder.v, (a$token_id %>%
                                     as.numeric() ) - b )
    
   
  }
  
  
  
  working.df <- working.df %>%
    mutate(self_POS = upos) # create new col for part of speech of target word (marked with prefix "self")
  
  working.df <- working.df %>%
    mutate(self_rel = dep_rel) # ditto for the dependency relation 
  
  working.df <- working.df %>%
    mutate(self_dd = dd_holder.v) # ditto for dependency distance
  
  working.df[which(working.df$self_dd > 0), "self_arc_dir"] <- "parent_precedes" # ditto for arc direction
  working.df[which(working.df$self_dd < 0), "self_arc_dir"] <- "parent_follows"
  
  
  
  
  
  
  working.df[, self_names.v] <- NA # add "self-" cols for morphology
  
  z <- working.df$feats %>%
    str_split(., "\\|", simplify = TRUE) # make a matrix of each name-value pair in the "feats" col of the input (e.g., "Case=Nom")
  
  z[which(z == "")] <- "Not_App"
  z[is.na(z)] <- "Not_App"
  
  
  
  for (j in seq_len(ncol(z))) {
    
    a <- apply(z[, j, drop=F], 1,   function(x) sub(".*=", "", x)    )
    b <- apply(z[, j, drop=F], 1,   function(x) sub("=.*", "", x)    )
    
    b <- paste0("self_", b)
    
    for (n in seq_along(a)) {
      working.df[n, b[n]] <- a[n]
      
    }
    
    
  }
  
  
  
  
  
  fp <- file.path("parsed_expanded", files.v[i]) # create file path for saving
  
  saveRDS(working.df, files.v[i])
  print(paste0("completed file ", files.v[i]))
  
  
}

files.v2 <- files.v


# x <- readRDS(files.v[i])

for (n in 1:9) {
  OldBook.v <- paste0("_", n, "_")
  NewBook.v <- paste0("_", "0", n, "_")
  
  OldHymn.v <- paste0("_", n, ".RDS")
  NewHymn.v <- paste0("_", "0", n, ".RDS")
  
  files.v2[which(str_detect(files.v2, OldBook.v))] <- files.v2[which(str_detect(files.v2, OldBook.v))] %>%
    gsub(OldBook.v, NewBook.v, .)
  
  files.v2[which(str_detect(files.v2, OldHymn.v))] <- files.v2[which(str_detect(files.v2, OldHymn.v))] %>%
    gsub(OldHymn.v, NewHymn.v, .)
  
  
  
  
  
}

for (i in seq_along(files.v)) {
  
  x <- readRDS(files.v[i])
  
  fp <- file.path(".", "renumbered", files.v2[i])
  saveRDS(x, file = fp)
  
  print(paste0("finished ", i))
  
}




n <- 1
