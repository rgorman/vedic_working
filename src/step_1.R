library(udpipe)
library(readtext)
library(tidyverse)


# load language model (switch to appropriate dir)
udmodel <- udpipe_load_model(file = "french-gsd-ud-2.5-191206.udpipe")

# make vector of file names (switch to appropriate dir)
files.v <- dir(pattern = ".txt")

i <- 2


for (i in seq_along(files.v)) {
  
  # read txt file from disk
  t <- readtext(file = files.v[i])
  
  # run language model
  x <- udpipe_annotate(udmodel, t$text)
  
  # convert to data frame
  x <- as.data.frame(x, detailed = TRUE)
  
  # write output to disk
  file_name <- files.v[i] %>%
    gsub(".txt", ".RDS", .)
  fp <- file.path("parsed_data", file_name) # file path for save
  
  saveRDS(x, file = fp)
  
  print(paste(file_name, " is completed"))
  
  
  
}




# code for splitting features 


# change to dir containing results of above loop
files.v <- dir(pattern = ".RDS") # load filenames into vector

##
working.df <- readRDS(files.v[1])

### create vectors of col names for morphology of target and parent tokens

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

self_names.v <- paste0("self_", morphs.v) # vector of col names for target word
parent_names_v <- paste0("parent_", morphs.v) # vector of col names for parent of target



###
#### loop to populate morpho-syntactic cols


for (i in seq_along(files.v)) { # loop to make separate col for each category of morpho-syntactic data
  
  working.df <- readRDS(files.v[i]) # load file from dir; file should be data frame
  
  # create col with term_id of the parent of each target token
  # ditto for dependency distance of each target token
  
  x <- working.df$sentence_id %>%
    unique() # vector with id number of each sentence
  
  parent_holder.v <- NULL # vector to store result of loop
  dd_holder.v <- NULL # vector to store result of loop
  
  for (n in seq_along(x)) { # loop to create vector of parent term_ids
    
    a <- working.df %>%
      filter(sentence_id == x[n]) # df with rows sentence by sentence
    
    b <- as.numeric(a$head_token_id) # vector with head_token_id for each row in sentence 
    
    b[which(b == 0)] <- NA # eliminate any head_token_id with value 0
    
    dd_holder.v <- c(dd_holder.v, (a$token_id %>%
                                     as.numeric() ) - b )
    
    parent_holder.v <- c(parent_holder.v,  a$term_id[b] %>%
                           as.numeric() ) # add parent term_token_id values for current sentence to vector
    
  }
  
  working.df[, "global_parent_id"] <- parent_holder.v # make and populate parent term_id_col in working.df
  
  
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
  
  
  ####### add values of parents to target tokens
  
  x <- working.df$global_parent_id
  
  working.df[, "parent_POS"] <- working.df$self_POS[x]
  working.df[, "parent_rel"] <- working.df$self_rel[x]
  working.df[, "parent_dd"] <- working.df$self_dd[x]
  working.df[, "parent_arc_dir"] <- working.df$self_arc_dir[x]
  
  working.df[, parent_names_v] <-   working.df[x, self_names.v]
  
  fp <- file.path("parsed_expanded", files.v[i]) # create file path for saving
  
  saveRDS(working.df, file = fp)
  print(paste0("completed file ", files.v[i]))
  
  
}



## drop certain columns

# change to dir containing results of above loop
files.v <- dir(pattern = ".RDS") # load filenames into vector


for (i in seq_along(files.v)) {
  
  working.df <- readRDS(files.v[i])
  
  drops.v <- which(str_detect(colnames(working.df), "Typo")) 
  drops.v <- c(drops.v,  which(str_detect(colnames(working.df), "Foreign")) )
  drops.v <- c(drops.v, which(str_detect(colnames(working.df), "Not_App")) )
  drops.v <- c(drops.v, which(str_detect(colnames(working.df), "Polarity")) )

  
  working.df <- working.df[, -drops.v]  
  
  saveRDS(working.df, files.v[i])
  
}




###

# find dd cut off for binning
holder <- NULL
for (i in seq_along(files.v)) {
  working.df <- readRDS(files.v[i])
  x <- working.df$self_dd
  x <- as.numeric(x)
  holder <- c(holder, x)
  
}

summary(abs(holder) )



# bin the dd
working.df[, "self_binned_dd"]  <- abs(working.df$self_dd)
working.df$self_binned_dd[which(working.df$self_binned_dd >= 3)] <- "gt_2"

working.df[, "parent_binned_dd"] <- working.df$self_binned_dd[working.df$global_parent_id]


x <- working.df %>% # make vector of col names
  colnames()
# drop selected cols

x <- x[str_detect(x, "self") | str_detect(x, "parent")] # select all cols with "self" or "parent" in name

x <- x[-which(x =="global_parent_id")] # not a variable
x <- x[-which(x =="self_dd")] # raw dds not needed 
x <- x[-which(x =="parent_dd")]



selected.cols.v <- x  # these are the cols for variables

# a loop to make all possible combinations of names in vector of selected cols
selected_vars.list <- vector(mode = "list", 3) # make list to store results
nomina.v <- NULL # vector to store names of list elements

for (k in 1:3) { 
  selected_vars.list[[k]] <- combn(selected.cols.v, k, simplify = FALSE) # make all possible combinations of variables
  nomina.v <- paste(length(selected.cols.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}


# a loop to add binned dd and make sample selection
holder.l <- vector(mode = "list", length(files.v)) # list to hold results

for (i in seq_along(files.v)) {
  
  working.df <- readRDS(file = files.v[i]) # read in files in directory
  
  # bin the dd
  working.df[, "self_binned_dd"]  <- abs(working.df$self_dd)
  working.df$self_binned_dd[which(working.df$self_binned_dd >= 3)] <- "gt_2"
  
  working.df[, "parent_binned_dd"] <- working.df$self_binned_dd[working.df$global_parent_id]
  
  working.df <-   working.df[, selected.cols.v] # drop metadata and keep only relevant columns as selected above
  
  
  
  working.df <- working.df %>%
    filter(!self_POS == "PUNCT") # drop punctuation from consideration
  
  if (nrow(working.df) >= 500 ) { # make random selection of n rows
    holder.l[[i]] <-working.df[sample(nrow(working.df), 500), ]
  } else {
    
    holder.l[[i]] <- working.df
  }
  
}

sample.df <- do.call(rbind, holder.l) # combine results into single data frame


# make 2 col data frame: each row is a unique simplex name-value pair, e.g. name = "parent_voice," value = "ACT", followed by the count of that pair

freq.tib_simplex <- gather(sample.df[,   ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Is_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))




# extract into separate vectors the variable names for combinations of two morphosyntactic categories
var_1 <- sapply(selected_vars.list[[2]], `[`, 1)
var_2 <- sapply(selected_vars.list[[2]], `[`, 2)


nomen.v <- paste(var_1, var_2, sep = "_And_") # combine simplex variable names into a combined name: e.g, "self_POS_And_self_rel"
# make matrix to store duplex name-value pairs
r <- nrow(sample.df)
c <- length(nomen.v)
duplex_sample.df <- matrix(ncol = c, nrow = r)
colnames(duplex_sample.df) <- nomen.v


# a loop to populate cols with values; values taken from original sample.df
for (i in seq_along(var_1)) {
  
  duplex_sample.df[, nomen.v[i]] <- paste(unlist(sample.df[, var_1[i]]), unlist(sample.df[, var_2[i] ]), sep = "_And_")
  
}

duplex_sample.df <- as.data.frame(duplex_sample.df) # convert matrix to data frame (required for next step)


# make 2 col data frame: each row is a unique duplex name-value pair

freq.tib_two_plex <- gather(duplex_sample.df[,  ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Are_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))





# extract into separate vectors the variable names for combinations of three morpho-syntactic categories
var_1 <- sapply(selected_vars.list[[3]], `[`, 1)
var_2 <- sapply(selected_vars.list[[3]], `[`, 2)
var_3 <- sapply(selected_vars.list[[3]], `[`, 3)

nomen.v <- paste(var_1, var_2, var_3, sep = "_And_")

r <- nrow(sample.df)
c <- length(nomen.v)
triplex_sample.df <- matrix(ncol = c, nrow = r)
colnames(triplex_sample.df) <- nomen.v



for (i in seq_along(var_1)) {
  
  triplex_sample.df[, nomen.v[i]] <- paste( unlist(sample.df[, var_1[i]]), unlist(sample.df[, var_2[i] ]), 
                                            unlist(sample.df[, var_3[i] ]),         sep = "_And_")
  
}

triplex_sample.df <- as.data.frame(triplex_sample.df)

freq.tib_three_plex <- gather(triplex_sample.df[,  ], variable_name, variable_value,  na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Are_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))

# remove any row containing a value of "NA"

freq.tib_simplex <- freq.tib_simplex %>%
  filter(!str_detect(combined, "NA"))


freq.tib_two_plex <- freq.tib_two_plex %>%
  filter(!str_detect(combined, "NA"))

freq.tib_three_plex <- freq.tib_three_plex %>%
  filter(!str_detect(combined, "NA"))

# drop all rows with frequency less than 5% of words
y <- nrow(sample.df)*.05

simplex_vars.tib <- freq.tib_simplex %>%
  filter(freq >= y)

two_plex.vars.tib <- freq.tib_two_plex %>%
  filter(freq >= y)

three_plex_vars.tib <-  freq.tib_three_plex %>%
  filter(freq >= y)



# create directory for variables and then change working directory to that dir

saveRDS(simplex_vars.tib, "simplex_5pc.RDS")
saveRDS(two_plex.vars.tib, "two_plex_5pc.RDS")
saveRDS(three_plex_vars.tib, "three_plex_5pc.RDS")

####
# add binned dd to all files
for (i in seq_along(files.v)) {
  working.df <- readRDS(files.v[[i]])
  # bin the dd
  working.df[, "self_binned_dd"]  <- abs(working.df$self_dd)
  working.df$self_binned_dd[which(working.df$self_binned_dd >= 3)] <- "gt_2"
  
  working.df[, "parent_binned_dd"] <- working.df$self_binned_dd[working.df$global_parent_id]
  
  saveRDS(working.df, files.v[[i]])
  
}


# code to populate matrix with selected var value pairs
rm(list = ls())
library(tidyverse)
library(magrittr)

# before running the loop, the selected variable combinations must be retrieved

# these data are stored at C:\Users\rgorm\Documents\non-AGLDT_trees\selected_var_val_pairs


simplex_vars.tib <- readRDS(file = "simplex_5pc.RDS")
two_tuple_vars.tib <- readRDS(file = "two_plex_5pc.RDS")
three_tuple_vars.tib <- readRDS(file = "three_plex_5pc.RDS")



##############

simplex_comb.l <- simplex_vars.tib$combined %>% #split variable and value and store in list object
  str_split("_Is_")


# split variable and value and store in list object
# there is one vector per list element
# the first element of each vector contains var name, the second contains var value

two_comb.l <- two_tuple_vars.tib$combined %>%
  str_split("_Are_")

# split the var name of each list element into component parts and store in new list
# each list element contains a vector with 2 elements
two_var.l <- sapply(two_comb.l, extract, 1) %>% # extract() needs package magrittr
  str_split("_And_")

# split the value name of each list element into component parts and store in  new list
# each list element contains a vector with 2 elements
two_val.l <- sapply(two_comb.l, extract, 2) %>%
  str_split("_And_")

# split variable and value and store in list object
# there is one vector per list element
# the first element of each vector contains var name, the second contains var value
three_comb.l <- three_tuple_vars.tib$combined %>%
  str_split("_Are_")

# split the var name of each list element into component parts and store in new list
# each list element contains a vector with 3 elements
three_var.l <- sapply(three_comb.l, extract, 1) %>%
  str_split("_And_")

# split the value name of each list element into component parts and store in  new list
# each list element contains a vector with 3 elements
three_val.l <- sapply(three_comb.l, extract, 2) %>%
  str_split("_And_")




#######################

# loop through each element in files.v and create one-hot columns for the selected combined var-value pairs
# input files should have self_ and parent_ variables for each token.

## change to working dir
files.v <- files.v <- dir( pattern=".*RDS")



for (i in seq_along(files.v)) {
  
  working.df <- readRDS(file = files.v[i])
  
  file_name <- gsub(".RDS", "", files.v[i]) # add file name to each row of df
  
  working.df  <-  cbind(file_name, working.df)
  # new.v <- gsub("-", "_", colnames(working.df)) # change hyphen to underscore in col names
  # colnames(working.df) <- new.v
  
  target.df <- working.df # create new df
  
  # add columns named according to selected var-val pairs; select name of input tibble
  # target.df[, all_vars.tib$combined] <- NA
  
  target.df[, simplex_vars.tib$combined ] <- NA
  target.df[, two_tuple_vars.tib$combined ] <- NA
  target.df[, three_tuple_vars.tib$combined ] <- NA
  
  # a loop for simplex var-val pairs
  
  working.df$self
  
  
  for (j in seq_along(simplex_comb.l)) {
    
    
    target.df[which(working.df[, simplex_comb.l[[j]][1]  ]  == simplex_comb.l[[j]][2]   ), 
              simplex_vars.tib$combined[j]   ] <- 1
    # which() identifies rows in target in which var column of working.df (working.df[, simplex_comb.l[[j]][1]  ])
    # has value required (simplex_comb.l[[j]][2] ); such cells are given value 1
    
    target.df[which(is.na(target.df[, simplex_vars.tib$combined[j] ])), simplex_vars.tib$combined[j]  ] <- 0
    # all other cells are given value 0
  }
  
  
  
  
  for (j in seq_along(two_var.l)) {
    
    
    target.df[which(working.df[, two_var.l[[j]][1] ] == two_val.l[[j]][1] &  working.df[, two_var.l[[j]][2] ] 
                    == two_val.l[[j]][2]),           two_tuple_vars.tib$combined[j]] <- 1
    # the same process as in j loop above except that it identifies where 2 columns in working.df
    # have the two values inticated in two_val.l.
    
    target.df[which(is.na(target.df[, two_tuple_vars.tib$combined[j] ])), two_tuple_vars.tib$combined[j]  ] <- 0
    
    
  }
  
  
  
  for (j in seq_along(three_var.l)) {
    
    
    
    target.df[which(working.df[, three_var.l[[j]][1] ]  == three_val.l[[j]][1]  
                    & working.df[, three_var.l[[j]][2] ]  == three_val.l[[j]][2] 
                    & working.df[, three_var.l[[j]][3] ]  == three_val.l[[j]][3] 
    )      , three_tuple_vars.tib$combined[j]    ]  <- 1
    
    # the same process as in j loop above except that it identifies where 3 columns in working.df
    # have the 3 values inticated in three_val.l.
    
    target.df[which(is.na(target.df[, three_tuple_vars.tib$combined[j] ])), three_tuple_vars.tib$combined[j]  ] <- 0
    
  }
  
  file_name <- paste0(file_name,   "_binary_val",  ".RDS") # add file type to file name for saving
  
  # ! make sub directory in current working directory called "binary_valued." Otherwise the following lines will not work!
  fp <- file.path("binary_valued", file_name) # file path for save
  
  saveRDS(target.df, file = fp) # save one-hot file to disk
  
  print(paste("finished with file ", i))
  
}


