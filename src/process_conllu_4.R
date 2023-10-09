require(magrittr)

sents.v <- combined.df$sentence_id %>%
  unique()

parent.holder.v <- NULL

for (i in seq_along(sents.v)) {
  a <- combined.df %>%
    filter(sentence_id == sents.v[i]) # df with rows sentence by sentence
  
  b <- as.numeric(a$head_token_id) # vector with head_token_id for each row in sentence 
  
  b[which(b == 0)] <- NA # eliminate any head_token_id with value 0
  
  
  parent.holder.v <- c(parent.holder.v,  a$GlobalTokenId[b] %>%
                         as.numeric() ) # add parent term_token_id values for current sentence to vector
  
  
  if( (i %% 100) == 0 ) {
    
    print(paste0("completed sentence ", i) )
    
  }
  
  
}

combined.df <- add_column(combined.df, Global_parent_id = parent.holder.v, .before = TRUE)


morphs.v

self_names.v <- morphs.v %>%
  paste0("self_", .)

parent_names.v <- morphs.v %>%
  paste0("parent_", .)

self_names.v <- self_names.v[-3]
parent_names.v <- parent_names.v[-3]

####### add values of parents to target tokens

x <- combined.df$Global_parent_id

combined.df[, "parent_POS"] <- combined.df$self_POS[x]
combined.df[, "parent_rel"] <- combined.df$self_rel[x]
combined.df[, "parent_dd"] <- combined.df$self_dd[x]
combined.df[, "parent_arc_dir"] <- combined.df$self_arc_dir[x]

combined.df[, parent_names.v] <-   combined.df[x, self_names.v]

fp <- file.path("parsed_expanded", files.v[i]) # create file path for saving

saveRDS(combined.df, "combined_self_and_parent.RDS")


###

# find dd cut off for binning
summary(abs(combined.df$self_dd))

summary(abs(holder) )



# bin the dd
combined.df[, "self_binned_dd"]  <- abs(combined.df$self_dd)
combined.df$self_binned_dd[which(combined.df$self_binned_dd >= 3)] <- "gt_2"

combined.df[, "parent_binned_dd"] <- combined.df$self_binned_dd[combined.df$global_parent_id]


####


x <- combined.df %>% # make vector of col names
  colnames()
# drop selected cols

x <- x[str_detect(x, "self_") | str_detect(x, "parent_")] # select all cols with "self" or "parent" in name

x <- x[-which(x =="Global_parent_id")]
x <- x[-which(x =="self_dd")]
x <- x[-which(x =="parent_dd")]
x <- x[-which(x =="self_Not_App")]


###


selected.cols.v <- x  # these are the cols for variables

# a loop to make all possible combinations of names in vector of selected cols
selected_vars.list <- vector(mode = "list", 3) # make list to store results
nomina.v <- NULL # vector to store names of list elements

for (k in 1:3) { 
  selected_vars.list[[k]] <- combn(selected.cols.v, k, simplify = FALSE) # make all possible combinations of variables
  nomina.v <- paste(length(selected.cols.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}

working.df <-   combined.df[, selected.cols.v] # drop metadata and keep only relevant columns as selected above


working.df <- working.df %>%
  filter(!self_POS == "PUNCT") # drop punctuation from consideration
s.v <- sample(1:nrow(working.df), 20000)

sample.df <- working.df[s.v, ]


########### create complex variables and find the most frequent var-val pairs

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
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Is_"))) %>%
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
  mutate(combined = paste(paste(variable_name, variable_value, sep = "_Is_"))) %>%
  select( combined) %>%
  group_by( combined) %>%
  summarize(freq = n())  %>%
  arrange(desc(freq))


##

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

####


saveRDS(simplex_vars.tib, "simplex_5pc.RDS")
saveRDS(two_plex.vars.tib, "two_plex_5pc.RDS")
saveRDS(three_plex_vars.tib, "three_plex_5pc.RDS")
############

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
  str_split("_Is_")

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
  str_split("_Is_")

# split the var name of each list element into component parts and store in new list
# each list element contains a vector with 3 elements
three_var.l <- sapply(three_comb.l, extract, 1) %>%
  str_split("_And_")

# split the value name of each list element into component parts and store in  new list
# each list element contains a vector with 3 elements
three_val.l <- sapply(three_comb.l, extract, 2) %>%
  str_split("_And_")

####
working.df <- combined.df


target.df <- working.df # create new df

# add columns named according to selected var-val pairs; select name of input tibble
# target.df[, all_vars.tib$combined] <- NA

target.df[, simplex_vars.tib$combined ] <- NA
target.df[, two_tuple_vars.tib$combined ] <- NA
target.df[, three_tuple_vars.tib$combined ] <- NA

# a loop for simplex var-val pairs


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


saveRDS(target.df, "combined_binary-valued.RDS")
