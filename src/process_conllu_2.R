require(tidyverse)
require(udpipe)
require(writexl)

test.df <- udpipe_read_conllu("sa_vedic-ud-test.conllu")

a <- test.df$token
b <- online.df$token

c <- a == b
which(c == FALSE)


str_detect(".", a)

d <- bind_cols(a, b)

colnames(d) <- c("gold", "test")

write_xlsx(d, path = "2_cols.xlsx")

b[597]

b <- b[-597]

e <- b %>% sub("\\.$", "", .)

f <- a == e
which(f == FALSE)

a[596]
e[596]

test.df$token <- f
online.df[597,]
swap.df <-  online.df[-597, ]

swap.df$token <- e

index.v <- which((swap.df$parent_Number == working.df$parent_Number) == FALSE) %>%
  length()

(9672 - index.v) / 9672

which(is.na(working.df$parent_Number) == FALSE) %>%
  length()

which((swap.df$VerbForm == working.df$VerbForm) == TRUE) %>%
  length()
(333-4)/333

index.v <- which((swap.df$Person == working.df$Person) == TRUE)

bind_cols(working.df$Person[index.v], swap.df$Person[index.v])


##############

working.df <- test.df

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

###############
x <- online.df %>% # make vector of col names
  colnames()
# drop selected cols

 x <- x[c(11, 15, 18, 19, 21:36)]

 selected.cols.v <- x

 
 
 # a loop to make all possible combinations of names in vector of selected cols
 selected_vars.list <- vector(mode = "list", 3) # make list to store results
 nomina.v <- NULL # vector to store names of list elements
 
 for (k in 1:3) { 
   selected_vars.list[[k]] <- combn(selected.cols.v, k, simplify = FALSE) # make all possible combinations of variables
   nomina.v <- paste(length(selected.cols.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
     append(nomina.v, .)
 }

 

 
 # make 2 col data frame: each row is a unique simplex name-value pair, e.g. name = "parent_voice," value = "ACT", followed by the count of that pair
 
 freq.tib_simplex <- gather(online.df[, selected.cols.v  ], variable_name, variable_value,  na.rm = TRUE) %>%
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
 r <- nrow(online.df)
 c <- length(nomen.v)
 duplex_sample.df <- matrix(ncol = c, nrow = r)
 colnames(duplex_sample.df) <- nomen.v
 
 
 # a loop to populate cols with values; values taken from original sample.df
 for (i in seq_along(var_1)) {
   
   duplex_sample.df[, nomen.v[i]] <- paste(unlist(online.df[, var_1[i]]), unlist(online.df[, var_2[i] ]), sep = "_And_")
   
 }
 
 duplex_sample.df <- as.data.frame(duplex_sample.df) # convert matrix to data frame (required for next step)

 
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
 
 r <- nrow(online.df)
 c <- length(nomen.v)
 triplex_sample.df <- matrix(ncol = c, nrow = r)
 colnames(triplex_sample.df) <- nomen.v
 
 
 
 for (i in seq_along(var_1)) {
   
   triplex_sample.df[, nomen.v[i]] <- paste( unlist(online.df[, var_1[i]]), unlist(online.df[, var_2[i] ]), 
                                             unlist(online.df[, var_3[i] ]),         sep = "_And_")
   
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
 y <- nrow(online.df)*.05
 
 simplex_vars.tib <- freq.tib_simplex %>%
   filter(freq >= y)
 
 two_plex.vars.tib <- freq.tib_two_plex %>%
   filter(freq >= y)
 
 three_plex_vars.tib <-  freq.tib_three_plex %>%
   filter(freq >= y)

 
 
 saveRDS(simplex_vars.tib, "simplex_5pc.RDS")
 saveRDS(two_plex.vars.tib, "two_plex_5pc.RDS")
 saveRDS(three_plex_vars.tib, "three_plex_5pc.RDS") 

 
 # code to populate matrix with selected var value pairs
 rm(list = ls())
 library(tidyverse)
 library(magrittr)
 
 # before running the loop, the selected variable combinations must be retrieved
 
 # these data are stored at C:\Users\rgorm\Documents\non-AGLDT_trees\selected_var_val_pairs
 
 
 simplex_vars.tib <- readRDS(file = "simplex_5pc.RDS")
 two_tuple_vars.tib <- readRDS(file = "two_plex_5pc.RDS")
 three_tuple_vars.tib <- readRDS(file = "three_plex_5pc.RDS")

 
 
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
 
 working.df <- readRDS("online_ref_by_work.RDS")
 
 #file_name <- gsub(".RDS", "", files.v[i]) # add file name to each row of df
 
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
 

 saveRDS(target.df,file = "online_binary_valued.RDS")
 