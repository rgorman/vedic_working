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
