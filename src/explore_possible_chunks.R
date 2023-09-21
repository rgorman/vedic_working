require(tidyverse)



x <-  gold.df$sentence_id %>%
  str_extract( "[^_]*")

## _[^_]*

gold.df$sentence_id %>% unique()

x %>% unique()

online.df <- add_column(online.df, full_ref = gold.df$sentence_id, .before = TRUE)
online.df <- add_column(online.df, book_ref = x, .before = TRUE)
online.df <- add_column(online.df, work_ref = x, .before = TRUE)


books.v <- x %>% unique()
tokenCount.l <- vector(mode = "list", length(books.v))

for (i in seq_along(tokenCount.l)) {
  
  a <- online.df %>%
    filter(work_ref == books.v[i]) %>%
    nrow()
  
  tokenCount.l[[i]] <- a
  
}


unlist(tokenCount.l) %>% sum()
