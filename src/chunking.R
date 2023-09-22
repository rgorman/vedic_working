## script to cluster chunks

require(caret)

colnames(working.df)

works.v <- working.df$work_ref %>% unique()

tokenCount.l <- vector(mode = "list", length(works.v))

for (i in seq_along(tokenCount.l)) {
  
  a <- working.df %>%
    filter(work_ref == works.v[i]) %>%
    nrow()
  
  tokenCount.l[[i]] <- a
  
}


unlist(tokenCount.l) %>% sum()

a <- unlist(tokenCount.l) / 200

a <- floor(a)


x <- working.df %>%
  filter(work_ref == works.v[[i]])

z <- createFolds(x$work_ref, 2)

df.holder.l <- vector(mode = "list", length(works.v))
for (i in seq_along(works.v)) {
  x <- working.df %>%
    filter(work_ref == works.v[[i]])
  
  index.v <- c(which(str_detect(colnames(x), "_Is_") == TRUE),
  which(str_detect(colnames(x), "_Are_") == TRUE) )
  
  x <- x[, index.v]
  
  folds.l <- createFolds(x$parent_Number_Is_Sing, a[i])
  
  fold.holder.l <- vector(mode = "list", length(folds.l))
  for (n in seq_along(folds.l)) {
    
    b <- x[folds.l[[n]], ]
    b <- colSums(b) / nrow(b)
    fold.holder.l[[n]] <- b
    
  }
  
  chunk.df <- do.call(bind_rows, fold.holder.l)
  chunk.df <- add_column(chunk.df, chunk_id = paste0(works.v[i], "_", 1:nrow(chunk.df)), .before = TRUE )
  df.holder.l[[i]] <- chunk.df
  
}

results.df <- do.call(bind_rows, df.holder.l)

saveRDS(results.df, file = "sample1.RDS")
