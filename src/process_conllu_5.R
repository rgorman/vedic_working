rm(list = ls())
require(tidyverse)
require(stylo)
require(caret)


book.v <- working.df$Book_Id %>% unique()
holder.l <- vector(mode = "list", length(book.v))
nomina.v <- NULL

for (i in seq_along(book.v)) {
  x <- working.df %>%
    filter(Book_Id == book.v[i])
  x <- x[, which(str_detect(colnames(x), "_Is_")== TRUE)]
  
  flds <- createFolds(1:nrow(x), 3)
  
  fold.holder.l <- vector(mode = "list", 3)
  for (j in seq_along(flds)) {
    
    fold.holder.l[[j]] <- x[flds[[j]], ] %>%
      colSums() 
    
    fold.holder.l[[j]] <-   fold.holder.l[[j]] / length(flds[[j]] ) 
    
  }
  
  a <- paste0("book_", i, "_sample_", 1:3)
  nomina.v <- c(nomina.v, a)
  
 holder.l[[i]] <- fold.holder.l
  
}

result.df <- do.call(bind_rows, holder.l)


rownames(result.df) <- nomina.v


dd <- dist(result.df)
cl <- hclust(dd)
plot(cl)
dc <- dist.cosine(as.matrix(result.df))

cl2 <- hclust(dc)
plot(cl2)
