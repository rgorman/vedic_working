rm(list = ls())
require(tidyverse)
require(stylo)
require(caret)

books.v <- combined.df$Book_Id %>%
  unique()

chunk.tags.v <- combined.df$Book_and_Hymn %>%
  sub("^(([^_]*_){1}[^_]*).*", "\\1", .) %>%
  unique()

sizes.v <- NULL
for (i in 1:10) {
  y <- combined.df %>%
    filter(Book_Id == books.v[i]) %>%
    nrow()
  sizes.v <- c(sizes.v, y)
  
}
names(sizes.v) <- 1:10

chunk.size.v <- 1000
chunk.counts.v <-  (sizes.v/chunk.size.v) %>%
  floor()

chunk.counts.v %>% sum()
fold.count.v <- 10

col.index <- which(str_detect(colnames(combined.df), "_Is_"))

###
conf.euclid.l <- vector(mode = "list", 100)
conf.eder.l <- vector(mode = "list", 100)
#conf.hclust.l <- vector(mode = "list", 100)

for (spin in 1:100) {
  
  
  holder.l <- vector(mode = "list", fold.count.v)
  
  word.count.v <- NULL
  truth.holder.v <- NULL
  
  for (i in seq_along(books.v)) {
    x <- combined.df %>%
      filter(Book_Id == books.v[i])
    
    x <- x[, col.index]
    
    x <-  x[1:(chunk.counts.v[i]*chunk.size.v), ]
    
    
    y <- createFolds(1:(chunk.counts.v[i]*chunk.size.v), k = chunk.counts.v[i])
    
    token.counts.v <- lengths(y)
    
    n1 <- paste0(chunk.tags.v[i], "_", "chunk_", 1:chunk.counts.v[i])
    # n1 <-  paste0(n1, "_", token.counts.v)
    
    word.count.v <- c(word.count.v, n1)
    
    fold.holder.l <- vector(mode = "list" , chunk.counts.v[i])
    for (j in seq_len(chunk.counts.v[i])) {
      smp <- x[y[[j]], ]
      nr <- nrow(smp)
      smp <- smp %>%
        colSums()
      smp <- smp / nr
      
      fold.holder.l[[j]] <- smp
      
    }
    
    holder.l[[i]] <- fold.holder.l
    print(paste0("completed ", i))
    
    truth.holder.v <- c(truth.holder.v, rep(i, chunk.counts.v[i]) )
    
  }
  
  
  result.m <- do.call(bind_rows, holder.l)
  result.m <- as.matrix(result.m)
  rownames(result.m) <- word.count.v
  
  d.euclid <- dist.eder(result.m)
  # d.cosine <- dist.cosine(result.m)
  #d.eders <- dist.eder(result.m)
  
  d.euclid.m <- d.euclid %>% as.matrix()
  guess.holder.l <- vector(mode = "list", nrow(d.euclid.m))
  
  for (iter in seq_len(nrow(d.euclid.m))) {
    
    r <- d.euclid.m[iter, -iter]
    m <- min(r)
    
    
    guess.holder.l[[iter]] <-  which(r == m) %>% names()
    
  }
  
  conf.euclid.l[[spin]] <- guess.holder.l
  
  print(paste0("finished interation ", spin))
}


####

d.euclid.m <- d.euclid %>% as.matrix()

r <- d.euclid.m[1, -1]
m <- min(r)
which(r == min(r)) %>% names()
######

guess.holder.l <- vector(mode = "list", nrow(d.euclid.m))

for (iter in seq_len(nrow(d.euclid.m))) {
  
  r <- d.euclid.m[iter, -iter]
  m <- min(r)
 
  
  guess.holder.l[[iter]] <-  which(r == m) %>% names()
  
}

#########
g.v <- unlist(conf.euclid.l)
g.v <- g.v %>% gsub("_chunk.*", "", .)
truth.v <- rownames(d.euclid.m) %>% 
  gsub("_chunk.*", "", .)

truth.v <-  rep(truth.v, 100)

a <- confusionMatrix(as.factor(g.v), as.factor(truth.v))

a$table
dist.entropy()
