rm(list = ls())
require(tidyverse)
require(stylo)
require(caret)
require(cluster)

#### bow clustering


books.v <- combined.df$Book_Id %>%
  unique()

chunk.tags.v <- combined.df$Book_and_Hymn %>%
  sub("^(([^_]*_){1}[^_]*).*", "\\1", .) %>%
  unique()

books.v <- books.v %>%
  sub("^(([^_]*_){1}[^_]*).*", "\\1", .)

fold.count.v <- 10
holder.l <- vector(mode = "list", fold.count.v)
col.index <- which(str_detect(colnames(combined.df), "_Is_"))
word.count.v <- NULL
for (i in seq_along(books.v)) {
  x <- combined.df %>%
    filter(Book_Id == books.v[i])
  
  x <- x[, col.index]
  
  y <- createFolds(1:nrow(x), fold.count.v)
  
  token.counts.v <- lengths(y)
  
  n1 <- paste0(chunk.tags.v[i], "_", "chunk_", 1:fold.count.v)
  n1 <-  paste0(n1, "_", token.counts.v)
  
  word.count.v <- c(word.count.v, n1)
  
  fold.holder.l <- vector(mode = "list" , fold.count.v)
  for (j in seq_len(fold.count.v)) {
    smp <- x[y[[j]], ]
    nr <- nrow(smp)
    smp <- smp %>%
      colSums()
    smp <- smp / nr
    
    fold.holder.l[[j]] <- smp
    
  }
  
  holder.l[[i]] <- fold.holder.l
  print(paste0("completed ", i))
  
}

result.m <- do.call(bind_rows, holder.l)
result.m <- as.matrix(result.m)
rownames(result.m) <- word.count.v

d.euclid <- dist(result.m)
d.cosine <- dist.cosine(result.m)
d.eders <- dist.eder(result.m)

cl_1 <- hclust(d.euclid, method = "ward.D2")
cl_2 <- hclust(d.cosine, method = "ward.D2")
cl_3 <- hclust(d.eders, method = "ward.D2")

plot(cl_1)
plot(cl_2)
plot(cl_3)

x <-cutree(cl_1, k = 10)
which(x[1:10] == 1) %>% length()

### info for loop testing clustering accuracy

sizes.v <- NULL
for (i in 1:10) {
   y <- combined.df %>%
    filter(Book_Id == books.v[i]) %>%
    nrow()
   sizes.v <- c(sizes.v, y)
  
}
names(sizes.v) <- 1:10

col.index <- which(str_detect(colnames(combined.df), "_Is_"))
chunk.size.v <- 1800
chunk.counts.v <-  (sizes.v/chunk.size.v) %>%
  floor()

####
###############################

chunk.size.v <- 100
chunk.counts.v <-  (sizes.v/chunk.size.v) %>%
  floor()

chunk.counts.v %>% sum()

###
conf.euclid.l <- vector(mode = "list", 100)
conf.eder.l <- vector(mode = "list", 100)
conf.hclust.l <- vector(mode = "list", 100)

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
  
  d.euclid <- dist(result.m)
  # d.cosine <- dist.cosine(result.m)
  d.eders <- dist.eder(result.m)
  
  cl_1 <- agnes(d.euclid, method = "ward")
  cl_2 <- agnes(d.eders, method = "ward")
  #cl_3 <- hclust(d.euclid, method = "ward.D2")
  
  ct1 <-cutree(cl_1, k = 10)
  ct2 <-cutree(cl_2, k = 10)
  #ct3 <-cutree(cl_3, k = 10)
  
 conf.euclid.l[[spin]] <-  confusionMatrix(as.factor(ct1), as.factor(truth.holder.v))
 conf.eder.l[[spin]] <- confusionMatrix(as.factor(ct2), as.factor(truth.holder.v))
 #conf.hclust.l[[spin]] <- confusionMatrix(as.factor(ct3), as.factor(truth.holder.v))
 
  
 print(paste0("finished interation ", spin))
}

score.euclid.v <- NULL
score.eder.v <- NULL 
#score.hclust.v <- NULL 

for (i in seq_along(conf.euclid.l)) {
  score.euclid.v <- c(score.euclid.v, conf.euclid.l[[i]]$overall[1])
  score.eder.v <- c(score.eder.v, conf.eder.l[[i]]$overall[1])
  #score.hclust.v <- c(score.hclust.v, conf.hclust.l[[i]]$overall[1])
  
}


score.euclid.v %>%
  summary()

score.eder.v %>%
  summary()



which(score.euclid.v == 1) %>% length()
which(score.eder.v == 1) %>% length()

which(score.euclid.v >= .9) %>% length()
which(score.eder.v >= .9) %>% length()

