library(dplyr)

bootstrapping <- function (vec, n = 100, bs_i = 20000) {
     if (!is.numeric(n) || !is.numeric(bs_i)) {
          stop('n and bs_i must be numeric!')
     }
     if (n < 0) n <- 0
     if (bs_i < 0) bs_i <- 20000
     
     if (is.numeric(vec)){
          bs_dist <- numeric(bs_i)
          for (i in 1:bs_i) {
               bs_dist[i] <- sample(vec, length(vec), replace = TRUE) %>% mean()
          }
          return(c(vec, sample(bs_dist, n)))
          
     } else {
          return(c(vec, sample(vec, n, replace = TRUE)))
     }
}

data_bs <- lapply(data, function(column) bootstrapping(column, n = 1000))

data_bs <- as.data.frame(data_bs)

write.csv(data_bs, file = "bootstrapped_data.csv", row.names = FALSE)