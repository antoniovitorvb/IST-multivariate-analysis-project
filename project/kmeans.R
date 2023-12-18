source('tidydata.R')

library(dplyr)
library(cluster)

# UNSUPERVISED LEARNING - K-MEANS CLUSTERING

fitdata_prepared <- fitdata %>%
     select(-booking_id, -attended) %>% 
     mutate_at(
          vars(months_as_member,weight,days_before),
          scale) %>%  # Scale numerical variables
     mutate(
          day_of_week = as.numeric(day_of_week),  # Convert factors to numeric
          time = as.numeric(time),
          category = as.numeric(category)
     )

KM_obj <- kmeans(fitdata_prepared, centers = 2)

fitdata$cluster <- KM_obj$cluster - 1

paste("K-means accuracy =", sum(fitdata$attended == fitdata$cluster) / length(fitdata$attended))


# Agglomerative Nesting (Hierarchical Clustering)

library(cluster)

best_agnes <- function(data, y) {
     metrics <- c('euclidean', 'manhattan')
     methods <- c('single', 'complete', 'average')
     
     results <- expand.grid(
          metric = metrics, method = methods, 
          pred_acc = NA_real_)
     
     for (i in seq_len(nrow(results))) {
          metric <- results$metric[i]
          method <- results$method[i]
          
          agnes_result <- agnes(data, metric = metric, method = method)
          
          cluster_result <- cutree(as.hclust(agnes_result), k = 2) - 1
          results$pred_acc[i] <- sum(y == cluster_result) / length(y)
     }
     
     best_combination <- results[which.max(results$pred_acc), ]
     
     return(best_combination)
}

best_combination <- best_agnes(fitdata_prepared, fitdata$attended)
print(best_combination)

# ==========

best_agnes <- agnes(
     fitdata_prepared,
     metric = best_combination$metric, 
     method = best_combination$method
)
fitdata$best_agnes <- cutree(best_agnes, k = 2) - 1

paste("AgNes accuracy =", 
      sum(fitdata$attended == fitdata$best_agnes) / length(fitdata$attended))

# plot(best_agnes, which.plots = 2, main = "Dendrogram of Hierarchical Clustering")

library(ggplot2)
library(gridExtra)

g1 <- ggplot(
     fitdata, 
     aes(
          x = days_before,
          y = weight,
          color = as.factor(cluster)
     )
) +
     geom_point(alpha = 0.5) +  # Use alpha to adjust point transparency if data points overlap
     labs(
          color = "Will attend?",
          x = "Days Before",
          y = "Weight") +
     theme_minimal() +
     ggtitle("K-means Clustering")

g2 <- ggplot(
     fitdata, 
     aes(
          x = months_as_member,
          y = weight,
          color = as.factor(cluster)
     )
) +
     geom_point(alpha = 0.5) +  # Use alpha to adjust point transparency if data points overlap
     labs(
          color = "Will attend?",
          x = "Months as Member",
          y = "Weight") +
     theme_minimal()
     # ggtitle("K-means Clustering of Fitness Data")

g3 <- ggplot(
     fitdata, 
     aes(
          x = days_before,
          y = weight,
          color = as.factor(best_agnes)
     )
) +
     geom_point(alpha = 0.5) +  # Use alpha to adjust point transparency if data points overlap
     labs(
          color = "Will attend?",
          x = "Days Before",
          y = "Weight") +
     theme_minimal() +
     ggtitle("Agglomerative Nesting")

g4 <- ggplot(
     fitdata, 
     aes(
          x = months_as_member,
          y = weight,
          color = as.factor(best_agnes)
     )
) +
     geom_point(alpha = 0.5) +  # Use alpha to adjust point transparency if data points overlap
     labs(
          color = "Will attend?",
          x = "Months as Member",
          y = "Weight") +
     theme_minimal()
     # ggtitle("Agglomerative Nesting of Fitness Data")

# grid.arrange(g1, g3, g2, g4,
#              ncol = 2, nrow = 2)

g5 <- ggplot(
     fitdata, 
     aes(
          x = days_before,
          y = weight,
          color = as.factor(attended)
     )
) +
     geom_point(alpha = 0.5) +  # Use alpha to adjust point transparency if data points overlap
     labs(
          color = "attended",
          x = "Days Before",
          y = "Weight") +
     theme_minimal() +
     ggtitle("Attendancy of Fitness Club")

g6 <- ggplot(
     fitdata, 
     aes(
          x = months_as_member,
          y = weight,
          color = as.factor(attended)
     )
) +
     geom_point(alpha = 0.5) +  # Use alpha to adjust point transparency if data points overlap
     labs(
          color = "attended",
          x = "Months as Member",
          y = "Weight") +
     theme_minimal()

grid.arrange(g5, g1, g3,
             g6, g2, g4,
             ncol = 3, nrow = 2)