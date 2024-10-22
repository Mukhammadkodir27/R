# Load necessary libraries
library(cluster)   # For k-means and clustering
library(mclust)    # For Adjusted Rand Index
library(ggplot2)   # For plotting
library(mgcv)      # For GAM

# Function to create artificial datasets
generate_data <- function(n, p, k, noise_p = 0.1, separation = 1.5) {
  # Generate cluster centers
  centers <- matrix(runif(k * p, -5, 5), nrow = k)
  
  # Generate data based on cluster centers
  data <- do.call(rbind, lapply(1:k, function(i) {
    matrix(rnorm(n * p, mean = centers[i, ]), ncol = p)
  }))
  
  # Calculate the number of noise variables based on proportion noise_p
  noise_vars <- as.integer(noise_p * p)
  
  if (noise_vars > 0) {
    # Add noise variables if needed
    noise <- matrix(rnorm(nrow(data) * noise_vars), nrow = nrow(data), ncol = noise_vars)
    
    # Check if dimensions match to avoid cbind error
    if (nrow(data) == nrow(noise)) {
      data <- cbind(data, noise)
    } else {
      stop("Error: The number of rows in data and noise must match.")
    }
  }
  
  list(data = data, true_clusters = rep(1:k, each = n))
}

# Example of data generation
set.seed(123)
generated <- generate_data(n = 50, p = 5, k = 3, noise_p = 0.2)

# Convert generated data to data frame
generated_data <- as.data.frame(generated$data)

# Add true cluster labels to the data frame
generated_data$cluster <- as.factor(generated$true_clusters)

# Visualize the generated data
ggplot(generated_data, aes(x = V1, y = V2, color = cluster)) +
  geom_point() +
  ggtitle("Generated Data with Noise") +
  theme_minimal()



# Perform k-means clustering function
kmeans_clustering <- function(data, k) {
  kmeans(data, centers = k, nstart = 20)  # Multiple initializations to avoid local optima
}

# Run k-means on the generated data
kmeans_result <- kmeans_clustering(generated$data, 3)

# Convert the generated data to a data frame for plotting
cluster_plot <- as.data.frame(generated$data)

# Add the cluster assignment from k-means to the data frame
cluster_plot$cluster <- factor(kmeans_result$cluster)

# Visualize the k-means clustering results on the first two dimensions
ggplot(cluster_plot, aes(x = V1, y = V2, color = cluster)) +
  geom_point() +
  ggtitle("K-means Clustering Results") +
  theme_minimal()






# Evaluate performance using Adjusted Rand Index
ari <- adjustedRandIndex(generated$true_clusters, kmeans_result$cluster)

# Print ARI result
print(paste("Adjusted Rand Index: ", round(ari, 3)))

# Plot ARI across multiple iterations with different sample sizes
ari_values <- sapply(seq(10, 100, by = 10), function(n) {
  data_gen <- generate_data(n = n, p = 5, k = 3)
  kmeans_res <- kmeans_clustering(data_gen$data, 3)
  adjustedRandIndex(data_gen$true_clusters, kmeans_res$cluster)
})

# Plot ARI
plot(seq(10, 100, by = 10), ari_values, type = "b", col = "blue", pch = 19,
     xlab = "Sample Size", ylab = "Adjusted Rand Index", main = "Clustering Performance vs Sample Size")





# Fit a GAM to analyze the relationship between sample size and ARI
sample_sizes <- seq(10, 100, by = 10)
gam_model <- gam(ari_values ~ s(sample_sizes))

# Predict values using the fitted GAM model
predicted_ari <- predict(gam_model)

# Plot GAM results
plot(sample_sizes, ari_values, type = "p", pch = 19, col = "blue", 
     xlab = "Sample Size", ylab = "ARI", main = "GAM Fit to ARI vs Sample Size")
lines(sample_sizes, predicted_ari, col = "red", lwd = 2)







# Minimum sample size recommendation based on GAM
optimal_sample_size <- sample_sizes[which.max(predicted_ari)]
cat("Recommended minimum sample size based on GAM analysis:", optimal_sample_size, "\n")

# Add a guideline to the plot
plot(sample_sizes, ari_values, type = "p", pch = 19, col = "blue", 
     xlab = "Sample Size", ylab = "ARI", main = "GAM Fit to ARI vs Sample Size")
lines(sample_sizes, predicted_ari, col = "red", lwd = 2)
abline(v = optimal_sample_size, col = "green", lty = 2)  # Add vertical line for optimal sample size




plot(sample_sizes, ari_values, type = "p", pch = 19, col = "blue", 
     xlab = "Sample Size", ylab = "ARI", main = "GAM Fit to ARI vs Sample Size")
lines(sample_sizes, predicted_ari, col = "red", lwd = 2)
abline(v = optimal_sample_size, col = "green", lty = 2, lwd = 3)  # Add vertical line for optimal sample size

