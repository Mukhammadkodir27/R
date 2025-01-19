#1.Data_loading_and_preprocessing_and_More_visualization_on_data_spreadness###############################
if (!require("pacman")) install.packages("pacman")
pacman::p_load("yaml", "plotly", "ggplot2", "Rtsne", "htmlwidgets", "stats", "ggcorrplot", 
               "readxl", "knitr", "clustertend", "hopkins", "factoextra", "GGally", "mclust",
               "gridExtra", "dendextend", "cluster", "fpc", "clustMixType", "reshape2",
               "MASS", "FactoMineR", "BiocManager", "gplots", "pheatmap", "tidyr", "umap",
               "fpc", "flexclust", "cluster", "ClusterR", "NbClust", "DescTools", "pastecs",
               "corrplot", "psych", "dbscan")

# Set working directory and load dataset
setwd("C:\\Users\\khali\\Desktop")
Dataset <- read_excel("2011-2021_Data_Extract_From_World_Development_Indicators.xlsx", 
                      sheet = "2017-21")

dim(Dataset)
head(Dataset)
str(Dataset)
summary(Dataset)
#View(Dataset)
options(scipen=999)
#columns containing missing values
colSums(is.na(Dataset))

#visualizing the missing values per column in bar chart, histogram and scatter plot for better visibility of missing data

# Count of missing values per column
missing_counts <- colSums(is.na(Dataset))
missing_counts <- missing_counts[missing_counts > 0]

# Bar chart for missing values per column
if (length(missing_counts) > 0) {
  bar_chart <- ggplot(data.frame(Columns = names(missing_counts), Missing = missing_counts), aes(x = reorder(Columns, Missing), y = Missing)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Missing Values per Column", x = "Columns", y = "Number of Missing Values") +
    theme_minimal()
} else {
  bar_chart <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "No Missing Values", size = 6, color = "green", hjust = 0.5, vjust = 0.5) +
    theme_void() +
    labs(title = "Missing Values per Column")
}

# # Scatter plot of missing values per column (adjusted)
# missing_indices <- which(is.na(Dataset), arr.ind = TRUE)
# scatter_data <- data.frame(
#   Rows = missing_indices[, 1],  # Row indices
#   Columns = factor(missing_indices[, 2], levels = 1:ncol(Dataset))  # Column indices as factors for categorical axis
# )

# scatter_plot <- ggplot(scatter_data, aes(x = Columns, y = Rows)) +
#   geom_point(color = "red", size = 1.5) +
#   labs(title = "Scatter Plot of Missing Values", x = "Columns", y = "Rows") +
#   theme_minimal()

# Histogram of missing values per row
gg_plot <- ggplot(data.frame(missing_per_row = rowSums(is.na(Dataset))), aes(x = missing_per_row)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Histogram of Missing Values per Row", x = "Number of Missing Values", y = "Frequency")

# Arranging all visualizations
grid.arrange(gg_plot, bar_chart)


##Data Cleaning Part
#started with filling NA with the mean 
colSums(is.na(Dataset))
dim(Dataset)
Dataset[, 5:19] <- lapply(Dataset[, 5:19], function(x) { ifelse(is.na(x), mean(x, na.rm = TRUE), x)})

#double checking
colSums(is.na(Dataset))

#clearly found a column without any values, so to drop  it 
blank_cols <- apply(Dataset, 2, function(col) all(is.na(col) | col == "sheet1"))
Dataset<-Dataset[,!blank_cols]
colSums(is.na(Dataset))

#last inspection of the dataset

# Count of missing values per column
missing_counts <- colSums(is.na(Dataset))
missing_counts <- missing_counts[missing_counts > 0]

# 1. Bar Chart for Missing Values per Column
if (length(missing_counts) > 0) {
  bar_chart <- ggplot(data.frame(Columns = names(missing_counts), Missing = missing_counts), aes(x = reorder(Columns, Missing), y = Missing)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Missing Values per Column", x = "Columns", y = "Number of Missing Values") +
    theme_minimal()
} else {
  bar_chart <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "No Missing Values", size = 6, color = "black", hjust = 0.5, vjust = 0.5) +
    theme_void() +
    labs(title = "Missing Values per Column")
}

# 2. Scatter Plot of Missing Values per Column
missing_indices <- which(is.na(Dataset), arr.ind = TRUE)
scatter_data <- data.frame(
  Rows = missing_indices[, 1],  # Row indices
  Columns = factor(missing_indices[, 2], levels = 1:ncol(Dataset))  # Column indices as factors for categorical axis
)

scatter_plot <- ggplot(scatter_data, aes(x = Columns, y = Rows)) +
  geom_point(color = "red", size = 1.5) +
  labs(title = "Scatter Plot of Missing Values", x = "Columns", y = "Rows") +
  theme_minimal()

# 3. Histogram of Missing Values per Row
gg_plot <- ggplot(data.frame(missing_per_row = rowSums(is.na(Dataset))), aes(x = missing_per_row)) +
  geom_histogram(binwidth = 1, fill = "brown", color = "white") +
  labs(title = "Missing Values per Row", x = "Number of Missing Values", y = "Frequency")

# 4. Proportion of Missing Values in the Dataset (Pie Chart)
missing_proportion <- sum(is.na(Dataset)) / (nrow(Dataset) * ncol(Dataset)) * 100
pie_data <- data.frame(
  Category = c("Missing", "Non-Missing"),
  Value = c(sum(is.na(Dataset)), (nrow(Dataset) * ncol(Dataset)) - sum(is.na(Dataset)))
)

pie_chart <- ggplot(pie_data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = paste("Proportion of Missing Values: ", round(missing_proportion, 2), "%")) +
  theme_void() +
  scale_fill_manual(values = c("black", "beige"))

# Arranging all visualizations into a single view
grid.arrange(gg_plot, bar_chart, scatter_plot, pie_chart, ncol = 2)


#selecting only integer columns to start the procedure
summary(Dataset)
str(Dataset)
dim(Dataset)
measures<-Dataset[5:18]

#observing distribution of all columns in the dataset

# Select numeric columns
numeric_columns <- sapply(measures, is.numeric)

# Melt only numeric columns
melted_data_before <- melt(measures[, numeric_columns], variable.name = "Column", value.name = "Value")

# Check the melted data (confirm it has 'Column' and 'Value' columns)
head(melted_data_before)

# Plot distributions (before standardization)
distribution_plot_before <- ggplot(melted_data_before, aes(x = Value)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Column, scales = "free", ncol = 3) +
  labs(title = "Distribution of Numeric Columns (Before Standardization)", x = "Value", y = "Frequency") +
  theme_minimal()

# Display the before standardization plot
grid.arrange(distribution_plot_before, ncol = 1)
#View(measures)

#observing the outliers in Boxplot
numerical_columns <- sapply(measures, is.numeric)
numerical_data <- measures[, numerical_columns]

long_numerical_data <- gather(numerical_data, key = "variable", value = "value")

ggplot(long_numerical_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot to detect outliers")
summary(measures$`GDP (current US$)`)
colSums(is.na(measures))
#since we detected outliers in GDP, GNI and net trade first log transform(there are non positive numbers converted to NaN's I'll handle them by adding a constant value to make them positive)
measures$log_GDP <- log(measures$`GDP (current US$)`)
measures$log_Trade <- log(measures$`Net trade in goods and services (BoP, current US$)`)
summary(measures$`Net trade in goods and services (BoP, current US$)`)
sum(measures$`Net trade in goods and services (BoP, current US$)` <= 0)
measures$log_Trade <- log(measures$`Net trade in goods and services (BoP, current US$)` + abs(min(measures$`Net trade in goods and services (BoP, current US$)`, na.rm = TRUE)) + 1)
measures$log_GNI <- log(measures$`GNI per capita, Atlas method (current US$)`)
#checking if i've got rid of NaN's and all fine?
summary(measures$log_Trade)
sum(is.na(measures$log_Trade))

#This will cap all values in the GDP (current US$) column at the 95th percentile 
#and store them in a new column capped_GDP.
#I will excluse the original GDP from the dataset
dim(measures)
str(measures)
measures<-measures[,2:17]
measures$`Net trade in goods and services (BoP, current US$)`<-NULL
measures$`GNI per capita, Atlas method (current US$)`<-NULL
#View(measures)
numerical_columns <- sapply(measures, is.numeric)
numerical_data <- measures[, numerical_columns]

long_numerical_data <- gather(numerical_data, key = "variable", value = "value")

ggplot(long_numerical_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot to detect outliers")

colSums(is.na(measures))
boxplot(measures$log_GDP, main = "Boxplot of Log-Transformed GDP")
summary(measures$log_GDP)

# Plot the heatmap
# Calculate the correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")

# Reshape the correlation matrix into a long format
long_cor_data <- melt(cor_matrix)

# Plot the heatmap
ggplot(data = long_cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap")

#Each cell in the heatmap represents the Pearson correlation coefficient between two variables,
#with the color intensity indicating the strength and direction of the correlation:
#Red indicates a strong positive correlation, meaning that as one variable increases, the other tends to increase as well.
#Blue indicates a strong negative correlation, meaning that as one variable increases, the other tends to decrease.
#Light colors near white indicate weak or no correlation between the variables.
#Exports of goods and services (% of GDP) vs. GDP (current US$): There is a very strong positive correlation (0.97), indicating that higher exports are strongly associated with higher GDP.
#GNI per capita (Atlas method) vs. Domestic credit provided by the financial sector: There is a moderate positive correlation, suggesting that countries with higher GNI per capita tend to have higher domestic credit provided by the financial sector.
#Government expenditure on education vs. Tax revenue: These variables have a positive correlation, indicating that higher tax revenues are often associated with higher government spending on education.
#Inflation (consumer prices) vs. Net trade in goods and services: There is a negative correlation, indicating that higher inflation is generally associated with lower net trade in goods and services.

#Creating a pairplot using ggpairs
ggpairs(numerical_data, title = "Pairplot of Numerical Features")

#Shows the data in its original scale, which can provide more intuitive insights into relationships and distributions.
#Helps identify patterns, correlations, or clusters in the raw data.

#Z-Standardizing the Dataset(measures)
lapply(measures, class)
measures_z<-as.data.frame(lapply(measures, scale))
#2.Clustering_prof_approach##################################################################
# K-Means
# let's try with 5 clusters
kmeans_result <- kmeans(measures_z, centers = 5)

# check sizes of these clusters 
kmeans_result$size

# what are centers of these clusters?
kmeans_result$centers

#creating a new column with cluster assignment 
measures$cluster <- kmeans_result$cluster

head(measures)
str(measures)

# general characteristics of clusters

# Mean of 'Net trade in goods and services (BoP, current US$)' by cluster
aggregate(data = measures, log_Trade ~ cluster, mean)

# Mean of 'Labor force participation rate, total (% of total population ages 15+)' by cluster
aggregate(data = measures, log_GDP ~ cluster, mean)

# Mean of 'Employment in agriculture (% of total employment)' by cluster
aggregate(data = measures, log_GNI ~ cluster, mean)

# more info about the output
attributes(kmeans_result)

# change the number of cluster to 3 and narrow to age and friends variables to make transparent plots
measures_reduced <- measures[9:12]
measures_reduced_z <- as.data.frame(lapply(measures_reduced, scale))
measures_reduced_km3 <- kmeans(measures_reduced_z, 4)

# simple plots
plot(measures_reduced_z, col = measures_reduced_km3$cluster, pch=".", cex=4)
points(measures_reduced_km3$centers, col = 1:5, pch = 8, cex=2, lwd=2)

#different plots
fviz_cluster(list(data=measures_reduced_z, cluster=measures_reduced_km3$cluster), 
             ellipse.type="norm", geom="point", stand=FALSE, palette="jco", ggtheme=theme_classic()) 


# Hierarchical clustering dendrogram for visualizing relationships
# Perform hierarchical clustering
# Perform hierarchical clustering
hc <- hclust(dist(measures_reduced_z), method = "ward.D2")

# Prune the dendrogram by cutting it into a specific number of clusters
clusters <- cutree(hc, k = 4)  # Adjust 'k' as needed

# Plot a pruned dendrogram
par(mfrow = c(1, 1))  # Reset layout to one plot per page
par(cex = 0.7)        # Reduce text size for smaller plots

# Convert hclust object to dendrogram and cut at specified clusters
dend <- as.dendrogram(hc)

# Plot the pruned dendrogram with cluster rectangles
plot(dend, main = "Simplified Dendrogram of Clusters", xlab = "", ylab = "Height")
rect.hclust(hc, k = 4, border = "blue")  # Highlight the clusters


# Parallel coordinate plot for cluster characteristics
ggparcoord(data = cbind(measures_reduced_z, cluster = as.factor(measures_reduced_km3$cluster)), 
           columns = 1:(ncol(measures_reduced_z)), groupColumn = "cluster", 
           scale = "uniminmax", showPoints = TRUE, 
           title = "Parallel Coordinate Plot of Clusters", 
           alphaLines = 0.5) + theme_minimal()


# narrow down the scope to compute the silhouette width faster (run the broader dataset on your own)

measures_reduced_km4 <- kmeans(measures_reduced_z, 3)

# silhouette plot
sil<-silhouette(measures_reduced_km4$cluster, dist(measures_reduced_z))
fviz_silhouette(sil)

# alternative commands for k-means
k1 <- kmeans(measures_reduced_z, centers = 3, nstart = 25) # Replace `cclust`

plot(measures_reduced_z, col = k1$cluster, pch = 16, main = "K-means Clustering")
points(k1$centers, col = 1:3, pch = 8, cex = 2, lwd = 2)

summary(k1)

attributes(k1)

print(k1$centers)



# yet another package ready to be used - factoextra::
# you can use the eclust () function from the factoextra :: package
# it allows for clusters using k-means, PAM, CLARA etc. methods 
# using Euclidean, Manhattan, Canberra, Minkowski distance etc.

# Elbow Method
elbow_clus <- fviz_nbclust(measures_reduced_z, kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Clusters")
plot(elbow_clus)

# Silhouette Method
sil_clus <- fviz_nbclust(measures_reduced_z, kmeans, method = "silhouette") +
  ggtitle("Silhouette Method for Optimal Clusters")
plot(sil_clus)

# most of steps from above easy to execute
# play with this part on your own basing on codes from above


#PAM clustering 
# Perform PAM clustering with 3 clusters
c1 <- pam(measures_reduced_z, 3)

# Print the result
print(c1)

# Display the medoids
print(c1$medoids)

# Display the clustering result
print(head(c1$clustering))

# Summary of the PAM clustering
#summary(c1)

# Silhouette information
sil <- silhouette(c1)

# Silhouette plot for the PAM clustering
fviz_silhouette(sil) + 
  ggtitle("Silhouette Plot for PAM Clustering") +
  theme_minimal()

# Visualizing clusters with factoextra
fviz_cluster(c1, geom = "point", ellipse.type = "convex") + 
  ggtitle("Cluster Plot with PAM Clustering") +
  theme_minimal()



# Better graphics with cluster visualization
fviz_cluster(c1, geom = "point", ellipse.type = "norm") # factoextra:: 
fviz_cluster(c1, geom = "point", ellipse.type = "convex") # factoextra:: 

# Another simple clustering method
pam_cluster_result <- eclust(measures_reduced_z, "pam", k = 3) 
fviz_silhouette(pam_cluster_result)
fviz_cluster(pam_cluster_result) 

# Experiment with different numbers of clusters and distance metrics
pam_manhattan <- eclust(measures_reduced_z, "pam", k = 2, hc_metric = "manhattan") 
fviz_silhouette(pam_manhattan)
fviz_cluster(pam_manhattan) 

# Determine optimal number of clusters using the elbow method
opt_clusters_elbow <- Optimal_Clusters_Medoids(measures_reduced_z, max_clusters = 10, distance_metric = "euclidean", plot_clusters = TRUE)

# Determine optimal number of clusters using the silhouette method
opt_clusters_silhouette <- Optimal_Clusters_Medoids(measures_reduced_z, max_clusters = 10, distance_metric = "euclidean", 
                                                    plot_clusters = TRUE, criterion = "silhouette")

#The highest silhouette score is at 4 clusters with a value of 0.147. 
#This suggests that 4 clusters might be the optimal number for your dataset, 
#as it provides the best-defined cluster separation according to the silhouette method.


#3.Dimention_reduction_applied##########################################################################
#I aim to retain at least 70-95% of the total variance in the dataset.
#Exploratory Data Analysis: Using PCA first to understand variance distribution.
#Clustering and Visualization: Using t-SNE or UMAP for cluster visualization and insights.
#Theoretical Models: Using Factor Analysis if you want interpretable factors.
#etermining a meaningful eps might require experimentation or dimensionality reduction 
#MDS to be used since my dataset is moderately sized


#Inspecting correlation and removing unnecessary

# Compute correlation matrix
cor_matrix <- cor(measures_z)

# View a portion of the matrix
head(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.4, tl.col = "black", order = "hclust", 
         number.cex = 0.4)


# Alternative visualization with ggplot2
# Assuming cor_matrix is already defined

# long_cor_data <- reshape2::melt(cor_matrix)
# 
# ggplot(data = long_cor_data, aes(x = Var1, y = Var2, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1, 1), space = "Lab", 
#                        name = "Correlation") +
#   theme_minimal() + 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#   coord_fixed() +
#   labs(title = "Correlation Heatmap")
# 

# Find highly correlated features (absolute value > 0.8)
threshold <- 0.8
high_corr <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)

# Extract the feature pairs
high_corr_pairs <- data.frame(
  Feature1 = rownames(cor_matrix)[high_corr[, 1]],
  Feature2 = colnames(cor_matrix)[high_corr[, 2]],
  Correlation = cor_matrix[high_corr]
)

# View the pairs of highly correlated features
high_corr_pairs

# Example: Drop one feature from each pair
drop_features <- unique(high_corr_pairs$Feature2)
reduced_data <- measures_reduced_z[, !(colnames(measures_reduced_z) %in% drop_features)]

# Compute correlation matrix for reduced data
cor_matrix_reduced <- cor(reduced_data)
corrplot(cor_matrix_reduced, method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = "black", order = "hclust")



# Compute the correlation matrix
cor_matrix <- cor(measures_reduced_z, use = "pairwise.complete.obs")

# Find highly correlated pairs (|r| > 0.8)
high_cor_pairs <- which(abs(cor_matrix) > 0.8 & lower.tri(cor_matrix), arr.ind = TRUE)

# Display highly correlated feature pairs and their correlations
high_cor_features <- data.frame(
  Feature1 = rownames(cor_matrix)[high_cor_pairs[, 1]],
  Feature2 = colnames(cor_matrix)[high_cor_pairs[, 2]],
  Correlation = cor_matrix[high_cor_pairs]
)
print(high_cor_features)
#This is fine-it simply means that no features in your dataset
#are highly correlated based on your defined threshold.


# Decide on features to drop (manual selection)
# Example: Drop one feature from each highly correlated pair
# Example: drop_features <- c("log_GNI", "Trade (% of GDP)")

# Remove selected features from the dataset
measures_reduced_z_filtered <- measures_reduced_z[, !(names(measures_reduced_z) %in% drop_features)]

#View(measures_reduced_z)
dim(measures_reduced_z)
summary(measures_reduced_z)
head(measures_reduced_z)


pca_result <- prcomp(measures_reduced_z, scale. = TRUE)
summary(pca_result) # To see variance explained
screeplot(pca_result, type = "lines") # Scree plot
reduced_data <- pca_result$x[, 1:4] # Retain the first 5 components

#Purpose: Projects data onto fewer dimensions that maximize variance.
#How Many Dimensions: Use the scree plot or cumulative variance explained plot to decide. Typically, the first few components explain most variance.
#Advantages:
#  Works well with continuous numerical data.
#Fast and interpretable.
#I'll first apply dimension reduction on original dataset

# Define a safe version of Rtsne with error handling
safe_Rtsne <- function(data, dims = 2, perplexity = 30, verbose = TRUE) {
  tryCatch(
    {
      # Run Rtsne
      result <- Rtsne(data, dims = dims, perplexity = perplexity, verbose = verbose)
      return(result)  # Return the result if successful
    },
    error = function(e) {
      # Custom error message
      message("An error occurred while running t-SNE: ", e$message)
      message("Attempting to remove duplicate rows and retry...")
      
      # Remove duplicates and retry
      unique_data <- data[!duplicated(data), ]
      
      if (nrow(unique_data) < 2) {
        stop("Not enough unique rows to perform t-SNE after removing duplicates.")
      }
      
      tryCatch(
        {
          # Retry t-SNE with unique data
          result <- Rtsne(unique_data, dims = dims, perplexity = perplexity, verbose = verbose)
          message("t-SNE completed successfully after removing duplicates.")
          return(result)
        },
        error = function(e2) {
          # Handle second failure
          stop("t-SNE failed even after removing duplicates: ", e2$message)
        }
      )
    }
  )
}

# Apply the safe version of t-SNE
tsne_result <- safe_Rtsne(measures_reduced_z)

# If t-SNE succeeded, plot the result
if (!is.null(tsne_result)) {
  plot(tsne_result$Y, col = clusters, pch = 19, main = "t-SNE Visualization")
}

# Identify duplicate rows
duplicates <- measures_reduced_z[duplicated(measures_reduced_z), ]
print(duplicates)
# Remove duplicate rows
measures_reduced_z_unique <- measures_reduced_z[!duplicated(measures_reduced_z), ]

# Run t-SNE
tsne_result <- Rtsne(measures_reduced_z_unique, dims = 2, perplexity = 30, verbose = TRUE)

# Plot the results
plot(tsne_result$Y, col = "blue", pch = 19, main = "t-SNE Visualization")

#-Distributed Stochastic Neighbor Embedding (t-SNE)
#Purpose: Projects high-dimensional data into 2 or 3 dimensions, preserving local structure.#
#How Many Dimensions: Typically 2-3 (used for visualization).
#Advantages:
#  Great for visualizing clusters.
#Preserves local neighborhood relationships.

# Run UMAP
umap_result <- umap(measures_reduced_z)

# Assuming clusters is from k-means or another clustering algorithm
clusters <- kmeans_result$cluster  # Replace with your clustering result

# Map clusters to colors
cluster_colors <- rainbow(length(unique(clusters)))
color_map <- cluster_colors[clusters]

# Plot UMAP result
plot(umap_result$layout, col = color_map, pch = 19, main = "UMAP Visualization")

#Uniform Manifold Approximation and Projection (UMAP)
#Purpose: Similar to t-SNE but faster and scalable, with better global structure preservation.
#How Many Dimensions: Typically 2-3.
#Advantages:
#  Efficient for clustering tasks.
#Handles large datasets better than t-SNE.

fa_result <- fa(measures_reduced_z, nfactors = 2, rotate = "varimax")
print(fa_result)
reduced_data <- fa_result$scores


#Factor Analysis
#Purpose: Reduces data based on underlying latent factors.
#How Many Dimensions: Decide based on eigenvalues >1 or explained variance.
#dvantages:
#  Useful if the data has an underlying theoretical structure.



#MDS
distance_matrix <- dist(measures_z, method = "euclidean")
mds_result <- cmdscale(distance_matrix, k = 2) # 2D output
plot(mds_result, col = clusters, pch = 19, main = "MDS Visualization")
# Add a small positive value to all distances to handle zeros
epsilon <- 1e-6
distance_matrix[distance_matrix == 0] <- epsilon
# Use cmdscale for metric MDS
mds_result <- cmdscale(distance_matrix, k = 2)

# Plot the MDS result
plot(mds_result, col = clusters, pch = 19, main = "MDS Visualization")

# Now run isoMDS
nonmetric_mds <- isoMDS(distance_matrix, k = 2)
# Interpretation of results here Initial value: The initial value of 31.571622 represents 
# the starting configuration's stress value (the measure of how well the initial configuration of 
# points fits the dissimilarity matrix).


plot(nonmetric_mds$points, col = clusters, pch = 19, main = "Non-metric MDS")

#Advantages of MDS
#Preserves pairwise relationships: Especially useful for datasets where distances are meaningful.
#Works with non-Euclidean metrics: You can specify alternative dissimilarity measures like Manhattan or Minkowski.
#Flexible for visualization: Produces interpretable 2D/3D plots.



#kNNdistplot(measures_reduced_z, k = 4) # Choose k = minPts - 1
#abline(h = 0.5, col = "red", lty = 2)  # Replace 0.5 with your chosen value for eps
#db <- dbscan(measures_reduced_z, eps = 0.5, minPts = 4)
#plot(measures_reduced_z, col = db$cluster + 1, pch = 19, main = "DBSCAN Clustering")
#fviz_cluster(db, measures_reduced_z, geom = "point", stand = FALSE)
#table(db$cluster)
#sum(db$cluster == 0) # Cluster 0 corresponds to noise

#The result of sum(db$cluster == 0) is 120, confirming that 120 points are classified as "noise" (cluster 0). In DBSCAN, noise refers to points that do not fit well into any cluster due to low density or outlier characteristics.


#Advantages of DBSCAN
#Noise Detection: Clearly identifies and separates outliers.
#Cluster Flexibility: Handles clusters of arbitrary shape and varying density.
#Minimal Parameters: Requires only eps and minPts.


#comparison of all techniques

# Assuming measures_reduced_z is scaled and cleaned
data <- measures_reduced_z

# 1. PCA
pca <- prcomp(data, center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca$x[, 1:2]) # First 2 PCs
fviz_cluster(list(data = pca_data, cluster = kmeans(pca_data, 3)$cluster), 
             main = "PCA")

# 2. t-SNE
# Define a safe wrapper for Rtsne
safe_Rtsne <- function(data, dims = 2, perplexity = 30, verbose = TRUE) {
  tryCatch(
    {
      # Run t-SNE
      tsne_result <- Rtsne(data, dims = dims, perplexity = perplexity, verbose = verbose)
      return(tsne_result)  # Return the result if successful
    },
    error = function(e) {
      # Check if the error is related to duplicates
      if (grepl("Remove duplicates", e$message)) {
        message("Error detected: ", e$message)
        message("Attempting to remove duplicate rows and retry...")
        
        # Remove duplicate rows
        unique_data <- data[!duplicated(data), ]
        
        if (nrow(unique_data) < 2) {
          stop("Not enough unique rows to perform t-SNE after removing duplicates.")
        }
        
        # Retry t-SNE with unique data
        tryCatch(
          {
            tsne_result <- Rtsne(unique_data, dims = dims, perplexity = perplexity, verbose = verbose)
            message("t-SNE completed successfully after removing duplicates.")
            return(tsne_result)
          },
          error = function(e2) {
            stop("t-SNE failed even after removing duplicates: ", e2$message)
          }
        )
      } else {
        # Propagate other errors
        stop("t-SNE failed: ", e$message)
      }
    }
  )
}

# Run t-SNE with error handling
tsne <- safe_Rtsne(data, dims = 2, perplexity = 30)

# If t-SNE succeeded, proceed
if (!is.null(tsne)) {
  plot(tsne$Y, main = "t-SNE Visualization")
}

# Remove duplicate rows from the data
data_unique <- data[!duplicated(data), ]

# Now, run t-SNE on the unique data
tsne <- Rtsne(data_unique, dims = 2, perplexity = 30)

tsne_data <- data.frame(tsne$Y)
fviz_cluster(list(data = tsne_data, cluster = kmeans(tsne_data, 3)$cluster), 
             main = "t-SNE")

# 3. UMAP
umap_config <- umap.defaults
umap_config$n_neighbors <- 15
umap_data <- umap(data, config = umap_config)$layout
umap_data <- data.frame(umap_data)
fviz_cluster(list(data = umap_data, cluster = kmeans(umap_data, 3)$cluster), 
             main = "UMAP")

# 4. Factor Analysis
fa <- fa(data, nfactors = 2, rotate = "varimax", fm = "ml")
fa_data <- data.frame(fa$scores)
fviz_cluster(list(data = fa_data, cluster = kmeans(fa_data, 3)$cluster), 
             main = "Factor Analysis")

# 5. MDS
mds <- cmdscale(dist(data), k = 2)
mds_data <- data.frame(mds)
fviz_cluster(list(data = mds_data, cluster = kmeans(mds_data, 3)$cluster), 
             main = "MDS")


# Results Visualization
# Combine plots for comparison (Optional: Using gridExtra or patchwork)
p1 <- fviz_cluster(list(data = pca_data, cluster = kmeans(pca_data, 3)$cluster), main = "PCA")
p2 <- fviz_cluster(list(data = tsne_data, cluster = kmeans(tsne_data, 3)$cluster), main = "t-SNE")
p3 <- fviz_cluster(list(data = umap_data, cluster = kmeans(umap_data, 3)$cluster), main = "UMAP")
p4 <- fviz_cluster(list(data = fa_data, cluster = kmeans(fa_data, 3)$cluster), main = "Factor Analysis")
p5 <- fviz_cluster(list(data = mds_data, cluster = kmeans(mds_data, 3)$cluster), main = "MDS")

grid.arrange(p1, p2, p3, p4, p5, nrow = 3)

#Now let's calc. the silhouette score

# Function to calculate silhouette score and print the average score
calculate_silhouette <- function(data, cluster_labels, method_name) {
  silhouette_scores <- silhouette(cluster_labels, dist(data))
  avg_silhouette_score <- mean(silhouette_scores[, 3])
  cat("Average Silhouette Score for", method_name, ":", avg_silhouette_score, "\n")
  return(avg_silhouette_score)
}

# 1. PCA
pca_clusters <- kmeans(pca_data, 3)$cluster
pca_silhouette <- calculate_silhouette(pca_data, pca_clusters, "PCA")

# 2. t-SNE
tsne_clusters <- kmeans(tsne_data, 3)$cluster
tsne_silhouette <- calculate_silhouette(tsne_data, tsne_clusters, "t-SNE")

# 3. UMAP
umap_clusters <- kmeans(umap_data, 3)$cluster
umap_silhouette <- calculate_silhouette(umap_data, umap_clusters, "UMAP")

# 4. Factor Analysis
fa_clusters <- kmeans(fa_data, 3)$cluster
fa_silhouette <- calculate_silhouette(fa_data, fa_clusters, "Factor Analysis")

# 5. MDS
mds_clusters <- kmeans(mds_data, 3)$cluster
mds_silhouette <- calculate_silhouette(mds_data, mds_clusters, "MDS")

# Optional: Store all results for comparison
silhouette_scores <- data.frame(
  Method = c("PCA", "t-SNE", "UMAP", "Factor Analysis", "MDS"),
  Silhouette_Score = c(pca_silhouette, tsne_silhouette, umap_silhouette, fa_silhouette, mds_silhouette)
)

# Print all silhouette scores
print(silhouette_scores)

#UMAP (0.4567) has the highest silhouette score, which suggests it may produce the best clustering structure in this case.
#PCA (0.4066), t-SNE (0.4035), and MDS (0.4066) have similar moderate scores, indicating decent clustering but not as strong as UMAP.
#Factor Analysis (0.3668) has the lowest score, which could mean weaker clustering results.



#4.Re-Clustering_on dim.reduced data####################################################################

#Confirming optimal number of clusters: Even though I've identified the best method (UMAP) and PCA,
#respectively, it's still essential to verify the optimal number of clusters.
#I can apply the Elbow Method again directly to the UMAP-reduced dataset
#to refine my cluster count.

#I'm running k-means clustering for a range of cluster numbers and plotting the 
#within-cluster sum of squares (WSS) to identify the "elbow point," where the 
#WSS starts to decrease at a slower rate.


# Function to compute WSS for different values of k
elbow_method <- function(data, max_k = 10) {
  wss <- numeric(max_k)
  
  for (k in 1:max_k) {
    kmeans_model <- kmeans(data, centers = k, nstart = 25)
    wss[k] <- kmeans_model$tot.withinss  # Total within-cluster sum of squares
  }
  
  return(wss)
}

# Apply Elbow Method to UMAP and PCA data
umap_wss <- elbow_method(umap_data)
pca_wss <- elbow_method(pca_data)

# Plotting the Elbow Method results separately
# UMAP plot
plot(1:10, umap_wss, type = "b", pch = 19, xlab = "Number of clusters", ylab = "WSS",
     main = "Elbow Method (UMAP)")

# PCA plot
plot(1:10, pca_wss, type = "b", pch = 19, xlab = "Number of clusters", ylab = "WSS",
     main = "Elbow Method (PCA)")

#Silhouette_just in case
# Install necessary package if you haven't already
if (!require(cluster)) install.packages("cluster", dependencies = TRUE)

# Function to compute Silhouette Scores for different values of k
silhouette_analysis <- function(data, max_k = 10) {
  library(cluster)
  
  silhouette_scores <- numeric(max_k)
  
  for (k in 2:max_k) {  # Starting from 2 because silhouette score isn't defined for k=1
    kmeans_model <- kmeans(data, centers = k, nstart = 25)
    silhouette_scores[k] <- mean(silhouette(kmeans_model$cluster, dist(data))[, 3])  # Average silhouette score
  }
  
  return(silhouette_scores)
}

# Apply Silhouette Analysis to UMAP and PCA data
umap_silhouette <- silhouette_analysis(umap_data)
pca_silhouette <- silhouette_analysis(pca_data)

# Plotting the Silhouette Analysis results
# UMAP plot
plot(2:10, umap_silhouette[2:10], type = "b", pch = 19, xlab = "Number of clusters", ylab = "Average Silhouette Score",
     main = "Silhouette Method (UMAP)", col = "blue")

# PCA plot
plot(2:10, pca_silhouette[2:10], type = "b", pch = 19, xlab = "Number of clusters", ylab = "Average Silhouette Score",
     main = "Silhouette Method (PCA)", col = "red")


# After I Check the plot for the highest average silhouette score, which indicates the (3 in my case)
# optimal number of clusters for both UMAP and PCA.

###K-means_clustering

# K-means clustering for UMAP data
umap_kmeans <- kmeans(umap_data, centers = 3, nstart = 25)
umap_df <- as.data.frame(umap_data)
umap_df$Cluster <- as.factor(umap_kmeans$cluster)

# K-means clustering for PCA data
pca_kmeans <- kmeans(pca_data, centers = 3, nstart = 25)
pca_df <- as.data.frame(pca_data)
pca_df$Cluster <- as.factor(pca_kmeans$cluster)
str(umap_df)
# Visualize UMAP k-means clusters
ggplot(umap_df, aes(x = X1, y = X2, color = Cluster)) +
  geom_point() +
  ggtitle("K-means Clustering (UMAP)") +
  theme_minimal()
str(pca_df)
# Visualize PCA k-means clusters
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  ggtitle("K-means Clustering (PCA)") +
  theme_minimal()

###DBSCAN_clustering

# DBSCAN clustering for UMAP data
umap_dbscan <- dbscan(umap_data, eps = 0.5, minPts = 5)
umap_df$DBSCAN_Cluster <- as.factor(umap_dbscan$cluster)

# DBSCAN clustering for PCA data
pca_dbscan <- dbscan(pca_data, eps = 0.5, minPts = 5)
pca_df$DBSCAN_Cluster <- as.factor(pca_dbscan$cluster)
str(umap_df)
# Visualize UMAP DBSCAN clusters
ggplot(umap_df, aes(x = X1, y = X2, color = DBSCAN_Cluster)) +
  geom_point() +
  ggtitle("DBSCAN Clustering (UMAP)") +
  theme_minimal()
str(pca_df)
# Visualize PCA DBSCAN clusters
ggplot(pca_df, aes(x = PC1, y = PC2, color = DBSCAN_Cluster)) +
  geom_point() +
  ggtitle("DBSCAN Clustering (PCA)") +
  theme_minimal()

###Heirarchical_clustering
# Hierarchical clustering for UMAP data
umap_dist <- dist(umap_data)
umap_hclust <- hclust(umap_dist, method = "ward.D2")
umap_clusters <- cutree(umap_hclust, k = 3)
umap_df$Hierarchical_Cluster <- as.factor(umap_clusters)

# Hierarchical clustering for PCA data

pca_dist <- dist(pca_data)
pca_hclust <- hclust(pca_dist, method = "ward.D2")
pca_clusters <- cutree(pca_hclust, k = 3)
pca_df$Hierarchical_Cluster <- as.factor(pca_clusters)
str(umap_df)
# Visualize UMAP hierarchical clusters
ggplot(umap_df, aes(x = X1, y = X2, color = Hierarchical_Cluster)) +
  geom_point() +
  ggtitle("Hierarchical Clustering (UMAP)") +
  theme_minimal()
str(pca_df)
# Visualize PCA hierarchical clusters
ggplot(pca_df, aes(x = PC1, y = PC2, color = Hierarchical_Cluster)) +
  geom_point() +
  ggtitle("Hierarchical Clustering (PCA)") +
  theme_minimal()

###visualizing all results in 3D 

# Function to save and display interactive 3D plots
save_and_browse_plot <- function(plot, file_name) {
  # Save plot as an interactive HTML file
  saveWidget(plot, file_name, selfcontained = TRUE)
  # Open the HTML file in the default browser
  browseURL(file_name)
}

# Create and save 3D clustering visualizations for different clustering methods
str(umap_df)
# 1. K-means 3D Plot for UMAP Data
plot_kmeans_umap_3d <- plot_ly(
  umap_df, x = ~X1, y = ~X2, z = ~Cluster, color = ~Cluster,
  colors = c('red', 'blue', 'green'),
  type = 'scatter3d', mode = 'markers'
) %>%
  layout(title = 'K-means Clustering (UMAP)')
save_and_browse_plot(plot_kmeans_umap_3d, "plot_kmeans_umap_3d.html")
str(pca_df)
# 2. K-means 3D Plot for PCA Data
plot_kmeans_pca_3d <- plot_ly(
  pca_df, x = ~PC1, y = ~PC2, z = ~Cluster, color = ~Cluster,
  colors = c('red', 'blue', 'green'),
  type = 'scatter3d', mode = 'markers'
) %>%
  layout(title = 'K-means Clustering (PCA)')
save_and_browse_plot(plot_kmeans_pca_3d, "plot_kmeans_pca_3d.html")
str(umap_df)
# 3. DBSCAN 3D Plot for UMAP Data
plot_dbscan_umap_3d <- plot_ly(
  umap_df, x = ~X1, y = ~X2, z = ~Cluster, color = ~DBSCAN_Cluster,
  colors = c('red', 'blue', 'green', 'purple'),
  type = 'scatter3d', mode = 'markers'
) %>%
  layout(title = 'DBSCAN Clustering (UMAP)')
save_and_browse_plot(plot_dbscan_umap_3d, "plot_dbscan_umap_3d.html")
str(pca_df)
# 4. DBSCAN 3D Plot for PCA Data
plot_dbscan_pca_3d <- plot_ly(
  pca_df, x = ~PC1, y = ~PC2, z = ~Cluster, color = ~DBSCAN_Cluster,
  colors = c('red', 'blue', 'green', 'purple'),
  type = 'scatter3d', mode = 'markers'
) %>%
  layout(title = 'DBSCAN Clustering (PCA)')
save_and_browse_plot(plot_dbscan_pca_3d, "plot_dbscan_pca_3d.html")
str(umap_df)
# 5. Hierarchical 3D Plot for UMAP Data
plot_hclust_umap_3d <- plot_ly(
  umap_df, x = ~X1, y = ~X2, z = ~Cluster, color = ~Hierarchical_Cluster,
  colors = c('red', 'blue', 'green'),
  type = 'scatter3d', mode = 'markers'
) %>%
  layout(title = 'Hierarchical Clustering (UMAP)')
save_and_browse_plot(plot_hclust_umap_3d, "plot_hclust_umap_3d.html")

# 6. Hierarchical 3D Plot for PCA Data
plot_hclust_pca_3d <- plot_ly(
  pca_df, x = ~PC1, y = ~PC2, z = ~Cluster, color = ~Hierarchical_Cluster,
  colors = c('red', 'blue', 'green'),
  type = 'scatter3d', mode = 'markers'
) %>%
  layout(title = 'Hierarchical Clustering (PCA)')
save_and_browse_plot(plot_hclust_pca_3d, "plot_hclust_pca_3d.html")

# Evaluate K-means Clustering (UMAP)
sil_kmeans_umap <- silhouette(umap_kmeans$cluster, dist(umap_data))
silhouette_score_kmeans_umap <- mean(sil_kmeans_umap[, 3])  # Silhouette Score


cat("K-means Clustering (UMAP):\n")
cat("Silhouette Score: ", silhouette_score_kmeans_umap, "\n")

# Evaluate K-means Clustering (PCA)
sil_kmeans_pca <- silhouette(pca_kmeans$cluster, dist(pca_data))
silhouette_score_kmeans_pca <- mean(sil_kmeans_pca[, 3])  # Silhouette Score

cat("K-means Clustering (PCA):\n")
cat("Silhouette Score: ", silhouette_score_kmeans_pca, "\n")

# Evaluate DBSCAN Clustering (UMAP)
sil_dbscan_umap <- silhouette(umap_dbscan$cluster, dist(umap_data))
silhouette_score_dbscan_umap <- mean(sil_dbscan_umap[, 3])  # Silhouette Score


cat("DBSCAN Clustering (UMAP):\n")
cat("Silhouette Score: ", silhouette_score_dbscan_umap, "\n")

# Evaluate DBSCAN Clustering (PCA)
sil_dbscan_pca <- silhouette(pca_dbscan$cluster, dist(pca_data))
silhouette_score_dbscan_pca <- mean(sil_dbscan_pca[, 3])  # Silhouette Score

cat("DBSCAN Clustering (PCA):\n")
cat("Silhouette Score: ", silhouette_score_dbscan_pca, "\n")

# Evaluate Hierarchical Clustering (UMAP)
sil_hclust_umap <- silhouette(umap_clusters, dist(umap_data))
silhouette_score_hclust_umap <- mean(sil_hclust_umap[, 3])  # Silhouette Score


cat("Hierarchical Clustering (UMAP):\n")
cat("Silhouette Score: ", silhouette_score_hclust_umap, "\n")

# Evaluate Hierarchical Clustering (PCA)
sil_hclust_pca <- silhouette(pca_clusters, dist(pca_data))
silhouette_score_hclust_pca <- mean(sil_hclust_pca[, 3])  # Silhouette Score


cat("Hierarchical Clustering (PCA):\n")
cat("Silhouette Score: ", silhouette_score_hclust_pca, "\n")
