# Load required libraries
library(vegan)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(tibble)

# Load dataset
file_path <- "filtered_data_10feb.csv"
filtered_data <- read_csv(file_path)

# Reshape data: Convert from long to wide format (samples as rows, compounds as columns)
abundance_matrix <- filtered_data %>%
  select(Individual, Plant_species, Compound, Abundance) %>%
  pivot_wider(names_from = "Compound", values_from = "Abundance", values_fill = list(Abundance = 0)) %>%
  column_to_rownames(var = "Individual")

# Convert all columns to numeric
abundance_matrix[] <- lapply(abundance_matrix, function(x) as.numeric(as.character(x)))

# Replace NA and Inf values with 0
abundance_matrix[is.na(abundance_matrix)] <- 0
abundance_matrix[is.infinite(abundance_matrix)] <- 0

# Ensure it's a proper numeric matrix
abundance_matrix <- as.matrix(abundance_matrix)

# Extract metadata (Plant species)
metadata <- filtered_data %>%
  select(Individual, Plant_species) %>%
  distinct() %>%
  column_to_rownames(var = "Individual")

# Compute Bray-Curtis dissimilarity matrix
bray_dist <- vegdist(abundance_matrix, method = "bray")

# Run PERMANOVA
set.seed(123)
permanova_result <- adonis2(bray_dist ~ Plant_species, data = metadata, permutations = 999)
print(permanova_result)

# Perform Principal Coordinates Analysis (PCoA)
pcoa_result <- cmdscale(bray_dist, k = 2, eig = TRUE)

# Convert PCoA results to a data frame
pcoa_df <- as.data.frame(pcoa_result$points)
colnames(pcoa_df) <- c("PCoA1", "PCoA2")

# Add plant species metadata
metadata <- filtered_data %>%
  select(Individual, Plant_species) %>%
  distinct() %>%
  column_to_rownames(var = "Individual")

# Merge PCoA results with metadata
pcoa_df <- pcoa_df %>%
  rownames_to_column(var = "Individual") %>%
  left_join(rownames_to_column(metadata, var = "Individual"), by = "Individual")

# Calculate convex hulls for each species
hulls <- pcoa_df %>%
  group_by(Plant_species) %>%
  slice(chull(PCoA1, PCoA2))

# Plot PCoA with convex hulls
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = Plant_species)) +
  geom_polygon(data = hulls, aes(fill = Plant_species), alpha = 0.2, color = "black", show.legend = FALSE) +  # Hulls
  geom_point(size = 4, alpha = 0.8) +  # Points
  theme_minimal() +
  labs(title = "PCoA of Volatile Compound Composition",
       x = "PCoA Axis 1",
       y = "PCoA Axis 2",
       color = "Plant Species") +
  theme(plot.title = element_text(hjust = 0.5))
