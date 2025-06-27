library(tidyverse)     # Data manipulation
library(vegan)         # Ecological stats like PERMANOVA
library(ggpubr)        # Publication-ready plots
library(reshape2)      # Data reshaping
library(ape)           # Phylogenetic utilities
library(pheatmap)      # Heatmap visualization

data <- read.csv("filtered_data_10feb.csv")

# Ensure necessary columns are factors
data$Plant_species <- as.factor(data$Plant_species)
data$Compound <- as.factor(data$Compound)
data$Individual <- as.factor(data$Individual)

avg_profile <- data %>%
  group_by(Plant_species, Compound) %>%
  summarize(mean_abundance = mean(Abundance, na.rm = TRUE)) %>%
  spread(key = Compound, value = mean_abundance, fill = 0)

# Export average profile table
write.csv(avg_profile, "average_compound_profile_2.csv", row.names = FALSE)

# Convert to matrix and normalize across compounds
heatmap_matrix <- as.matrix(avg_profile[,-1])
rownames(heatmap_matrix) <- avg_profile$Plant_species
heatmap_matrix <- sweep(heatmap_matrix, 2, colSums(heatmap_matrix, na.rm = TRUE), "/")

# Generate heatmap of compound profiles
pheatmap(heatmap_matrix,
         scale = "row",
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "ward.D",
         color = colorRampPalette(c("white", "blue", "red"))(100),
         main = "Heatmap of Volatile Compounds Across Plant Species")


# CV = SD / Mean for each compound within species
cv_data <- data %>%
  group_by(Plant_species, Compound) %>%
  summarize(
    mean_abundance = mean(Abundance, na.rm = TRUE),
    sd_abundance = sd(Abundance, na.rm = TRUE)
  ) %>%
  mutate(CV = sd_abundance / mean_abundance) %>%
  filter(!is.na(CV))  # Remove NA caused by single observations

# Set phylogenetic order
plant_order <- c("Ballota nigra", "Stachys palustris", "Lavandula angustifolia", "Nepeta racemosa",
                 "Rhinanthus minor", "Ligustrum vulgare", "Buddleja davidii", "Veronica spicata",
                 "Calystegia silvatica", "Cirsium vulgare", "Cirsium arvense", "Arctium minus",
                 "Centaurea nigra", "Tripleurospermum inodorum", "Jacobaea vulgaris", "Symphoricarpos albus",
                 "Heracleum sphondylium", "Hydrangea paniculata", "Impatiens glandulifera",
                 "Rubus caesius", "Rubus fruticosus agg.", "Lotus corniculatus", "Trifolium repens",
                 "Hypericum perforatum", "Geranium pratense", "Epilobium angustifolium",
                 "Tilia sp2", "Ranunculus acris")

# Apply ordering
cv_data$Plant_species <- factor(cv_data$Plant_species, levels = plant_order)

ggplot(cv_data, aes(x = Plant_species, y = CV)) +
  geom_boxplot(fill = "chartreuse4", alpha = 0.7, outlier.color = "gray", outlier.shape = 16, outlier.size = 2) + 
  geom_jitter(width = 0.2, alpha = 0.4, color = "darkslategray") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  labs(title = "Distribution of Coefficient of Variation (CV) Within Plant Species",
       x = "Plant Species",
       y = "Coefficient of Variation (CV)")


