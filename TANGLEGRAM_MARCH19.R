#TANGLEGRAM MARCH19
library(ggplot2)
library(ggtree)
library(tidyverse)
library(readxl)
library(ape)
library(dendextend)
library(stringr)
library(phytools)
library(rcompanion)
library(vegan)

# Load the dataset
file_path <- "bray_curtis_distances3.csv"  # Ensure the correct path
bray_curtis_distances <- as.dist(read.csv(file_path, row.names = 1))

bray_curtis_distances <- as.dist(read.csv("bray_curtis_distances3.csv", row.names = 1))

# Load and process the phylogenetic tree
tree <- read.tree("plants_seq_alingned3.fasta.treefile")

plot(tree)

# Clean and standardize tree tip labels to match plant_order
tree$tip.label <- str_replace_all(tree$tip.label, "[_.0-9]+$", "")

# Root the tree at the midpoint
tree <- midpoint.root(tree)

# Check if the tree is ultrametric, binary, and rooted
if (!is.ultrametric(tree)) {
  tree <- chronos(tree)  # Convert to ultrametric if necessary
}

if (!is.binary.tree(tree)) {
  tree <- multi2di(tree)  # Convert to binary if necessary
}

if (!is.rooted(tree)) {
  tree <- root(tree, outgroup = tree$tip.label[1], resolve.root = TRUE)  # Ensure it's rooted
}


#Perform hierarchical clustering
hc_chemical <- hclust(bray_curtis_distances, method = "average")

# Convert phylogenetic tree to dendrogram
phylo_dendro <- as.dendrogram(as.hclust(tree))
chemical_dendro <- as.dendrogram(hc_chemical)

# # Extract bootstrap values if available
# if (!is.null(tree$node.label)) {
#   bootstrap_values <- as.numeric(tree$node.label)
#   print("Bootstrap values:")
#   print(bootstrap_values)
# } else {
#   bootstrap_values <- rep(NA, length(tree$edge)/2)  # If no bootstrap values, fill with NAs
#   print("No bootstrap values found in the tree.")
# }
# 
# # Extract bootstrap values if available
# if (!is.null(tree$node.label)) {
#   tree$node.label <- as.numeric(tree$node.label)
#   tree_data <- fortify(tree)
#   bootstrap_df <- tree_data %>% filter(!is.na(label)) %>% select(node, label)
#   print("Bootstrap values:")
#   print(bootstrap_df)
# } else {
#   print("No bootstrap values found in the tree.")
#   bootstrap_df <- data.frame()
# }
# 
# # Convert tree to ggtree object and plot with bootstrap values
# p <- ggtree(tree) +
#   geom_tiplab(size = 3) +
#   geom_text2(data = bootstrap_df, aes(x = x, y = y, label = label), size = 3, hjust = -0.2) +
#   theme_tree2()
# 
# print(p)  # Ensure tree with bootstrap is displayed


# Create tanglegram
par(mar = c(1,1,1,1))
tanglegram(phylo_dendro, chemical_dendro, lwd = 2.0, margin_inner = 9.0, margin_outer = 3.0,
           sort = TRUE, match_order_by_labels = TRUE, common_subtrees_color_lines = TRUE,
           highlight_distinct_edges  = FALSE, highlight_branches_lwd = FALSE)

# # Compute Baker's gamma correlation
# bakers_gamma <- cor_bakers_gamma(phylo_dendro, chemical_dendro)
# print(paste("Baker's Gamma Correlation: ", round(bakers_gamma, 4)))

# Compute Pearson correlation
dend_list <- dendlist(phylo_dendro, chemical_dendro)
pearson_corr <- cor.dendlist(dend_list, method_coef = "pearson")
print(paste("Pearson Correlation: ", round(pearson_corr, 1)))

#option 2co

library(ggplot2)
library(ggtree)
library(tidyverse)
library(readxl)
library(ape)
library(dendextend)
library(stringr)
library(phytools)
library(rcompanion)
library(vegan)

# Load the dataset
file_path <- "bray_curtis_distances3.csv"  # Ensure the correct path
bray_curtis_distances <- as.dist(read.csv(file_path, row.names = 1))

# Load and process the phylogenetic tree
tree <- read.tree("plants_seq_alingned3.fasta.treefile")

# Clean and standardize tree tip labels
tree$tip.label <- str_replace_all(tree$tip.label, "[_.0-9]+$", "")

# Root the tree at the midpoint
tree <- midpoint.root(tree)

# Check if the tree is ultrametric, binary, and rooted
if (!is.ultrametric(tree)) {
  tree <- chronos(tree)  # Convert to ultrametric if necessary
}

if (!is.binary.tree(tree)) {
  tree <- multi2di(tree)  # Convert to binary if necessary
}

if (!is.rooted(tree)) {
  tree <- root(tree, outgroup = tree$tip.label[1], resolve.root = TRUE)  # Ensure it's rooted
}
# 
# # Standardize Bray-Curtis distance row names
# rownames(bray_curtis_matrix) <- str_replace_all(rownames(as.matrix(bray_curtis_distances)), "[_.0-9]+$", "_")
# 
# # Ensure only species in both datasets are used
# common_species <- intersect(tree$tip.label, rownames(as.matrix(bray_curtis_distances)))
# print("Common species in both datasets:")
# print(common_species)
# 
# # Reorder species in both datasets
# tree <- drop.tip(tree, setdiff(tree$tip.label, common_species))
# common_species <- sort(common_species)
# bray_curtis_distances <- as.dist(as.matrix(bray_curtis_distances)[common_species, common_species])

# Perform hierarchical clustering
hc_chemical <- hclust(bray_curtis_distances, method = "average")

# Convert phylogenetic tree to dendrogram properly
phylo_dendro <- as.dendrogram(hclust(cophenetic(tree)))
chemical_dendro <- as.dendrogram(hc_chemical)

# Ensure labels match in correct order
phylo_dendro <- rotate(phylo_dendro, common_species)
chemical_dendro <- rotate(chemical_dendro, common_species)

# Plot phylogenetic tree with bootstrap values
ggtree(tree) +
  geom_tiplab(size = 3) +
  geom_nodelab(aes(label=bootstrap), size = 3, hjust=-0.2) +
  theme_tree2()

# Create tanglegram
par(mar = c(1,1,1,1))
tanglegram(phylo_dendro, chemical_dendro, lwd = 2.0, margin_inner = 9.0, margin_outer = 3.0,
           sort = TRUE, match_order_by_labels = TRUE, common_subtrees_color_lines = TRUE,
           highlight_distinct_edges  = FALSE, highlight_branches_lwd = FALSE)
# 
# # Compute Baker's gamma correlation
# if (all(labels(phylo_dendro) == labels(chemical_dendro))) {
#   bakers_gamma <- cor_bakers_gamma(phylo_dendro, chemical_dendro)
#   print(paste("Baker's Gamma Correlation: ", round(bakers_gamma, 4)))
# } else {
#   print("Error: Dendrogram labels do not match exactly!")
# }

# Compute Pearson correlation
dend_list <- dendlist(phylo_dendro, chemical_dendro)
pearson_corr <- cor.dendlist(dend_list, method_coef = "pearson")
print(paste("Pearson Correlation: ", round(pearson_corr, 4)))


#Test for Phylogenetic Signal
#This determines whether chemical traits tend to be more similar among closely related species.

#Blomberg’s K / Pagel’s Lambda

library(phytools)
phylosig(tree, chemical_dendro, method = "K", test = TRUE)


#option 3---june 7-25-------------------------------

# Load required packages
library(ggplot2)
library(ggtree)
library(tidyverse)
library(readxl)
library(ape)
library(dendextend)
library(stringr)
library(phytools)
library(vegan)

# Load Bray-Curtis distance matrix
file_path <- "bray_curtis_distances3.csv"
bray_curtis_distances <- as.dist(read.csv(file_path, row.names = 1, check.names = FALSE))

# Load and process the phylogenetic tree
tree <- read.tree("plants_seq_alingned3.fasta.treefile")

# Clean tree tip labels (remove underscores, digits, and trailing characters)
tree$tip.label <- str_replace_all(tree$tip.label, "[_.0-9]+$", "")

# Root and format the tree
tree <- midpoint.root(tree)
if (!is.ultrametric(tree)) tree <- chronos(tree)  # Make ultrametric
if (!is.binary.tree(tree)) tree <- multi2di(tree) # Make binary
if (!is.rooted(tree)) tree <- root(tree, outgroup = tree$tip.label[1], resolve.root = TRUE)

# Convert dist object to matrix for label cleaning
bray_curtis_matrix <- as.matrix(bray_curtis_distances)

# Clean Bray–Curtis distance row names to match tree labels

cleaned_names <- str_replace_all(rownames_matrix, "[_.0-9]+$", "")
rownames(bray_curtis_distances) <- cleaned_names
colnames(bray_curtis_distances) <- cleaned_names

# Keep only shared species
common_species <- intersect(tree$tip.label, rownames(as.matrix(bray_curtis_distances)))
bray_curtis_distances <- as.dist(as.matrix(bray_curtis_distances)[common_species, common_species])
tree <- drop.tip(tree, setdiff(tree$tip.label, common_species))

# Perform hierarchical clustering on VOC data
hc_chemical <- hclust(bray_curtis_distances, method = "average")

# Convert both tree and chemical clustering to dendrograms
phylo_dendro <- as.dendrogram(hclust(cophenetic(tree)))
chemical_dendro <- as.dendrogram(hc_chemical)

# Ensure consistent label order
common_species <- sort(common_species)
phylo_dendro <- rotate(phylo_dendro, common_species)
chemical_dendro <- rotate(chemical_dendro, common_species)

# Plot phylogenetic tree (optional, before tanglegram)
ggtree(tree) +
  geom_tiplab(size = 3) +
  theme_tree2()

# Plot tanglegram comparing phylogenetic and chemical dendrograms
par(mar = c(1,1,1,1))
tanglegram(
  phylo_dendro,
  chemical_dendro,
  lwd = 2,
  margin_inner = 9,
  margin_outer = 3,
  sort = TRUE,
  match_order_by_labels = TRUE,
  common_subtrees_color_lines = TRUE,
  highlight_distinct_edges = FALSE,
  highlight_branches_lwd = FALSE
)

# Correlation between dendrograms
if (all(labels(phylo_dendro) == labels(chemical_dendro))) {
  pearson_corr <- cor.dendlist(dendlist(phylo_dendro, chemical_dendro), method_coef = "pearson")
  bakers_gamma <- cor_bakers_gamma(phylo_dendro, chemical_dendro)
  cat("Pearson correlation: ", round(pearson_corr, 4), "\n")
  cat("Baker's gamma correlation: ", round(bakers_gamma, 4), "\n")
} else {
  warning("Dendrogram labels do not match. Check for inconsistencies in species naming.")
}

#plot three with bootstrap values


# Load required libraries
library(ape)
library(phytools)

# Load the phylogenetic tree
tree <- read.tree("plants_seq_alingned3.fasta.treefile")

# Optional: View tree structure
# str(tree)

# Root the tree at the midpoint (optional but often helps in clarity)
tree <- midpoint.root(tree)

# Ladderize the tree for cleaner plotting
tree <- ladderize(tree)

# Plot the tree with bootstrap values
plot(tree, show.tip.label = TRUE, cex = 0.7)  # Basic plot
nodelabels(tree$node.label, frame = "none", adj = c(1.1, -0.3), cex = 0.6)  # Add bootstrap values

# Optional: Save to PDF
pdf("phylogenetic_tree_with_bootstrap.pdf", width = 8, height = 10)
plot(tree, show.tip.label = TRUE, cex = 0.7)
nodelabels(tree$node.label, frame = "none", adj = c(1.1, -0.3), cex = 0.6)
dev.off()


#option to calculate pearson correlation

# Load necessary libraries
library(ggplot2)
library(ggtree)
library(tidyverse)
library(readxl)
library(ape)
library(dendextend)
library(stringr)
library(phytools)
library(rcompanion)
library(vegan)

# Load Bray-Curtis distance matrix
file_path <- "bray_curtis_distances3.csv"
bray_curtis_distances <- as.dist(read.csv("bray_curtis_distances3.csv", row.names = 1))

# Load and process the phylogenetic tree
tree <- read.tree("plants_seq_alingned3.fasta.treefile")
tree$tip.label <- str_replace_all(tree$tip.label, "[_.0-9]+$", "")  # Clean tip labels
tree <- midpoint.root(tree)

if (!is.ultrametric(tree)) {
  tree <- chronos(tree)
}
if (!is.binary.tree(tree)) {
  tree <- multi2di(tree)
}
if (!is.rooted(tree)) {
  tree <- root(tree, outgroup = tree$tip.label[1], resolve.root = TRUE)
}

# Match labels between datasets
bray_labels <- rownames(as.matrix(bray_curtis_distances))
tree_labels <- tree$tip.label
common_species <- intersect(bray_labels, tree_labels)

# Filter to keep only common species
tree <- drop.tip(tree, setdiff(tree_labels, common_species))
bray_curtis_distances <- as.dist(as.matrix(bray_curtis_distances)[common_species, common_species])

# Build dendrograms
hc_chemical <- hclust(bray_curtis_distances, method = "average")
chemical_dendro <- as.dendrogram(hc_chemical)
phylo_dendro <- as.dendrogram(hclust(cophenetic(tree)))

any(is.na(tree$tip.label))       # Check for NAs
any(duplicated(tree$tip.label))  # Check for duplicates


# Align dendrograms
phylo_dendro <- rotate(phylo_dendro, labels(chemical_dendro))

# Plot tanglegram
par(mar = c(1,1,1,1))
tanglegram(phylo_dendro, chemical_dendro,
           lwd = 2.0,
           margin_inner = 9.0,
           margin_outer = 3.0,
           sort = TRUE,
           match_order_by_labels = TRUE,
           common_subtrees_color_lines = TRUE,
           highlight_distinct_edges = FALSE,
           highlight_branches_lwd = FALSE)

# Pearson correlation
dend_list <- dendlist(phylo_dendro, chemical_dendro)
pearson_corr <- cor.dendlist(dend_list, method_coef = "pearson")
cat("Pearson Correlation: ", round(pearson_corr, 4), "\n")


#option 4 to calculate pearson

# Load libraries
library(ggplot2)
library(ggtree)
library(tidyverse)

library(ape)
library(dendextend)
library(stringr)
library(phytools)
library(rcompanion)
library(vegan)

# Load Bray-Curtis distance matrix
bray_curtis_distances <- as.dist(read.csv("bray_curtis_distances3.csv", row.names = 1))

# Load and process the phylogenetic tree
tree <- read.tree("plants_seq_alingned3.fasta.treefile")
tree$tip.label <- str_replace_all(tree$tip.label, "[_.0-9]+$", "")  # Clean tip labels

# Root tree if needed
tree <- midpoint.root(tree)
if (!is.ultrametric(tree)) tree <- chronos(tree)
if (!is.binary.tree(tree)) tree <- multi2di(tree)

# Match labels between tree and distance matrix
species_intersect <- intersect(tree$tip.label, labels(bray_curtis_distances))
tree <- drop.tip(tree, setdiff(tree$tip.label, species_intersect))
bray_curtis_distances <- as.dist(as.matrix(bray_curtis_distances)[species_intersect, species_intersect])

# Create dendrograms
phylo_dist <- cophenetic(tree)
phylo_dendro <- as.dendrogram(hclust(as.dist(phylo_dist)))
chemical_dendro <- as.dendrogram(hclust(bray_curtis_distances))

# Compute Pearson correlation
dend_list <- dendlist(phylo_dendro, chemical_dendro)
pearson_corr <- cor.dendlist(dend_list, method_coef = "pearson")
cat("Pearson Correlation between chemical and phylogenetic distances:", round(pearson_corr, 4), "\n")

# Plot tanglegram
par(mar = c(1,1,1,1))
tanglegram(phylo_dendro, chemical_dendro,
           lwd = 2.0, margin_inner = 9.0, margin_outer = 3.0,
           sort = TRUE, match_order_by_labels = TRUE,
           common_subtrees_color_lines = TRUE,
           highlight_distinct_edges  = FALSE,
           highlight_branches_lwd = FALSE)

