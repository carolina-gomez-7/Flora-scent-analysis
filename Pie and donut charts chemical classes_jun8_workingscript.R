#pie charts of compound relative abundance
# Load required libraries
library(tidyverse)

merged_data1 <- read.csv("relative_abundance_compound_class.csv")

# Summarize total abundance per class and sort by abundance
class_summary <- merged_data1 %>%
  group_by(Class) %>%
  summarise(Total_Abundance = sum(`Relative_abundance`, na.rm = TRUE)) %>%
  mutate(
    Percentage = Total_Abundance / sum(Total_Abundance) * 100,
    Label = paste0(sprintf("%.1f%%", Percentage))
  ) %>%
  arrange(desc(Total_Abundance)) %>%
  mutate(Class = factor(Class, levels = Class))  # Reorder factor levels

# Compute position for labels
class_summary <- class_summary %>%
  mutate(
    ymax = cumsum(Percentage),
    ymin = lag(ymax, default = 0),
    mid = (ymax + ymin) / 2
  )

# Plot donut chart (with hole in center)
ggplot(class_summary, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = Class)) +
  geom_rect() +
  geom_text(aes(x = 3, y = mid, label = Label), size = 4) +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  labs(title = "Relative Abundance of Compound Classes") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Plot true pie chart
ggplot(class_summary, aes(x = "", y = Percentage, fill = Class)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), size = 4) +
  theme_void() +
  labs(title = "Relative Abundance of Compound Classes") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# plot donut chart 2

ggplot(class_summary, aes(x = 2, y = Percentage, fill = Class)) +  # Set x = 2 for donut structure
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +  # Adjust to control the size of the donut hole
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), size = 4, color = "white") +
  theme_void() +
  labs(title = "Relative Abundance of Compound Classes") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )



# Plot pie charts for compound proportions

# Load necessary libraries
library(tidyverse)
library(ggrepel)

# Load dataset
compound_data <- read.csv("Compund_Class.csv")

# Keep only relevant columns
compound_data <- compound_data %>% select(Compound, Class)

# Count the number of compounds in each class and calculate percentages
class_counts <- compound_data %>%
  group_by(Class) %>%
  summarize(count = n()) %>%
  mutate(
    percentage = round((count / sum(count)) * 100, 1),
    label = paste0(percentage, "%")
  ) %>%
  arrange(desc(count))

# Set factor levels to control slice order
class_counts$Class <- factor(class_counts$Class, levels = class_counts$Class)

# Plot pie chart with percentage labels
ggplot(class_counts, aes(x = "", y = count, fill = Class)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  theme_void() +
  labs(title = "Distribution of Compound Classes") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Plot donut chart with percentage labels
ggplot(class_counts, aes(x = 2, y = count, fill = Class)) +  # Use x = 2 for donut
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +  # Creates the hole by shrinking x-axis range
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  theme_void() +
  labs(title = "Distribution of Compound Classes") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
