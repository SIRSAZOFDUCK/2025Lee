## A machine learning benchmarking system for antibiotic prescribing in primary care
## Lee et al. 2025

## Set up -----------------

# Clear environment
rm(list = ls())

# Set working directory
setwd("/Users/u1771937/Desktop/Supervision/HMS/2024-25/Harry Lee/data") # Change to your own folder

# Install packages
list.of.packages <- c("htmlwidgets","leaflet","httr", "maps","datatable","purrr","readr", "dplyr", "ggplot2", "stringr", "factoextra", "cluster", "NbClust", "tibble", "randomForest", "ggraph", "igraph", "caret", "party", "car", "ggpubr", "FSA", "fingertipsR", "tidyr", "plotly", "scatterplot3d", "viridis", "ggrepel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

options(repos = c(
  ropensci = 'https://ropensci.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages("fingertipsR")

# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(factoextra)
library(tidyr)
library(cluster)
library(NbClust)
library(tibble)
library(randomForest)
library(ggraph)
library(igraph)
library(caret)
library(party)
library(car)
library(ggpubr)
library(FSA)
library(fingertipsR)
library(purrr)
library(plotly)
library(scatterplot3d)
library(viridis)
library(ggrepel)
library(data.table)
library(httr)
library(maps)
library(leaflet)
library(htmlwidgets)

## Read in data ----------

# QOF 2023-24 
# https://digital.nhs.uk/data-and-information/publications/statistical/quality-and-outcomes-framework-achievement-prevalence-and-exceptions-data/2023-24 

qof.data <- read.csv("PREVALENCE_2324.csv") %>% # Read in data
  mutate(proportion = 100*REGISTER/PRACTICE_LIST_SIZE)

# Extract prevalence data

p.af <- qof.data %>% # Atrial fibrillation
  filter(GROUP_CODE == "AF") %>%
  rename(AF = proportion) %>%
  select(PRACTICE_CODE, AF)

p.asthma <- qof.data %>% # Asthma
  filter(GROUP_CODE == "AST") %>%
  rename(AST = proportion) %>%
  select(PRACTICE_CODE, AST)

p.cancer <- qof.data %>% # Cancer
  filter(GROUP_CODE == "CAN") %>%
  rename(CAN = proportion) %>%
  select(PRACTICE_CODE, CAN)

p.chd <- qof.data %>% # Coronary heart disease
  filter(GROUP_CODE == "CHD") %>%
  rename(CHD = proportion) %>%
  select(PRACTICE_CODE, CHD)

p.ckd <- qof.data %>% # Chronic kidney disease
  filter(GROUP_CODE == "CKD") %>%
  rename(CKD = proportion) %>%
  select(PRACTICE_CODE, CKD)

p.copd <- qof.data %>% # COPD
  filter(GROUP_CODE == "COPD") %>%
  rename(COPD = proportion) %>%
  select(PRACTICE_CODE, COPD)

p.dm <- qof.data %>% # Diabetes
  filter(GROUP_CODE == "DM") %>%
  rename(DM = proportion) %>%
  select(PRACTICE_CODE, DM)

p.dementia <- qof.data %>% # Dementia
  filter(GROUP_CODE == "DEM") %>%
  rename(DEM = proportion) %>%
  select(PRACTICE_CODE, DEM)

p.epilepsy <- qof.data %>% # Epilepsy
  filter(GROUP_CODE == "EP") %>%
  rename(EP = proportion) %>%
  select(PRACTICE_CODE, EP)

p.hf <- qof.data %>% # Heart failure
  filter(GROUP_CODE == "HF") %>%
  rename(HF = proportion) %>%
  select(PRACTICE_CODE, HF)

p.hyp <- qof.data %>% # Hypertension
  filter(GROUP_CODE == "HYP") %>%
  rename(HYP = proportion) %>%
  select(PRACTICE_CODE, HYP)

p.ld <- qof.data %>% # Learning disability
  filter(GROUP_CODE == "LD") %>%
  rename(LD = proportion) %>%
  select(PRACTICE_CODE, LD)

p.psychosis <- qof.data %>% # Psychosis
  filter(GROUP_CODE == "MH") %>%
  rename(MH = proportion) %>%
  select(PRACTICE_CODE, MH)

p.obesity <- qof.data %>% # Obesity
  filter(GROUP_CODE == "OB") %>%
  rename(OB = proportion) %>%
  select(PRACTICE_CODE, OB)

p.osteo <- qof.data %>% # Osteoporosis
  filter(GROUP_CODE == "OST") %>%
  rename(OST = proportion) %>%
  select(PRACTICE_CODE, OST)

p.pad <- qof.data %>% # Peripheral arterial disease
  filter(GROUP_CODE == "PAD") %>%
  rename(PAD = proportion) %>%
  select(PRACTICE_CODE, PAD)

p.palliative <- qof.data %>% # Palliative care
  filter(GROUP_CODE == "PC") %>%
  rename(PC = proportion) %>%
  select(PRACTICE_CODE, PC)

p.ra <- qof.data %>% # Rheumatoid arthritis
  filter(GROUP_CODE == "RA") %>%
  rename(RA = proportion) %>%
  select(PRACTICE_CODE, RA)

p.stroketia <- qof.data %>% # Stroke / TIA
  filter(GROUP_CODE == "STIA") %>%
  rename(STIA = proportion) %>%
  select(PRACTICE_CODE, STIA)

# Link QOF data
df_names <- ls(pattern = "^p\\.") # Find all dataframes starting with "p."
df_list <- mget(df_names) # Convert list into dataframes
all.data <- reduce(df_list, full_join, by = "PRACTICE_CODE") # join

# Add total list size data
list.size <- qof.data %>% # Get list size data
  filter(GROUP_CODE == "AF") %>%
  select(PRACTICE_CODE, PRACTICE_LIST_SIZE) %>%
  rename(list.size = PRACTICE_LIST_SIZE)

all.data <- all.data %>%
  full_join(list.size, by = "PRACTICE_CODE")

# Add proportion of <18 years
age.under18 <- qof.data %>% # Get over 18 list size data
  filter(GROUP_CODE == "CKD") %>%
  select(PRACTICE_CODE, PRACTICE_LIST_SIZE) %>%
  rename(age.over18 = PRACTICE_LIST_SIZE) %>%
  full_join(list.size, by = "PRACTICE_CODE") %>%
  mutate(age.under18 = 100*(list.size - age.over18)/list.size) %>% # Calcualte proportion under 18 years
  select(PRACTICE_CODE, age.under18) %>%
  full_join(all.data, by = "PRACTICE_CODE")

all.data <- age.under18

# Add proportion of >=60 years
# List characteristics from NHS Digital, Sep 2023 (mid-year point)
# https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/september-2023
list.size.m <- read.csv("gp-reg-pat-prac-sing-age-male.csv") %>% # Read in data
  mutate(AGE = ifelse(AGE == "95+", "95", AGE)) %>% # Convert 95+ to 95 so it can be numeric  
  filter(AGE != "ALL") %>% # Remove total age rows
  mutate(AGE = as.numeric(AGE)) %>% # Convert age to numeric
  filter(AGE >= 60) %>% # Keep ages over 60+
  select(ORG_CODE, NUMBER_OF_PATIENTS) %>%
  group_by(ORG_CODE) %>%
  summarise(TOTAL_PATIENTS = sum(NUMBER_OF_PATIENTS, na.rm = TRUE)) %>% # Sum number of patients across all ages by GP practice
  rename(PRACTICE_CODE = ORG_CODE) %>%
  rename(age.over60m = TOTAL_PATIENTS) 
  
       
list.size.f <- read.csv("gp-reg-pat-prac-sing-age-female.csv") %>%
  mutate(AGE = ifelse(AGE == "95+", "95", AGE)) %>% # Convert 95+ to 95 so it can be numeric  
  filter(AGE != "ALL") %>% # Remove total age rows
  mutate(AGE = as.numeric(AGE)) %>% # Convert age to numeric
  filter(AGE >= 60) %>% # Keep ages over 60+
  select(ORG_CODE, NUMBER_OF_PATIENTS) %>%
  group_by(ORG_CODE) %>%
  summarise(TOTAL_PATIENTS = sum(NUMBER_OF_PATIENTS, na.rm = TRUE)) %>% # Sum number of patients across all ages by GP practice
  rename(PRACTICE_CODE = ORG_CODE) %>%
  rename(age.over60f = TOTAL_PATIENTS)

age.over60 <- list.size.m %>% # Calculate proportion aged 60 and over
  full_join(list.size.f, by = "PRACTICE_CODE") %>%
  mutate(age.over60 = age.over60m + age.over60f) %>%
  full_join(list.size, by = "PRACTICE_CODE") %>%
  mutate(age.over60 = 100*age.over60/list.size) %>%
  select(PRACTICE_CODE, age.over60)

all.data <- all.data %>% # Add to all data
  full_join(age.over60, by = "PRACTICE_CODE")

# Add proportion of females
list.size.f <- read.csv("gp-reg-pat-prac-sing-age-female.csv") %>%
  mutate(AGE = ifelse(AGE == "95+", "95", AGE)) %>% # Convert 95+ to 95 so it can be numeric  
  filter(AGE != "ALL") %>% # Remove total age rows
  mutate(AGE = as.numeric(AGE)) %>% # Convert age to numeric
  select(ORG_CODE, NUMBER_OF_PATIENTS) %>%
  group_by(ORG_CODE) %>%
  summarise(TOTAL_PATIENTS = sum(NUMBER_OF_PATIENTS, na.rm = TRUE)) %>% # Sum number of patients across all ages by GP practice
  rename(PRACTICE_CODE = ORG_CODE) %>%
  rename(females = TOTAL_PATIENTS) %>%
  full_join(list.size, by = "PRACTICE_CODE") %>%
  mutate(females = 100*females/list.size) %>%
  select(PRACTICE_CODE, females)
  
all.data <- all.data %>% # Add to all data
  full_join(list.size.f, by = "PRACTICE_CODE")

# Link smoking prevalence and IMD data from Fingertips

# Fingertips API (see https://fingertips.phe.org.uk/documents/fingertips_api_guide.pdf and https://github.com/ropensci/fingertipsR)
# profiles <- profiles() # Show PHOF profiles. Use to identify which profile set you need. Here we use profile ID 20 for primary care
# inds <- indicators(ProfileID = "20") # Show indicators for selected PHOF profile. Here we use indicator ID 91280 {smoking prevalence] and 93553 [IMD 2019 score]
# areas <- area_types() # Show area types. Here we use area type 7 [GP practices] 


imd <- fingertips_data(IndicatorID = 93553, AreaTypeID = 7) %>% # Get IMD data
  # Keep required time period
  filter(Timeperiod == "2019") %>% 
  # Remove All England value
  filter(AreaType != "England") %>% 
  # Keep required fields only
  dplyr::select(AreaCode, Value) %>%
  # Rename value to specific indicator
  rename("imd" = "Value") %>%
  rename(PRACTICE_CODE = AreaCode)


p.smoking <- fingertips_data(IndicatorID = 91280, AreaTypeID = 7) %>% # Get smoking prevalence data
  # Keep required time period
  filter(Timeperiod == "2023/24") %>% 
  # Remove All England value
  filter(AreaType != "England") %>% 
  # Keep required fields only
  dplyr::select(AreaCode, Value) %>%
  # Rename value to specific indicator
  rename("p.smoking" = "Value") %>%
  rename(PRACTICE_CODE = AreaCode)

all.data <- all.data %>%
  left_join(p.smoking, by = "PRACTICE_CODE") %>%
  left_join(imd, by = "PRACTICE_CODE") 

## Clean data -----------------

# Keep practice coes
practice_ids <- all.data$PRACTICE_CODE

# Remove non-numeric columns (from 6353 to 6267)
ml.data <- all.data %>%
  select(where(is.numeric)) %>%
  drop_na() # or use another imputation method

# Function to flag outliers using the IQR method +/- three IQRs to only remove extreme outliers
is_extreme_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 3 * iqr) | x > (q3 + 3 * iqr)
}

# Create an outlier flag matrix
extreme_outlier_df <- ml.data %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), is_extreme_outlier))


# Flag any row with at least one extreme outlier
extreme_rows <- apply(extreme_outlier_df, 1, any)
sum(extreme_rows)

# Remove outliers - 463.6237 removed. 5774 lett
ml.data <- ml.data[!extreme_rows, ]

# Scale data to mean 0, SD 1
scaled_data <- scale(ml.data)

## Perform clustering ---------------

# Number of clusters
# Create the base plot object
elbow_plot <- fviz_nbclust(
  scaled_data,
  kmeans,
  method = "wss",
  k.max = 10,
  linecolor = "#2c3e50"
)

# Customize for publication
elbow_plot +
  labs(
    title = "Elbow Method for Optimal Number of Clusters",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_minimal(base_size = 18) +  # Overall text size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    panel.grid.minor = element_blank()
  )

# Save
ggsave("Fig 1. elbow_plot.png", elbow_plot, width = 6, height = 4, dpi = 1200)

#nb <- NbClust(scaled_data, min.nc = 2, max.nc = 10, method = "kmeans") # get statistical recommendation for number of clusters

# Determine optimal number of cluters = answer is 4
set.seed(123)
res.nbclust <- NbClust(ml.data, distance = "euclidean",
                       min.nc = 2, max.nc = 5, #identify min and max number of clusters you want to consider
                       method = "complete", index ="all") 

# Clustering algorithm
set.seed(123) # for reproducibility
k <- 4  # replace with your chosen number of clusters
km <- kmeans(scaled_data, centers = k, nstart = 25)

# Add cluster assignments back to original data
ml.data$cluster <- factor(km$cluster)

## Visualise clusters ------

# Choose colors for clusters
plasma_colors <- plasma(256)
cluster_colors <- plasma_colors[c(1, 50, 150, 200)] 
cluster_labels <- levels(pca_data$cluster)

# 2D visualisation

png("Fig 2. clusters 2D.png", width = 2000, height = 1800, res = 600)

fviz_cluster(km, data = scaled_data, geom = "point", ellipse.type = "norm", 
             # Adjust the colors for each cluster
             palette = cluster_colors, 
             pointsize = 0.2,
             # Customizing the plot appearance
             main = "K-means Clustering of GP Practices",
             ggtheme = theme_minimal() +
               theme(
                 plot.title = element_text(size = 8, face = "bold", color = "black", hjust = 0.5),
                 axis.title = element_text(size = 6, face = "bold"),
                 axis.text = element_text(size = 6),
                 legend.title = element_text(size = 8),
                 legend.text = element_text(size = 8)
               )) 

dev.off()

# Run PCA on scaled data
pca <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca$x[, 1:3])  # Get first 3 components
pca_data$cluster <- factor(ml.data$cluster)  # Add cluster info

# Plot 3D interactive
plot <- plot_ly(
  pca_data, 
  x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~cluster, colors = cluster_colors,
  type = "scatter3d", mode = "markers",
  marker = list(size = 4, opacity = 0.8)
) %>%
  layout(
    title = "3D PCA Plot of GP Practice Clusters",
    scene = list(
      xaxis = list(title = 'PC1'),
      yaxis = list(title = 'PC2'),
      zaxis = list(title = 'PC3')
    )
  )

# Save
htmlwidgets::saveWidget(plot, "Fig 4. clusters 3D interactive.html")

# Plot 3D static

# Save to a high-resolution image
png("Fig 3. clusters 3D.png", width = 2000, height = 1800, res = 600)

# Create the 3D plot
s3d <- scatterplot3d(
  pca_data$PC1, pca_data$PC2, pca_data$PC3,
  color = cluster_colors[as.numeric(pca_data$cluster)],
  pch = 16,
  angle = 55,
  main = "3D PCA Plot of GP Practice Clusters",
  xlab = "PC1",
  ylab = "PC2",
  zlab = "PC3",
  grid = TRUE,
  box = TRUE,
  cex.symbols = .1,
  cex.main = 0.5,       # Title size
  cex.lab = 0.5,          # Axis labels size
  cex.axis = 0.5)        # Axis tick mark size)

# Add a custom legend
legend("topright",
       legend = cluster_labels,
       col = cluster_colors,
       pch = 19,
       cex = 0.4,
       title = "Cluster")

# Close the image file
dev.off()

## Profile the clusters ----

# Round features to 2dp
cluster.features <- ml.data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) 

numeric_cols <- sapply(cluster.features, is.numeric)
cluster.features[numeric_cols] <- lapply(cluster.features[numeric_cols], function(x) round(x, 2))

# Transpose for saving
df_transposed <- t(cluster.features)
df_transposed <- as.data.frame(df_transposed)
df_transposed$RowNames <- rownames(df_transposed)

write.csv(df_transposed, "Fig 5. cluster features.csv")

# PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
summary(pca_result)

# Extract PCA results (scores for the first two principal components)
pca_scores <- data.frame(pca_result$x)

# Add cluster labels 
pca_scores$cluster <- km$cluster
loadings <- as.data.frame(pca_result$rotation)

# Biplot

# Save the plot to a PNG file
png("Fig 6. biplot 2D.png", width = 2000, height = 1800, res = 200)

# Increase the left margin for space
par(mar = c(5, 6, 4, 2))  # Adjust left margin for labels

# Create the biplot with smaller text size
biplot(pca_result, 
       ann = FALSE,  # Disable automatic annotation of labels
       col = c("white", "darkblue"), 
       xlim = c(-0.06, 0.06), 
       ylim = c(-0.06, 0.06))  # Ensure both x and y axis are visible

# Add title and axis labels with controlled text size
title(xlab = "Principal Component 1", 
      ylab = "Principal Component 2", 
      cex.lab = 1.2,  # Decrease axis label size
      cex.main = 1.5,  # Decrease title size
      font.lab = 2,    # Bold axis labels
      font.main = 2)   # Bold title

# Save and close the plotting device
dev.off()

# Vectors on the 3D plotly
# Extract PCA results
pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], PC3 = pca_result$x[, 3])

# Extract loadings (directions of the variables in PCA)
loadings <- pca_result$rotation

# Create the base 3D plot
pca_data$cluster <- as.factor(km$cluster)
#cluster_colours <- "1" = "#0D0887", "2" = 
point_colors <- cluster_colors[pca_data$cluster]

# Create the base 3D plot with colored points
fig <- plot_ly(data = pca_data, 
               x = ~PC1, y = ~PC2, z = ~PC3, 
               type = "scatter3d", mode = "markers", 
               marker = list(size = 5, color = point_colors, opacity = 0.7))  # Color points by cluster

# Add vectors (arrows for loadings) as lines
scaling_factor <- 50  # Adjust this factor to scale the vectors to a visible length
for (i in 1:ncol(loadings)) {
  fig <- fig %>%
    add_trace(
      x = c(0, loadings[1, i] * scaling_factor),  # Apply scaling factor to loadings for visibility
      y = c(0, loadings[2, i] * scaling_factor),
      z = c(0, loadings[3, i] * scaling_factor),
      type = "scatter3d",
      mode = "lines",  # Use "lines" to plot the vectors
      line = list(color = "red", width = 3)
    )
}

# Add labels for loadings with slight offset
fig <- fig %>%
  add_trace(
    x = loadings[1, ] * scaling_factor,  # Adjust label position based on the scaling
    y = loadings[2, ] * scaling_factor,
    z = loadings[3, ] * scaling_factor,
    type = "scatter3d",
    mode = "text",
    text = rownames(loadings),
    textfont = list(size = 12, color = "black"),
    showlegend = FALSE
  )

# Customize layout for better visibility
fig <- fig %>% layout(
  title = "PCA 3D Plot with Loadings Vectors",
  scene = list(
    xaxis = list(title = "PC1"),
    yaxis = list(title = "PC2"),
    zaxis = list(title = "PC3")
  )
)

# Show the plot
fig

# Save
htmlwidgets::saveWidget(fig, "Fig 7. biplot 3D interactive.html")


# Eigenvalues - says how much each principal component is important
eig.val <- (pca_result$sdev)^2
eig.val

# Variance of each principal component
variances <- (pca_result$sdev)^2  # Eigenvalues (variance explained)

# Proportion of variance explained by each component
prop_variance <- variances / sum(variances)  # Proportion of total variance explained

# Cumulative variance explained
cum_variance <- cumsum(prop_variance)

# Print the results
variances  # Eigenvalues (variance explained by each PC)
prop_variance  # Proportion of variance explained by each PC
cum_variance  # Cumulative variance explained by PCs

# Results for Variables
res.var <- get_pca_var(pca_data)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation
write.csv(controbutions, "Fig 8. Variable contributions.csv")

## Plot variable distribution by cluster

# Set up a results table
results_table <- data.frame(
  Variable = character(),
  Kruskal_p = numeric(),
  Pairwise_1 = numeric(),
  Pairwise_2 = numeric(),
  Pairwise_3 = numeric(),
  Pairwise_4 = numeric(),
  Pairwise_5 = numeric(),
  Pairwise_6 = numeric(),
  stringsAsFactors = FALSE
)

# Plot boxplots of unscaled data 
# Change the feature and disease labels and rerun

feature = "list.size" #change this variable by column name
disease = "list size"

png(paste0("Fig 9. By cluster ",disease, ".png"), width = 2000, height = 1800, res = 300)

ggboxplot(ml.data, x = "cluster", y = feature, 
          color = "cluster", palette = cluster_colors,
          order = c("1", "2", "3","4"),
          ylab = c(paste0("Prevalence of ", disease,"\n")), xlab = "Cluster",
          title = paste0("Prevalence of ", disease, " by cluster"))

dev.off()

png(paste0("Fig 9B. By cluster ",disease, ".png"), width = 2000, height = 1800, res = 300)

ggboxplot(ml.data, x = "cluster", y = feature, 
          color = "cluster", palette = cluster_colors,
          order = c("1", "2", "3","4"),
          ylab = c(paste0( disease,"\n")), xlab = "Cluster",
          title = paste0(disease, " by cluster"))

dev.off()

# Funtion to compute statistics and add result to table
append_test_results <- function(data, response_var, group_var = "cluster", p_adjust = "bonferroni") {
  # Kruskal-Wallis test
  kruskal_p <- kruskal.test(reformulate(group_var, response = response_var), data = data)$p.value
  
  # Pairwise Wilcoxon test
  pw <- pairwise.wilcox.test(data[[response_var]], data[[group_var]], p.adjust.method = p_adjust)
  
  # Extract pairwise comparisons (6 comparisons for 4 groups)
  pw_comparisons <- as.data.frame(as.table(pw$p.value))
  names(pw_comparisons) <- c("Group1", "Group2", "Pairwise_p")
  
  # Convert Group1 and Group2 to characters to ensure comparison works
  pw_comparisons$Group1 <- as.character(pw_comparisons$Group1)
  pw_comparisons$Group2 <- as.character(pw_comparisons$Group2)
  
  # Exclude self-comparisons where Group1 == Group2
  pw_comparisons <- pw_comparisons[pw_comparisons$Group1 != pw_comparisons$Group2, ]
  
  # Remove duplicates like "A vs B" and "B vs A"
  pw_comparisons <- pw_comparisons[!duplicated(t(apply(pw_comparisons[, c("Group1", "Group2")], 1, sort))), ]
  
  # Format pairwise comparisons into specific column names and comparison labels
  pairwise_p_values <- pw_comparisons$Pairwise_p
  comparisons <- paste(pw_comparisons$Group1, "vs", pw_comparisons$Group2)
  
  # Ensure exactly 6 pairwise columns (if there are fewer, fill with NA)
  if (length(pairwise_p_values) < 6) {
    pairwise_p_values <- c(pairwise_p_values, rep(NA, 6 - length(pairwise_p_values)))
    comparisons <- c(comparisons, rep(NA, 6 - length(comparisons)))
  }
  
  # Create a row for the results table
  new_row <- data.frame(
    Variable = response_var,
    Kruskal_p = kruskal_p,
    Pairwise_1 = pairwise_p_values[1],
    Pairwise_2 = pairwise_p_values[2],
    Pairwise_3 = pairwise_p_values[3],
    Pairwise_4 = pairwise_p_values[4],
    Pairwise_5 = pairwise_p_values[5],
    Pairwise_6 = pairwise_p_values[6],
    Pairwise_Comparison_1 = comparisons[1],
    Pairwise_Comparison_2 = comparisons[2],
    Pairwise_Comparison_3 = comparisons[3],
    Pairwise_Comparison_4 = comparisons[4],
    Pairwise_Comparison_5 = comparisons[5],
    Pairwise_Comparison_6 = comparisons[6],
    row.names = NULL
  )
  
  # Append the results to the global results table
  return(new_row)
}
  
new_result <- append_test_results(ml.data, feature)
results_table <- rbind(results_table, new_result)

# Save results table
write.csv(results_table, "Fig 10. Summary of statistics comparing cluster features.csv")


## Antiibiotic prescribing

# Set paths
path.data.prescribing <- "/Users/u1771937/Desktop/Data/Datasources/gpprescribing"

# Load prescribing data
included.months <- list("202304", "202305", "202306", "202307", "202308", "202309", "202310", "202311", "202312",
                        "202401","202402","202403")

for(i in included.months){
  setwd(path.data.prescribing) # set to prescribing data folder
  file = paste0("EPD_",i,".csv") # define file naming convention
  data <- fread(file = file, header = T, sep = ",") # read in data file
  data$section <- substr(data$BNF_CODE, 1, 4) # Get section codes only
  data <- data %>% filter(section == "0501")
  data1 <- setDT(data)[,.(ITEMS = sum(ITEMS)), by = .(PRACTICE_CODE)] # Aggregate
  assign(paste0("data_",i), data1) # keep as dataframe object
  rm(data, data1)
}

# Aggregate data across the year
dataset_names <- ls(pattern = "^data_")
combined_data <- do.call(rbind, lapply(dataset_names, function(name) get(name)))
data.rx <- aggregate(ITEMS ~ PRACTICE_CODE, data = combined_data, sum)

# Add to all data and calculate prescribing rate
gp_data_clean$cluster <- km$cluster
all.data3 <- all.data %>% select(PRACTICE_CODE, list.size, females)
all.data4 <- left_join(gp_data_clean, all.data3, by = c("list.size", "females"))
all.data5 <- all.data4 %>% left_join(data.rx, by = "PRACTICE_CODE") %>%
  mutate(rx.rate.per.1000 = 1000* ITEMS/list.size)

# Calculate median items by cluster and stats
#plot boxplots - unscaled, no outliers
feature="rx.rate.per.1000" #change this variable by column name
setwd("/Users/u1771937/Desktop/Supervision/HMS/2024-25/Harry Lee/data")
png(paste0("Fig 11. Antibiotic prescribing by cluster.png"), width = 2000, height = 1800, res = 300)

ggboxplot(all.data5, x = "cluster", y = feature, 
          color = "cluster", palette = cluster_colors,
          order = c("1", "2", "3","4"),
          ylab = c(paste0("Antibiotic prescriptions per 1000")), xlab = "Cluster",
          title = paste0("Antibiotic prescriptions per 1000, by cluster"),
          outlier.shape = NA) +
  ylim(0,1000)

dev.off()


#Kruskal-Wallis test
aggregate(rx.rate.per.1000 ~ cluster, all.data5, summary)
res.aov <- kruskal.test(rx.rate.per.1000 ~ cluster, data = all.data5) #ANOVA test #CHANGE Y variable
res.aov #results
PT = pairwise.wilcox.test(all.data5$rx.rate.per.1000,#CHANGE 
                          all.data5$cluster, 
                          p.adjust.method="bonferroni") #post-hoc pairwise comparisons
PT



## Mapping

# Collate data
map.data <- all.data5 %>% select(PRACTICE_CODE, cluster)

# Add postcode data
postcode <- read.csv("epraccur.csv", header = F) %>%
  rename("PRACTICE_CODE" = "V1") %>%
  rename("postcode" = "V10") %>%
  select(PRACTICE_CODE, postcode)

map.data <- map.data %>% left_join(postcode, by = "PRACTICE_CODE")

# Geocode
postcodes <- map.data$postcode

# postcode directory from ONA
# https://geoportal.statistics.gov.uk/datasets/6fb8941d58e54d949f521c92dfb92f2a/about

lookup <- fread("ONSPD_FEB_2025_UK.csv")
lookup$postcode <- gsub(" ", "", toupper(lookup$pcd))
map.data$postcode <- gsub(" ", "", toupper(map.data$postcode))
map.data2 <- map.data %>% left_join(lookup, by = "postcode")

# Get map
uk_map <- map_data("world", region = "UK")

# Plot map England
png(paste0("Fig 12. Map England.png"), width = 2000, height = 1800, res = 300)

ggplot() +
  geom_polygon(data = uk_map, aes(x = long, y = lat, group = group),
               fill = "grey90", color = "white") +
  geom_point(data = map.data2, 
             aes(x = long, y = lat, color = factor(cluster)), 
             size = .5, alpha = 0.7) +
  coord_quickmap() +
  scale_color_manual(values = cluster_colors) +
  theme_minimal() +
  ylim(49.5,56.5) +
  xlim(-6, 3) +
  labs(color = "Cluster", title = "Practice locations by cluster") +
  theme(
    axis.text = element_blank(),  # Hide axis text
    axis.ticks = element_blank(), # Hide axis ticks
    axis.title = element_blank(), # Hide axis titles
    panel.grid = element_blank(), # Remove grid lines
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = c(0.7, 0.7) 
  ) + 
  guides(color = guide_legend(override.aes = list(size = 4)))

dev.off()

# Interactive map

map.data2$cluster <- as.character(map.data2$cluster)
pal <- colorFactor(palette = cluster_colors, domain = unique(map.data2$cluster))

leaflet_map <- leaflet(map.data2) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long,
    lat = ~lat,
    color = ~pal(cluster),  # Use cluster directly now that types match
    radius = 4,
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste("Practice:", PRACTICE_CODE, "<br>Cluster:", cluster)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = unique(map.data2$cluster),  # Must match the domain
    title = "Cluster",
    opacity = 1
  )

# Save
saveWidget(leaflet_map, file = "Fig 13. Interactive map.html", selfcontained = TRUE)
