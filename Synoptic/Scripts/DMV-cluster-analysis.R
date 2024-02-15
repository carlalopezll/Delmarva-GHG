library(vegan)
library(ggplot2)
library(dplyr)
library(factoextra)
library(ggpubr)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in the data

merge <- read.csv("Master spreadsheet.csv")

# For looking at all sample types
new_SW <- na.omit(merge[ , c("Site", "Site_ID", "Sample_Type", "watershed", "Date_corrected", "CO2_uM", "CH4_uM", "pH", "DO_mgL", "SpC", "Temp_C", "NPOC", "Cl", "SO4", "d18O", "d2H")])

# For looking at just SW
new_SW <- filter(new_SW, Sample_Type == "SW")


# Add watershed column

new_SW$watershed <- ifelse(new_SW$Site == "ND" | new_SW$Site == "BD" |
                              new_SW$Site == "TS" | new_SW$Site == "DK" | new_SW$Site == "FR", new_SW$watershed <- "Jackson Lane", 
                            ifelse(new_SW$Site == "TA" | new_SW$Site == "TB" | new_SW$Site == "DB" |
                                     new_SW$Site == "FN", new_SW$watershed <- "Tiger Paw",
                                   ifelse(new_SW$Site == "JA" | new_SW$Site == "JB" |
                                            new_SW$Site == "JC" | new_SW$Site == "NB", new_SW$watershed <- "Jones Road" , 
                                          ifelse(new_SW$Site == "OB" | new_SW$Site == "XB" |
                                                   new_SW$Site == "MB" | new_SW$Site == "HB", new_SW$watershed <- "Baltimore Corner", 
                                                 ifelse(new_SW$Site == "TI" | new_SW$Site == "QB" | new_SW$Site == "DF", new_SW$watershed <- "Baltimore Corner", NA)))))

# Do the cluster analysis

# CO2 

K <- kmeans(new_SW[ , 6], centers = 4)

# Looking at info within K
names(K)
K$cluster
centers <- K$centers   # these are the means of each variable in each cluster

# At the minimum, all cluster centres are at the mean of their Voronoi sets (the set of data points which are nearest to the cluster centre).

K$withinss
  # 4554045 9583115 5137897

K$size
  #4 18 45

# Adding a column to dataframe with the cluster each observation falls in
new_SW$cluster <- K$cluster

# Visualizing the clusters

# Dimension reduction using PCA
res.pca <- prcomp(new_SW[ , 6:16],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(K$cluster)
# Add Sample type (SW, GW, RI, CH) groups from the original data sett
ind.coord$Sample_Type <- new_SW$Sample_Type
# Add watershed variable
ind.coord$watershed <- new_SW$watershed
# Add site
ind.coord$Site <- new_SW$Site

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "watershed", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "%)" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "%)" )
) +
  stat_mean(aes(color = cluster), size = 4)

ggplot(new_SW, aes(x= watershed, y = cluster)) +
  geom_point()

ggsave("Graphs/CO2 Cluster analysis.jpg")
