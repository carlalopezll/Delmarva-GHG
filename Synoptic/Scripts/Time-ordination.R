library(vegan)
library(ggplot2)
library(geosphere)
library(dplyr)


# Set the working  directory
setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in data
full_synoptic <- read.csv("SynopticCurrent2.csv")

full_synoptic$Site

# Fix date
# Fixing date for field data

full_synoptic_SW$Date <- as.POSIXct(strptime(as.character(full_synoptic_SW$Date,"%d/%m/%Y"), format = "%Y%m%d"))

# Remove CO2 and CH4 (few sampling dates)
full_synoptic <- select(full_synoptic, -8, -9)

# Remove rivers
full_synoptic <- filter(full_synoptic, !Site == "CR" & !Site == "AG" & !Site == "TR")

# Filter for SW and GW
full_synoptic_SW <- filter(full_synoptic, Sample_Type == "SW")
full_synoptic_UW <- filter(full_synoptic, Sample_Type == "UW")

##### Dispersion of sites across time ####

# Calculating sites across time using betadisper

# Do I need to standardize my matrix????
full_synoptic_SW_std <- scale(full_synoptic_SW[ , 6:42], center=TRUE, scale=TRUE)

# Create the dissimilarity matrix for the full dataset
dis <- vegdist(full_synoptic_SW_std, method = "euclidean", na.rm = TRUE)

# Create groups using Site
group <- as.factor(full_synoptic_UW$Site)

# Calculating group dispersion

mod <- betadisper(dis, group)

plot(mod, ellipse = TRUE, hull = FALSE, main = "", lwd = 2, cex = 1, label.cex = 0.8)


# plot(x, axes = c(1,2), cex = 0.7,
#      pch = seq_len(ng), col = NULL, lty = "solid", lwd = 1, hull = TRUE,
#      ellipse = FALSE, conf,
#      segments = TRUE, seg.col = "grey", seg.lty = lty, seg.lwd = lwd,
#      label = TRUE, label.cex = 1,
#      ylab, xlab, main, sub, ...)

boxplot(mod)

# Creating a new column with the distance values for each point

full_synoptic_SW$dist <- mod$distances

ggplot(full_synoptic_SW, aes(x= Site, y= dist)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  labs(x = "Site", y= "Distance from centroid") +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"), axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

ggsave("Graphs/Distance boxplot.jpg")

ggplot(full_synoptic_SW, aes(x = Date, y = dist, color = Site)) +
  geom_point() +
  labs(x = "Date", y= "Distance from centroid") +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"), axis.text=element_text(size=9), axis.title=element_text(size=18,face="bold"))
ggsave("Graphs/Distance timeseries.jpg")

# Significance tests 

anova(mod)

# Response: Distances
#           Df    Sum Sq    Mean Sq F value Pr(>F)
# Groups    15 0.0007740 5.1601e-05  1.0656  0.398
# Residuals 98 0.0047458 4.8426e-05

permutest(mod, pairwise = TRUE, permutations = 99)

TukeyHSD(mod)

(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

# Exploring relationships of distance values with variables
# To ask: what is the most important variable driving this variability across time?

# Try a stepwise model selection

# lm <- lm(Cl_mg_L ~ ., data = full_synoptic_SW [ , 6:10])
#   
# step(lm)

ggplot(full_synoptic_SW, aes(x= SO4_mg_L, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "SO4 (mg/L)", y = "Distance from centroid") +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

summary(lm(SO4_mg_L ~ dist, data = full_synoptic_SW))
ggsave("Graphs/distance vs SO4.jpg")

ggplot(full_synoptic_SW, aes(x= NPOC_mgC_L, y = dist)) +
  geom_point() +
  labs(x = "Dissolved organic carbon (mg/L)", y = "Distance from centroid") +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

summary(lm(NPOC_mgC_L ~ dist, data = full_synoptic_SW))
ggsave("Graphs/distance vs DOC.jpg")

ggplot(full_synoptic_SW, aes(x= X54Fe_ppb, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Fe (ug/L)", y = "Distance from centroid") +
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"))

summary(lm(X54Fe_ppb ~ dist, data = full_synoptic_SW))
ggsave("Graphs/distance vs Fe.jpg")


# PERMANOVA on the centroids and distances

adon.results <- adonis (full_synoptic_SW_std ~ group, method = "euclidean", permutations = 999, na.rm = TRUE)
print(adon.results)

#             Df  SumsOfSqs  MeanSqs F.Model     R2 Pr(>F)    
# group      15  554770872 36984725  5.9016 0.4746  0.001 ***
# Residuals  98  614157000  6266908         0.5254           
# Total     113 1168927872                  1.0000           
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# # Creating a correlation matrix and looking at it
corr_Xi <- cor(full_synoptic_SW [ , 7:10])
# View(corr_Xi)
# names(Xi)
# head(Xi)
# 
# # Running the PCA and looking at it
Z <- princomp(full_synoptic_SW [ , 6:42], cor=TRUE, scores=TRUE) # Why would you want scores=FALSE?
# names(Z)
# Z
# 
# # Looking at weights (or eigenvalues = gives you the magnitude of vectors and variance explained by each)
# Z$sdev
# summary(Z)
# 
# # Accesing things in Z (the PCA)
# 
# Z[3]
# 
# # Looking at loadings (or eigenvectors= influence of each variable on all the PCs)
# Z$loadings


# Same but only for Jackson Lane sites

JL_synoptic <- filter(full_synoptic, Site_ID == "ND-SW" | Site_ID == "BD-SW" | 
                        Site_ID == "DK-SW" | Site_ID == "TS-SW")

dis_JL <- vegdist (JL_synoptic [ , 6:41], distance = "euclidean", na.rm = TRUE)

group_JL <- as.factor(JL_synoptic$Site_ID)

mod_JL <- betadisper(dis_JL, group_JL)

plot(mod_JL)
boxplot(mod_JL)

JL_synoptic$dist <- mod_JL$distances

ggplot(JL_synoptic, aes( x = Date, y = dist)) +
  geom_point()

# Calculating site across time using ordination distance

# 1. Create ordination
# 2. Measure Euclidean distance between dates
# 3. Add up distance for each observation (site)


#####################################################################################

# Perform k-means clustering on the lake data without lat/long + species ID column

K <- kmeans(full_synoptic_SW[ , 5:41], centers = 3)

# Looking at info within K
names(K)
K$cluster
centers <- K$centers   # these are the means of each variable in each cluster
# At the minimum, all cluster centres are at the mean of their Voronoi sets 
# (the set of data points which are nearest to the cluster centre).

K$withinss
# 4554045 9583115 5137897

K$size
#4 18 45

# Replace code to only be a 3-character ID (Ore, Pyg, Pal)
lake$SkistoCode <- substr(lake [ , 24], start= 1, stop= 3)

# Adding a column to dataframe with the cluster each observation falls in
full_synoptic_SW$cluster <- K$cluster

# Plotting species and cluster

ggplot(lake, aes(x= SkistoCode, y = cluster)) +
  geom_point()

# Create a dissimilarity matrix
dis <- vegdist(lake [ , 4:23], method='euclidean', binary=FALSE)

# Creating a hierarchical cluster from the previous dissimilarity matrix
hcluster <- hclust(dis, method = "average")
names(hcluster)
hcluster

# Looking into the hierarchical cluster info
hcluster$merge
hcluster$height
hcluster$labels
hcluster$labels <- lake$SkistoCode

# Plot the dendrogram
plot(hcluster)

# Looking at lat + long
# Include those columns in the clustering
K_spatial <- kmeans(lake[ , 2:23], centers = 3)
K_spatial$cluster
lake$spatial_cluster <- K_spatial$cluster

# Creating a dissimilarity matrix and plotting the dendogram
dis_spatial <- vegdist(lake [ , 2:23], method='euclidean', binary=FALSE)
hcluster_spatial <- hclust(dis_spatial, method = "average")
hcluster_spatial$labels <- lake$SkistoCode
plot(hcluster_spatial)

# Trying other linkage methods
# Nearest-neighbor or single
# The single linkage method (which is closely related to the minimal spanning tree) 
# adopts a ‘friends of friends’ clustering strategy.

hcluster2 <- hclust(dis, method = "single")
hcluster2$labels <- lake$SkistoCode
hcluster2$merge
plot(hcluster2)
