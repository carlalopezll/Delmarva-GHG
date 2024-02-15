# Script for testing out PCA and other ordination techniques
# Carla López Lloreda
# Created: 10/6/2021, updated: 11/8/2021

library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr)
library(corrplot) # for correlation vis
library(ggplot2)
library(ggfortify)
library(vegan)

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")



# Subset out rivers
GHG <- filter(merge, !Site_ID == "CR-SW", !Site_ID == "AG-SW", !Site_ID == "TR-SW")
GHG_SW <- filter(GHG, Sample_Type == "SW")
GHG_UW <- filter(GHG, Sample_Type == "UW")

cor <- cor(GHG_SW[ , -c(1:6, 7:8, 11:12)], use="pairwise.complete.obs")

GHG_new <- GHG_SW[ , -c(1:4, 7:8, 11:12)]
head(cor)
corrplot(cor, method="circle", na.label=" ")
ggsave("correlation matrix.jpg")

# Creating correlation matrix
corrplot(cor, method="number")
corrplot(cor, method="shade")

# Creating a PCA
Z <- princomp(na.omit(GHG_SW[ , -c(1:4, 7:8, 11:12)], cor = TRUE, scores = TRUE))
Z
head(Z)

# Looking at weights (or eigenvalues = gives you the magnitude of vectors and variance explained by each)
Z$sdev
summary(Z)

# Component 1: 42%


# Looking at loadings (or eigenvectors= influence of each variable on all the PCs)
Z$loadings

# Looking at scores in each PC
Z$scores[,1]
Z$scores[,2]

plot(Z$scores[,1], Z$scores[,2], xlab='PC1', ylab='PC2', main='PC1 vs. PC2 for DMV 2020-11 synoptic')

# Plotting the PCA
biplot(Z, xlab = "PC1 (42%)", ylab = "PC2 (26%)")


autoplot(Z, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, shape = FALSE) + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))

plot(Z$scores[,1], CO2)



mod <- rda(na.omit(GHG_SW[ , -c(1:4, 7:8, 11:12)]), scale = TRUE)

## plot the PCA
plot(mod, scaling = 3)
