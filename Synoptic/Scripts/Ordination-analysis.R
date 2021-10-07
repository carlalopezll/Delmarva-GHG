# Script for testing out PCA and other ordination techniques
# Carla López Lloreda
# 10/6/2021


library(dplyr) # data manipulation
library(tidyr) # reshaping data functions
library(readr)
library(corrplot) # for correlation vis
library(ggplot2)
library(ggfortify)

All_2011 <- read_csv("2020-11/202011_Synoptic_all_new.csv")
head(All_2011)
str(All_2011)

All_2011$SO4_mg_L <- as.numeric(All_2011$SO4_mg_L)

cor <- cor(All_2011[ , -c(1:2)], use="pairwise.complete.obs")
head(cor)
corrplot(cor, method="circle")
ggsave("correlation matrix.jpg")

# Creating correlation matrix
corrplot(cor, method="number")

# Creating a PCA
Z <- princomp(na.omit(All_2011[ , -c(1:2)]), cor=TRUE, scores=TRUE)
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
