# PCA on Delmarva synoptic data

library(corrgram)
library(corrplot)
library(ggfortify) # for autoplot
library(factoextra)
library(tidyr)

# Read in data

setwd("C:/Users/Carla López Lloreda/Dropbox/Grad school/Research/Delmarva project/Projects/Synoptic/Data")

# Read in data



# Filter sample types and rivers
SW <- filter(merge, Sample_Type == "SW")
SW <- filter(SW, !Site_ID == "CR-SW", !Site_ID == "AG-SW", !Site_ID == "TR-SW")

UW <- filter(merge, Sample_Type == "UW")

# Keeping just the variables we want and ones with few NAs
# Using select might be easier (in case the column numbers change)

str(SW)

SW_PCA <- select(SW, Site_ID, CO2_uM, CH4_uM, pH, SpC, DO_percent, Temp_C, Cl_mg_L,
                        SO4_mg_L, NPOC_mgC_L, d2H_VSMOW, d18O_VSMOW, DIC_mgC_L, TDN_mgN_L,
                        TDP_mgP_L)

SW_PCA <- SW[ , c(3, 5:6, 12:16, 27:32, 35:36, 41:50)]
SW_PCA <- SW_PCA[ , -c(9, 15, 17)]

UW_PCA <- UW[ , c(3, 5:6, 12:16, 27:32, 35:36, 41:50)]
UW_PCA <- UW_PCA[ , -c(9, 15, 17)]

#######################

# For surface water
# Creating a correlation matrix and plotting the corrolelogram
cor <- cor(na.omit(SW_PCA[2:23]))
view(cor)

# Plot a corrolelogram
corrplot(cor, method="circle", type="lower", na.label=" ")

testRes = cor.mtest(cor, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.05, addrect = 2, type = "lower")

# corrplot(cor, method="circle", type="lower", na.label=" ", order = 'FPC')

# For groundwater

cor <- cor(na.omit(UW_PCA[2:23]))
view(cor)

# Plot a corrolelogram
corrplot(cor, method="circle", type="lower", na.label=" ")

testRes = cor.mtest(cor, conf.level = 0.95)

## specialized the insignificant value according to the significant level
corrplot(cor, p.mat = testRes$p, sig.level = 0.05, addrect = 2, type = "lower")





# Running the PCA and looking at it
Z <- princomp(na.omit(SW_PCA[2:26]), cor=TRUE, scores=TRUE)
Z

# Looking at weights (or eigenvalues = gives you the magnitude of vectors and variance explained by each)
Z$sdev
summary(Z)

# Looking at loadings (or eigenvectors= influence of each variable on all the PCs)
Z$loadings

# Looking at scores in each PC
Z$scores[,1]

# Plot using lattice subsetted by year- VERY COOL
library(lattice)
xyplot(Z$scores[,2]~Z$scores[,1]|SW$Site, xlab='PC1', ylab='PC2')

# Plotting the PCA
autoplot(Z, data = na.omit(SW_PCA), colour = 'Site', loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, shape = FALSE,) + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))

fviz_pca_var(Z,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

fviz_pca_biplot(Z, label ="var")

fviz_pca_ind(Z,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(Z,
             col.ind = Site, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

# Scree plot: percentage explained by each of the PCs
fviz_eig(Z)

# Extracting PCA values
loadings <- as.data.frame(Z$loadings[,1:2])

SW_PCA$PC1 <- Z$scores[,1]
Z$scores[,1]
Z$loadings

#### PCA for groundwater ####

# Creating a correlation matrix and looking at it
cor <- cor(na.omit(UW_PCA[2:25]))
view(cor)

# Plot a corrolelogram
corrplot(cor, method="circle", na.label=" ")
ggsave("Correlation matrix_UW.jpg")

# Running the PCA and looking at it
Z <- princomp(na.omit(UW_PCA[2:25]), cor=TRUE, scores=TRUE)
Z

# Looking at weights (or eigenvalues = gives you the magnitude of vectors and variance explained by each)
Z$sdev
summary(Z)

# Looking at loadings (or eigenvectors= influence of each variable on all the PCs)
Z$loadings

# Looking at scores in each PC
Z$scores[,1]

# Plot using lattice subsetted by year- VERY COOL
library(lattice)
xyplot(Z$scores[,2]~Z$scores[,1]|UW$Site, xlab='PC1', ylab='PC2')

# Plotting the PCA
autoplot(Z, data = na.omit(UW_PCA), colour = 'Site', loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, shape = FALSE,) + 
  theme_bw() + theme(legend.position = "none", panel.border = element_blank(), 
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))

fviz_pca_var(Z,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

fviz_pca_biplot(Z, label ="var")
