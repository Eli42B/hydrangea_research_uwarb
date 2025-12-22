############## Supplementary PCA analysis graph ################

#Setup
rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()

#-----------------------------load libraries

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#install.packages("ggplot2")
library(ggplot2)
#install.packages("FactoMineR")
#install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library("corrplot")
library(missMDA)     # for missing values in PCA analysis 
library(readr)       # for data reading 


#-------------------------------Load data

# d = read.csv("pca_analysis.csv", header = TRUE, row.names = 1)
# This uses our ranks, but we are switching back to raw counts 
d = read.csv(pca_filepath, header = TRUE, row.names = 1)
# d = d[-51,] #removing silver leaf (removed manually from original data in Ranking_culviars.csv) 

# there are badly encoded characters that R is having a fit about, trying to fix 
rownames(d) <- iconv(rownames(d), from = "latin1", to = "UTF-8")
rownames(d) <- iconv(rownames(d), from = "WINDOWS-1252", to = "UTF-8")
# i tried to fix it in the original csv file but R kept throwing a fit


#------------------------------Graphs 

#You can do a lot of other graphs that will help set this graph up / help us decide what graph to select. Full code is in the same folder. This code will only load the simplified final graph. 

#Run PCA 
res.pca = PCA(d, graph = FALSE)

#Extract Eigenvalues
eig.val <- get_eigenvalue(res.pca)

#this handy thing grabs all the data we'll want to graph and sticks it in a dataframe, with each column representing a component 
var <- get_pca_var(res.pca)

#Graphing individuals 
ind <- get_pca_ind(res.pca)

#simplified pca biplot where abundance > 2 and colors are simplified 
p = fviz_pca_biplot(res.pca, repel = TRUE,
                    col.var = "#2E9FDF", # Variables color
                    col.ind = "#696969", # Individuals color
)
p


# Chat gpt helped a lot iwth stuff below this  

# testing whether we want all five dimensions 

# 0) required packages ---------------------------------------------------


# 2) Inspect NAs and structure -------------------------------------------
str(d)
colSums(is.na(d))     # how many NAs per column
rowSums(is.na(d))     # how many NAs per row
summary(d)

# 3) Decide how to handle the NA column(s) ------------------------------
#Impute missing values with missMDA (better than mean imputation)
# Estimate number of components (ncp) for imputation
# note: estim_ncpPCA works on numeric matrices only; ensure your data frame is numeric
d_num <- as.data.frame(lapply(d, as.numeric))   # coerce to numeric 
rownames(d_num) <- rownames(d)

# estimate ncp (this might take a moment)
set.seed(42)
ncp_est <- missMDA::estim_ncpPCA(d_num, ncp.max = 5)  # try up to 5 components
print(ncp_est)   # check suggested ncp

# impute with the estimated ncp
imp <- missMDA::imputePCA(d_num, ncp = ncp_est$ncp)
d_imputed <- imp$completeObs   # fully imputed dataset

# 4) Run PCA on the imputed/cleaned matrix -------------------------------
res.pca <- PCA(d_imputed, graph = FALSE, scale.unit = TRUE)  # scale.unit=TRUE standardizes variables

# 5) Diagnostics & plots -------------------------------------------------
# eigenvalues
eig.val <- get_eigenvalue(res.pca); print(eig.val)

# variable contributions
var <- get_pca_var(res.pca)

# individuals and biplot
ind <- get_pca_ind(res.pca)
p <- fviz_pca_biplot(res.pca, repel = TRUE,
                     col.var = "#2E9FDF",    # Variables color
                     col.ind = "#696969")    # Individuals color

# optionally set a font family that supports your characters:
p + theme(text = element_text(size = 15, family = "Arial"))

# So what we are seeing is that 97% of hte variance is explained by dimensions 1-4, and 90% of hte variance is explained by Dim 1-3. So we should probalby drop dimension 5, it's kind of suspect data anyway with all hte missing variables, and we may wan tto drop dimension 4 too. 

# Cleaning up the graph since it's very busy 
# ------------------------------------------------------------------------

# select top 15 farthest cultivars in the top right and bottom right quadrants 
top15 = 

top15 <- ind_coords %>% arrange(desc(dist)) %>% slice(1:15) %>% pull(Cultivar)

# plot biplot, labeling only the top 15
p <- fviz_pca_biplot(res.pca, repel = TRUE,
                     col.var = "#2E9FDF",
                     col.ind = "#696969",
                     label = "var",                # keep all arrows labeled
                     invisible = "ind")            # hide all points temporarily

# now add points for all cultivars
p <- p + geom_point(data = ind_coords, aes(x = Dim.1, y = Dim.2), color = "#696969") +
  geom_text_repel(data = ind_coords %>% filter(Cultivar %in% top15),
                  aes(x = Dim.1, y = Dim.2, label = Cultivar),
                  color = "#696969")
p



###########################################################################
# Omit from graphing -- I tried out 3D and didn't like 
###########################################################################

# testing w 3 dimensions 
# ---------------------------------------------------------------------------

# loading smaller dataset 
d_small = d[ , 1:3]          # select just the first 3 columns 
d_small = na.omit(d_small)   # remove NAs 


#Run PCA 
res.pca = PCA(d_small, graph = FALSE)

#Extract Eigenvalues
eig.val <- get_eigenvalue(res.pca)

#this handy thing grabs all the data we'll want to graph and sticks it in a dataframe, with each column representing a component 
var <- get_pca_var(res.pca)

#Graphing individuals 
ind <- get_pca_ind(res.pca)

#simplified pca biplot where abundance > 2 and colors are simplified 
p = fviz_pca_biplot(res.pca, repel = TRUE,
                    col.var = "#2E9FDF", # Variables color
                    col.ind = "#696969", # Individuals color
)
p
p + theme(text = element_text(size = 15))

# nope I like the five dimensions better, they match better with what I observed in the field nad hte final rankings 

