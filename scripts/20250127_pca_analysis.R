################################################################
#  DATA
################################################################

rm(list = ls(all = TRUE)) #blanks out your values 
dev.off()
getwd()
setwd("~/School/2024 Semester 1/Jude Ogden/Hydrangea_code/data in use")
d = read.csv("pca_analysis.csv", header = TRUE, row.names = 1)
d = d[-51,] #removing silver leaf 

################################################################
# LIBRARIES
################################################################

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#install.packages("ggplot2")
library(ggplot2)
#install.packages("FactoMineR")
#install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library("corrplot")


###############################################################

#Run PCA 
res.pca = PCA(d, graph = FALSE)
print(res.pca)

#Extract Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) #scree plot

#this handy thing grabs all the data we'll want to graph and sticks it in a dataframe, with each column representing a component 
var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

#Graphing Contribution 

head(var$contrib, 5) #bigger value of contribution is more important 
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)  

#Barchart version of contribution of variables by percentage 

# Contributions of variables to PC1:PC3
fviz_contrib(res.pca, choice = "var", axes = 1:3, top = 10)

# Contributions of variables to PC1# Contributions of variablabels.default()les to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC1 and PC2
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

#Graphing importance of different qualities 
res.pca = PCA(d, graph = TRUE) 
#unsurprisingly, stanardized shannon, abundane, and richness were all highly related to each other and affected dimension 1, while standardized days was slightly negatively correlated to the other three and mostly affected dimension 2. Variabls closer to the center are more important to includle in a model. We'll be including all of them, but standardized days should be weighted lower than the other ones. 

#####################################3
##   Graphs of individuals ## 
#######################################

ind <- get_pca_ind(res.pca)
ind

# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)
#simple plot (circle)
fviz_pca_ind(res.pca)
#simple circle  plot + colors 
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

#simplified color dot chart, where abundance > 2  
d2 = d[d$StandardizedAbundance >=2,]
res.pca = PCA(d2, graph = FALSE)
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
#simplified pca biplot where abundance > 2 and colors are simplified 
p = fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
)
p
p + theme(text = element_text(size = 15))

