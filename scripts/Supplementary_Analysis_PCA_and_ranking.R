# ######################################################################
#  PCA Analysis and Ranking 
# ######################################################################


########################################33
#SETUP 
##########################################

d = read.csv(pca_analysis_filepath, header = TRUE, row.names = 1)
d = d[-51,] #removing silver leaf 

################################################################
# LIBRARIES
################################################################

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library(FactoMineR)   # for pca analysis
library(factoextra)   # for pca analysis
library(corrplot)     # for plotting correlations 

###############################################################

#organizing data 

dmerge = read.csv(dmergeFINAL_filepath) #we've been heavily removing NAs from dmerge but for scatterplots we may want those back 
dmerge$TotalInflorescences_AB = as.numeric(dmerge$TotalInflorescences_AB)
dmerge$TotalInflorescences_AB = log(dmerge$TotalInflorescences_AB) #take natural log for inflorescnces, since our previous scatterplots showed that it is a logarithmic relationship 

#-----------------------------------------------------------------------

###################################################
# PCA Analysis - ranking importance of dimensions
###################################################

#Run PCA 
res.pca = PCA(d, graph = FALSE)
print(res.pca)

#Extract Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val #Relative importance of dimensions to PCA 
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) #scree plot


#this handy thing grabs all the data we'll want to graph and sticks it in a dataframe, with each column representing a component 
var <- get_pca_var(res.pca)
var
# Contributions to the principal components
head(var$contrib)

#############################
# Quartiles 
#############################

#summon data 

ranks = read.csv(pca_ranks_filepath, header = TRUE, row.names = 1) 

#Assign quantiles 
quantile(ranks$shannon)
quantile(ranks$abundance)
quantile(ranks$richness)
photos = na.omit(ranks$Photo_insect_count) #We need to omit hte NAs from the list of photos to get our quantiles 
quantile(photos)

############################
#Pearson correlation between 2019-2021 abundance and 2024 abundance
############################

dpearson = read.csv(pca_dpearson_filepath, header = TRUE) #call in the data. Oh oops we need to use spearman correlation because this is non-continuous, non-normally distributed ranked data rather than the original data.

r = cor(dpearson$Quantile.photo.abundance.data, dpearson$Quantile.2024.abundance.data, method = 'spearman')
r #level of correlation is 0.44, which is moderate  

