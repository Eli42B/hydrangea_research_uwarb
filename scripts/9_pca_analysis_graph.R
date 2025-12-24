############## PCA analysis graph ################

#-----------------------------load libraries

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(missMDA)     # for missing values in PCA analysis 
library(readr)       # for data reading 

# load data 
#--------------------------------------------

# we are using raw data rather than ranks here. Originally we tried ranking everything, but in the final manuscript we decided to go back to just a PCA plot and not giving out 'star' ratings  

d = read.csv(pca_filepath, header = TRUE) # make sure to re-load this, we want the cultivar to appear as a column name 

# Cleaning data 

rownames(d) <- iconv(rownames(d), from = "latin1", to = "UTF-8")
rownames(d) <- iconv(rownames(d), from = "WINDOWS-1252", to = "UTF-8")
# there are badly encoded characters that R is having a fit about, trying to fix
d = d %>% 
  clean_names()     # clean the names 
d[26,1] = "Invincibelle Wee White" # rename weird characters 
d = d[-28,] # remove Tardiva, looks like we lack data for it 

# Replace NA values with the mean photo insect count 

mean_insect_count = mean(d$photo_insect_count, na.rm = TRUE)
mean_insect_count 
# calculate hte mean of d$photo_insect_count 
# = 161.5588 
d$photo_insect_count[is.na(d$photo_insect_count)] <- mean_insect_count
# set the NAs as the mean 
# the days are already set with the average; I did it in Excel befor eI realized that doing it in R is a better best practice 

# Run the pca 
# ---------------------------------------------------------

#Run PCA 
res.pca = PCA(d[,2:6], graph = FALSE, scale.unit = TRUE)
# note: the scaling is important or it looks really funky
# only do a pca of column 1, that's the cultivars  

#Extract Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val 
#this handy thing grabs all the data we'll want to graph and sticks it in a dataframe, with each column representing a component 
var <- get_pca_var(res.pca)

#Graphing individuals 
ind <- get_pca_ind(res.pca)

#simplified pca biplot where abundance > 2 and colors are simplified 
p = fviz_pca_biplot(res.pca, repel = TRUE,
                    col.var = "#2E9FDF", # Variables color
                    col.ind = "#696969", # Individuals color
)
p # looks terrible 

# Trying to make it prettier 
####################################################################

#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# graphing by correlative power of each datapoint 
# -------------------------------------------------

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21,    # let's have them be circles
             fill = '#E7B800',   # coloring circles
             repel = TRUE        # avoid text overlapping 
)

# graphing by category 
# ------------------------------------------------

fviz_pca_ind(res.pca, 
             geom.ind = "point", 
             addEllipses = TRUE, 
             legend.title = "Species"
)  # oh, it's one giant ellipse 

# graph by two categories (species and shape) 
# ---------------------------------------------------

# we need to do this in ggplot2, fviz_pca_ind function can't do it 

# Extract PCA scores 
pca_scores = as_tibble(res.pca$ind$coord)

# we are going to have to change this dataframe pretty significantly 

pca_scores <- pca_scores %>%      
  rename(shannon = Dim.1, 
         abundance = Dim.2, 
         richness = Dim.3, 
         days = Dim.4) 
# rename the pca scores 

pca_scores$cultivar = d$cultivar
# add back the cultivar type 

dmerge = read.csv(dmergeFINAL_filepath, header = TRUE)
dmerge = dmerge %>%
  clean_names() %>% 
  dplyr::select(cultivar, species, flower_shape) %>% 
  distinct(cultivar, .keep_all = TRUE) 
pca_scores = pca_scores %>% 
  left_join(
    dmerge %>%  dplyr::select(cultivar, species, flower_shape),
    by = "cultivar"
  )
# bring in dmerge to get the cultivar species and flower shape
# clean it to remove duplicates 
# left join the pca scores and dmerge to add the cultivar species nad flower shape to the pca_scores dataframe 

# adding back our arrows (ie the loadings) 

var_coords = as_tibble(res.pca$var$coord)
var_coords$variable = rownames(var_coords)
var_coords$variable = c("shannon","abundance","richness","days", "photo_insect_count") 
# extracting arrow coordinates and saving as tibble

arrow_scale = 3          
var_coords = var_coords %>% 
  mutate(
    Dim.1 = Dim.1*arrow_scale, 
    Dim.2 = Dim.2*arrow_scale, 
    Dim.3 = Dim.3*arrow_scale, 
    Dim.4 = Dim.4*arrow_scale, 
    Dim.5 = Dim.5*arrow_scale
  )
# these arrows will look tiny on our plot 
# let's make them bigger by 3x, we can change this later 

# plotting just points and saving as a map 

points_plot = ggplot() + 
  # points 
  geom_point( 
    data = pca_scores, 
    aes(x = shannon, y = abundance, 
        color = species, 
        shape = flower_shape), 
    size = 3, alpha = 0.6) + 
  scale_color_manual(values = c("brown", "#00AFBB"))

points_plot

#adding arrows for variables 

arrows_plot = 
  geom_segment(
    data = var_coords, 
    aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), 
    arrow = arrow(length = unit(0.3,"cm")), 
    color = "black"
  ) 

points_plot + arrows_plot

# adding labels for the arrows 

label_plot = 
  geom_text(
    data = var_coords, 
    aes(x = Dim.1, y = Dim.2, 
        label = variable), 
    color = "black", 
    vjust = -0.5
  )
label_plot

points_plot + arrows_plot + label_plot
# pretty plot 

p
# basic pca plot 

points_plot + 
  arrows_plot + 
  labs(
    title = "Hydrangea Cultivar PCA Plot",
    subtitle = "Top performers clustered along the righthand sides"
  )
