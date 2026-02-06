##############################################
# Piechart 
##############################################

# load libraries 
# ------------------------------------------

library(dplyr)        # mutating stuff
library(ggplot2)      # graphing
library(scales)       # adds the 'percent' command



# load data 
#--------------------------------------------

# we are using raw data rather than ranks here. Originally we tried ranking everything, but in the final manuscript we decided to go back to just a PCA plot and not giving out 'star' ratings  

#d2024 = read.csv(piechart_filepath1, header = TRUE) 
#d2019 = read.csv(piechart_filepath2, header = TRUE) 
d = read.csv(barchart_piechart_filepath, header = TRUE)

#d2024 <- d2024 %>% 
#  clean_names()

#d2019 <- d2019 %>% 
#  clean_names()

# piechart 
# --------------------------------------------------------

gray_palette <- c(
  "#000000",  # black
  "#525252",  # dark gray
  "#717171",  # medium gray
  "#A3A3A3",  # light gray
  "#C8C8C8",  # very light gray
  "#E5E5E5"   # very very light gray
)

# basic piechart 

ggplot(d2019, aes(x = "", y = photo_count_2019_2022, fill = family)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  geom_text(aes(label = family), position = position_stack(vjust = 0.5)) 

# pie chart with only large slices labeled 

cutoff <- 0.05  # 5%

d2019 <- d2019 %>%
  mutate(
    n = photo_count_2019_2022,
    frac = n / sum(n),
    label_txt = if_else(frac >= cutoff, paste0(family, " (", percent(frac), ")"), NA_character_)
  )

ggplot(d2019, aes(x = "", y = n, fill = family)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  geom_text(
    aes(label = label_txt),
    position = position_stack(vjust = 0.5),
    size = 3.6,
    na.rm = TRUE              # silently drop NAs
  ) 
