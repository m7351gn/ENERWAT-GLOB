ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, repos='http://cran.us.r-project.org')
}

packages <- c("tidyverse", "vroom", "openxlsx", "reshape2",
              "geosphere", "FNN", "rnaturalearth", 
              "terra", "tidyterra", "sf", "stars", "viridis",
              "ggpointdensity", "patchwork", "ggthemes", "ggtext",
              "pacman", "installr", "parallel")

ipak(packages)
