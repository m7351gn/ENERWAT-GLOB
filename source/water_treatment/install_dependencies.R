ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, repos='http://cran.us.r-project.org')
}

packages <- c("dplyr", "vroom", "reshape2", "data.table",
              "tidyr", "stringr", "forcats",
              "drc",
              "FNN", "terra", "geosphere", 
              "parallel",
              "ggplot2", "patchwork", "cowplot", "grid", "gridExtra", "ggpointdensity",
              "viridis", "RColorBrewer", "ggtext", "ggthemes"
              )

ipak(packages)
