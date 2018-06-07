#All the packages required to run ArchFlow
packages <- c("archdata",
                      "plyr",
                      "dplyr",
                      "RcmdrMisc",
                      "compositions",
                      "ggplot2",
                      "ggfortify",
                      "ggthemes",
                      "plotrix",
                      "dendextend",
                      "d3heatmap",
                      "stargazer",
                      "devEMF",
                      "devtools",
                      "rmarkdown")
            
            
            new <- packages[!(packages %in% installed.packages()[,"Package"])]
            
            if(length(new)) install.packages(new)
            lapply(packages, require, character.only=TRUE)

#All the functions used to run ArchFLow
                       
            source("../../R_Fun/arch_dendro.R")
            source("../../R_Fun/arch_latex.R")
            source("../../R_Fun/arch_logcenter.R")
            source("../../R_Fun/arch_PCA.R")
            source("../../R_Fun/arch_pie_plot.R")
            source("../../R_Fun/arch_sum_table.R")
            source("../../R_Fun/arch_triangles.R")
            source("../../R_Fun/arch_uniformity.R")
            source("../../R_Fun/arch_varmat.R")
            source("../../R_Fun/classifica.R")
            source("../../R_Fun/entropia.R") 
            source("../../R_Fun/etiquetes_elements.R")
            source("../../R_Fun/arch_heatmap.R")
            source("../../R_Fun/arch_scatter_matrix.R")
            
            rm(new)
            rm(packages)
            
  
###BUGS: WHEN DOING STARGAZER FOR LATEX THE DIGITS ARE NOT ROUNDED IN RSD!!!
message("#All the packages were installed and loaded succesfull!
        #All the functions were loaded to the system succesfully!
          # The ArchFlow rmd files are ready to Run!")






