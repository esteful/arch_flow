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
                       
            source("../../installation/arch_dendro.R")
            source("../../installation/arch_latex.R")
            source("../../installation/arch_logcenter.R")
            source("../../installation/arch_PCA.R")
            source("../../installation/arch_pie_plot.R")
            source("../../installation/arch_sum_table.R")
            source("../../installation/arch_triangles.R")
            source("../../installation/arch_uniformity.R")
            source("../../installation/arch_varmat.R")
            source("../../installation/classifica.R")
            source("../../installation/entropia.R") 
            source("../../installation/etiquetes_elements.R")
            source("../../installation/arch_heatmap.R")
            source("../../installation/arch_scatter_matrix.R")
            
            rm(new)
            rm(packages)
            
  
###BUGS: WHEN DOING STARGAZER FOR LATEX THE DIGITS ARE NOT ROUNDED IN RSD!!!
message("#All the packages were installed and loaded succesfull!
        #All the functions were loaded to the system succesfully!
          # The ArchFlow rmd files are ready to Run!")






