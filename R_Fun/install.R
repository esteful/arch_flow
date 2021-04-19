#All the packages required to run ArchFlow

#First of all, make sure the working directory is the correct one. The same as the .Rproj file.
getwd() -> current_wd
setwd(current_wd)


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
            #watch out the difference between installed.packages and install.packages
            if(length(new)) install.packages(new)
            lapply(packages, require, character.only=TRUE)

#All the functions used to run ArchFLow
                       
            source("../arch_flow/R_Fun/arch_latex.R")
            source("../arch_flow/R_Fun/arch_dendro.R")
            source("../arch_flow/R_Fun/arch_logcenter.R")
            source("../arch_flow/R_Fun/arch_PCA.R")
            source("../arch_flow/R_Fun/arch_pie_plot.R")
            source("../arch_flow/R_Fun/arch_sum_table.R")
            source("../arch_flow/R_Fun/arch_triangles.R")
            source("../arch_flow/R_Fun/arch_evenness.R")
            source("../arch_flow/R_Fun/arch_varmat.R")
            source("../arch_flow/R_Fun/classifica.R")
            source("../arch_flow/R_Fun/entropia.R") 
            source("../arch_flow/R_Fun/etiquetes_elements.R")
            source("../arch_flow/R_Fun/arch_heatmap.R")
            source("../arch_flow/R_Fun/arch_scatter_matrix.R")
            source("R_Fun/arch_mahala")

            
            rm(new)
            rm(packages)
            
  


message("The following folder is the working directory of this project:")
print(getwd())

message("#All the packages were installed and loaded succesfully! #All the functions were loaded to the system succesfully! # The ArchFlow rmd files are ready to run!")

#sessionInfo() 






