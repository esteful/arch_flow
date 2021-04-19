  arch_latex<- function(group_list, landscape = FALSE, MVC = FALSEM, rm_vars= rm_vars){
    
    cat(paste("%Copy and paste this code in the Latex File", "\n\n"))  
    j <- 1
    
    
    for (i in group_list){
      names(group_list)[j] -> i_name
      j+1 -> j
      
      message("%",i_name)
      stargazer(arch_sum_table(i[,-c(rm_vars)]), title = paste("Summary table of", i_name, sep = " "), summary = FALSE, digits = 1, label = paste("Sum_table", i_name, sep = "_"))
      
      
      
      if (MVC == TRUE){
        
        #Print the MVC in landscape mode
        if (landscape == TRUE){
          message("\\begin{landscape}")
        }
        stargazer(arch_varmat(i[,-c(rm_vars)]), title = paste("MVC of", i_name, sep = " "), summary= FALSE,  digits = 2, label = paste("MVC of", i_name, sep = "-"), font.size = "tiny")
        
        if (landscape == TRUE) {
          message("\\end{landscape}")} 
        
      }  
    }
  }
  