to_oxides <- function(x){
  
  #convert the ppm values to wt% oxide values 
  #The conversion_factor table needs to be preloaded
  
  conversion_factors <- read_csv2("~/Programming/Github/arch_flow/R_fun/ox_ppm/conversion_factors.csv", col_names = TRUE)
  
  
    for (i in conversion_factors[,1]){ 	
    if(length(grep(i, colnames(x)))>0){
      colx <- grep(i, colnames(x))
      x[colx] <- round(x[colx] / conversion_factors[i,4] / 10000)
      
      colnames(x)[which(names(x) == "Al")] <- "Al2O3"
      colnames(x)[which(names(x) == "Fe")] <- "Fe2O3"
      colnames(x)[which(names(x) == "K")] <- "K2O"
      colnames(x)[which(names(x) == "P")] <- "P2O5"
      colnames(x)[which(names(x) == "Na")] <- "Na2O"
      
      print(i)
    }
  }
  return(x)
}











