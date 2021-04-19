"arch_sum_table" <- function(x, signif = FALSE, decimals = 2, sig_digits = 3) {
  
     
  ##Obtain Mean, SD and RSD of the given dataframe
        sum <- #calculate the values using numSummary 
        numSummary(x, 
                   statistics=c("mean", "sd", "cv") #specify the stats to display
        )
      
      table	<- as.data.frame(sum$table)  	#extract only the table
      
      table$cv <- table$cv*100  #the cv  in % (RSD)
  
  
      
###Format number according to significant digits 
      
      if (signif == FALSE){
        
        
        #Set decimal digits for mean and sd
        row.names(table) -> row_names
        
        round(table$mean, decimals) -> mean_dec
        round(table$sd, decimals-1) -> sd_dec
        round(table$cv, 0) -> cv_dec
        
        cbind(mean_dec,sd_dec,cv_dec) -> table
        
        as.data.frame(table) -> table
        row.names(table) <- row_names #set back row names
        str(table)
        
        
      }
      
      
  ###Format number according to significant digits 
    if (signif == TRUE){
       
                 
              #Set significant digits for mean and sd
                
                row.names(table) -> row_names
                
                  #create a matrix with the desired significant digits
                  vec_sig_mean <- lapply(table[,c(1)], function(N)   formatC(signif(N, digits= sig_digits), digits = sig_digits, flag="#")) 
                  vec_sig_sd <- lapply(table[,c(2)], function(N)   formatC(signif(N, digits= sig_digits-1), digits = sig_digits-1, flag="#")) 
                  #change the matrix to dataframe
                  
                  df_sig_mean <- as.data.frame(vec_sig_mean) #df with mean
                  names(df_sig_mean) <- row_names
                  
                  df_sig_sd <-  as.data.frame(vec_sig_sd)  #df with sd
                  names(df_sig_sd) <- row_names
                  
                  rbind(df_sig_mean, df_sig_sd) -> df_mean_sd
                  as.data.frame(t(df_mean_sd)) -> df_mean_sd #truncate
                  
            
                #join in one df formated Mean, SD and RSD  
                  table <- cbind(df_mean_sd, round(table$cv,0))  #round the RSD
            
                row.names(table) <- row_names #set back row names

    }
  
      
  
      
      
      
  ##Add column with N (number of individuals) 
      # N of samples
        table$N <- sum$n
  
  
  #For datasets containing both compounds (e.g. BaO) and elements (e.g. Hf), arrange the rows to list the compounds at the beginning of the table
        
      #Name of the compounds
      table$izenak  <-  row.names(table)  	#keep the row.names(because arrange(){dplyr} removes them) 
      
      
        "element"  ->  table$what[nchar(row.names(table)) <= 2] #create a new column indicating which are elements (these contain two or less characters)
        "compound" ->  table$what[nchar(row.names(table)) > 2] #create a new column indicating which are compounds (these contain more than two characters)
    
        table <- arrange(table, what, izenak) ## arrange the row.names (izenak) according to what they are elements/compounds.  
      
       row.names(table) <- table$izenak 	#put it back the names of elements
      
   
  ##Setting names of the columns in the final table 
  
        colnames(table)[1] <-  "Mean"			#names of columns
        colnames(table)[2] <- "St. Dev"
        colnames(table)[3] <- "RSD"
        colnames(table)[4] <- "N"
  
  
    table <- table[,-c(5,6)] #remove the undesired columns 
    table <- table[,c(4,1,2,3)] # Change the order or columns to put the first N
    #table <- rbind(table, c(nrow(table),0, 0, 0))
    #row.names(table)[nrow(table)] <- "N. Samples"
  
  return(table)
  
}

