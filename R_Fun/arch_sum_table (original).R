 "sum_table" <- function(x) {

	sum <- #calculate the values using numSummary 
	numSummary(x, 
			statistics=c("mean", "sd", "cv") #specify the stats to display
 				)
	
	table	<- as.data.frame(sum$table)  	#extract only the table
  
	table$cv <- table$cv*100  #the cv  in % (RSD)
		

	#3 significant digits for meand and sd
	row.names(table) -> row_names
	lapply(table[,c(1:2)], function(N)   formatC(signif(N, digits=3), digits = 3, flag="#")) -> table_ICP_3sig
	as.data.frame(table_ICP_3sig)-> table_ICP_df
	cbind(table_ICP_df, round(table[,c(3)], 2)) ->table
	row.names(table) <- row_names

	table$N <- sum$n

	table$izenak  <-  row.names(table)  	#keep the row.names(because arrange(){dplyr} removes them) 
      
      "element"  ->  table$what[nchar(row.names(table)) <= 2] #create a new column with elements
      "compound" ->  table$what[nchar(row.names(table)) > 2] #add the compounds to this column 

 
 	table <- arrange(table, what, izenak) ## arrange! first compounds and then elements!

 	row.names(table) <- table$izenak 	#put it back the names of elements

  colnames(table)[1]<-  "Mean"			#names of columns
	colnames(table)[2] <- "St. Dev"
	colnames(table)[3] <- "RSD"
	colnames(table)[4] <- "N"
 

table <- table[,-c(5,6)] #remove the undesired columns 


table <- table[,c(4,1,2,3)] # Change the order or columns to put the first N

return(table)

}

  