
"entropia02" <-   function(df_chem)
  {
    # calculates the entropy of a dataset row by row
    # based on the code of Jaume Buxeda i Garrigòs

#Entropy:
#It contains the contributions of each element that is taken as a divisor in the ALR transformation to the entropy of total information. 
#In addition, it includes the value of the entropy of the information (in base logarithms 2) (H2)
#and the relative value that this entropy has with respect to the maximum that can reach for the number of dimensions (elements) that are take into account (H2%). 
#The entropy is calculated on the value τ.i once the total variation (vt) has been subtracted.


#Probability
#It includes the probabilities of each of the divisors estimated from the relative frequency 
#that each value τ.i represents with respect to the sum of all these values.


  n<-dim(df_chem)[1] #n of rows (individuals)
  d<-dim(df_chem)[2] #n of columns (elements/compounds)
  varmat<-matrix(0, n,d+2) #df for entropy
  varmat2<-matrix(0,n,d) #df for probability
 
  {
    # normalize to 1 all values in the dataframe
    varmat2<-as.matrix(sweep(df_chem,1,apply(df_chem,1,sum),FUN="/"))
  }
  
  for (j in 1:n) #for every row
  {
    for (k in 1:d) #for evert column
    {
      if (varmat2[j,k]==0) 
           varmat[j,k]<-0 
      else varmat[j,k]<- log((1/varmat2[j,k]))/log(2) #add the values 
        varmat[j,d+1] <- varmat[j,d+1]+ (varmat[j,k]*varmat2[j,k])
    }
  }
  
  varmat[,d+2]<-varmat[,d+1]/(log(d)/log(2))
  
  dimnames(varmat) <-list(c(dimnames(df_chem)[[1]]),
                          c(dimnames(df_chem)[[2]], 
                            "H2", "H2%"))
  
  list(Entropy=as.data.frame(varmat),
      Probability= as.data.frame(varmat2)) 
}






