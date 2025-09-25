# Multiple imputation function


convert_fac<-function(data){
  
  data_m<- as.data.frame(data)
  
  data_m$a2 <- as.numeric(data_m$a2) 
  data_m$a2[data_m$a2==1] <- -1
  data_m$a2[data_m$a2==2] <- 1  
  
  return(data_m)
}

mi<- function(data, 
              m = 20,
              A = 200, 
              method)
{
  data_miss <- as.data.frame(data)
  
  data_m<- data_miss[,c(1:7)]
  
  data_m$a2 <- factor(data_m$a2) 
  
  pred <- make.predictorMatrix(data_m) 
  
  mi_data <- data_m %>%
    mice( m=m, maxit=5,
          predictorMatrix=pred,
          method = method ) %>%
    mice::complete("all") %>%
    lapply(function(y) convert_fac(y))
  
  mi_results<-results_miest(mi_data,n, m ,A)
  

  return(data.frame(mi_results))
  
}