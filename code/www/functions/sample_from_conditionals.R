sample_from_conditionals <- function(model, n = 2, pois = FALSE){
  
  length_cond <- length(model$predicted)
  #n_per <- n/length_cond
  #print(paste(n_per, "samples will be taken at each level for a total of", n ,"samples"))
  
  samples <- c()
  for(i in seq_along(model$predicted)){
    if(!pois){
      #sd <-  model$std.error[i] * (1^(1/2))
      #sd <- sd = (model$conf.high)/(1.96*2)
      tmp <- rnorm(n, model$predicted[i], model$std.error[i])
    }else{
      tmp <- rpois(n, model$predicted[i])
    }
    
    
    samples <- c(samples, tmp)
  }
  
  return(samples)
}