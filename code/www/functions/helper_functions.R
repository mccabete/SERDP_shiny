
data_name_to_formal <- function(data_name){
  length <- length(data_name)
  name <- rep(NA, length)
  for(i in 1:length){
    name[i] <- installation_lookup_table$Formal_name[data_name[i] == installation_lookup_table$data_name]
  }
  return(name)
}

subset_data <- function (data, installation_name, grep = FALSE){
  
  name <- installation_lookup_table$data_name[installation_name == installation_lookup_table$Formal_name]
  
  if(grep){
    
    similar_name_rows <- grep(name, data$plot_id) 
    data <- data[similar_name_rows,]
    
  }else{
    
    data <- filter(data, installation == name)
    
  }
  
  
  return(data)
}

## Helper Function for hypotheticals 
names_to_variables <- function(state_vars_name){
  name <- glm_map$glm_names[glm_map$state_vars_name == state_vars_name]
  return(name)
}

var_to_names <- function(glm_name){
  name <- glm_map$state_vars_name[glm_map$glm_names == glm_name]
  return(name)
}


variable_transform_plot <- function(p, reactive_vars){  # takes ggpredict object, returns a ggredict object with rawdata backtransformed 
  num_vars <- length(reactive_vars)
  no_transforms <- c("avg_litter_depth_all", "avg_canopy_cover", "avg_1yr_vp..Pa.")
  
  for(i in 1:num_vars){
    variable <- reactive_vars[i]
    
    if(!(variable %in% no_transforms)){
      
      rawdata <-  attr(p, "rawdata")
      at_list <- attr(p, "at.list")
      
      if(variable == "biomass_log"){
        
        if(i == 1){
          rawdata$x <- exp(as.numeric(rawdata$x))
          p$x <- exp(as.numeric(p$x))
        }else{
          
          rawdata$group <- as.character(exp(as.numeric(rawdata$group))) 
          at_list$biomass_log <- as.character(exp(as.numeric(at_list$biomass_log)))
          p$group <- as.character(exp(as.numeric(as.character(p$group))))
          
        }
        
        
      }
      
      if(variable == "d_since_fire_log"){
        
        
        if(i == 1){
          rawdata$x <- exp(as.numeric(rawdata$x))
          p$x <- exp(as.numeric(p$x))
        }else{
          rawdata$group <- as.character(exp(as.numeric(rawdata$group))) 
          at_list$d_since_fire_log <- as.character(exp(as.numeric(at_list$d_since_fire_log)))
          p$group <- as.character(exp(as.numeric(as.character(p$group))))
          
          
        }
        
      }
      
      if(variable == "logit_litter"){
        
        if(i == 1){
          rawdata$x <- exp(as.numeric(rawdata$x))/(1 + exp(as.numeric(rawdata$x)))
          p$x <- exp(as.numeric(p$x))/(1 + exp(as.numeric(p$x)))
        }else{
          rawdata$group <- as.character(exp(as.numeric(rawdata$group)) / (1 + exp(as.numeric(rawdata$group)))) 
          at_list$logit_litter <- as.character(exp(as.numeric(at_list$logit_litter))/ (1 + exp(as.numeric(at_list$logit_litter))))
          p$group <- as.character(exp(as.numeric(as.character(p$group))) / (1 + exp(as.numeric(as.character(p$group)))))
          
        }
        
      }
      attr(p, "rawdata") <- rawdata
      attr(p, "at.list") <- at_list
      
    }
    
  }
  
  return(p)
  
}

var_trans_normal_units <- function(vals, variable_name){
  
  var <- variable_name
  if(!(variable_name %in% glm_map$glm_names)){
    var <- names_to_variables(variable_name)
  }
  
  
  if(var == "logit_litter"){
    vals <- exp(vals)/(1 + vals)
  }
  if(var == "d_since_fire_log"){
    vals <- exp(vals)
  }
  if(var == "biomass_log"){
    vals <- exp(vals)
  }
  return(vals)  
  
}

var_trans_lm <- function(vals, variable_name){
  
  var <- variable_name
  if(!(variable_name %in% glm_map$glm_names)){
    var <- names_to_variables(variable_name)
  }
  
  
  if(var == "logit_litter"){
    vals <- log( vals/(1 - vals) )
  }
  if(var == "d_since_fire_log"){
    vals <- log(vals)
  }
  if(var == "biomass_log"){
    vals <- log(vals)
  }
  return(vals)  
  
}