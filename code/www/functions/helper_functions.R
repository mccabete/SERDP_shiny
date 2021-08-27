
glm_map <- read.csv("www/glm_names_map.csv")
installation_lookup_table <- read.csv("www/SERDP_data_installtion_lookup_table.csv")

# Helper data ----
installation_lookup_table <- read.csv("www/SERDP_data_installtion_lookup_table.csv")


# Tick data ----
tick_map <- sf::st_read("www/Tick_prevelence_absense_map.shp") #By default, this function abriviates column names? Renaming them does cause errors.
tick_map$pathogen_number <- tick_map$Hmn_pt_ * tick_map$Ticks
tick_map_pretty_names <- dplyr::rename(tick_map,"Installtion" = FULLNAM,
                                       "Total Ticks" = ttl_tck,
                                       "Human Pathogens Per Tick" = Hmn_pt_,
                                       "Trapping Effort" = trp_ffr,
                                       "Ticks Per Trap" = tcks_p_,
                                       "Tick Borne Disease Risk" = PxA,
                                       "Pathogens Detected" = pathogen_number
)
tick_map_pretty_names <- select(tick_map_pretty_names, c("Installtion",
                                                         "Total Ticks",
                                                         "Human Pathogens Per Tick",
                                                         "Trapping Effort",
                                                         "Ticks Per Trap",
                                                         "Tick Borne Disease Risk",
                                                         "Pathogens Detected"))

ticks <- read.csv("www/ticks.csv", stringsAsFactors = FALSE)
ticks_for_distribution <- select(ticks, c(
  "installation" ,
  "count"
))
ticks_for_distribution <- ticks_for_distribution %>%
  group_by(installation ) %>%
  summarise( count = sum(count))

ticks_for_distribution$installation <- data_name_to_formal(ticks_for_distribution$installation)


pathogens <- read.csv("www/pathogenicity_by_installation.csv", stringsAsFactors = FALSE)
#pathogens <- select(pathogens, c("Installation", "tbo", "Human", "Wildlife", "Domestic_Animals", "Endosymbiont", "Human_Endo","Human_Animal", "Unknown", "Vertebrate_Host","Disease"))
dung <- read.csv("www/dung.csv", stringsAsFactors = FALSE)
dung <- select(dung, c("installation", "species", "visit_year")) %>% na.omit()
dung <- dung[dung$installation %in% installation_lookup_table$data_name, ]
dung$installation <- data_name_to_formal(dung$installation)
dung <- dung %>% group_by(installation, species) %>% summarise(count = n())

# Vegetation data ----
plot_data <- read.csv("www/all_plotlevel_data.csv")

# SEM data ----
#path_data <- read.csv("www/path_analysis_data.csv")
#all_host_effects <- read.csv("www/all_hosts_sem_effects.csv") # Could change if we need it
load("www/tick_glmer.Rdata")
path_data <- read.csv("www/path_data.csv", stringsAsFactors = FALSE)
glm_map <- read.csv("www/glm_names_map.csv")
#state_vars_name <- c("Days Since Fire", "% Litter Cover", "Litter Depth",  "% Canopy Cover","Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit")

installation.name <- c("Avon Park Air Force Range", "Fort Benning", "Camp Blanding Army Base",
                       "Eglin Air Force Base", "Fort Gordon Army Base", "Fort Jackson Army Base", "Moody Air Force Base",
                       "Camp Shelby Joint Forces Training Center", "Tyndall Air Force Base")
state_vars_name <- c("Days Since Fire", "% Litter Cover", "Litter Depth",  "% Canopy Cover","Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit")
#stat_vars_sig <-  c("Days Since Fire", "% Litter Cover", "Litter Depth",  "% Canopy Cover","Standing Biomass g/(m^2)", "1 Year Vapor Pressure Deficit") # Need to modify by what is significant at predicting ticks?
path_data <- read.csv("www/path_data.csv", stringsAsFactors = FALSE)


sliderInput_var <- function(id) {
  id_var <- names_to_variables(id)
  tmp_nums <- var_trans_normal_units(path_data[[id_var]], id)
  id_name <- paste0("col", id_var)
  min <- min(tmp_nums) / 5
  max <- max(tmp_nums) * 5
  
  mean <- mean(tmp_nums)
  
  sliderInput(id_name, label = id, 
              min = round(min, digits = 1),
              max = round(max, digits = 1), 
              value = c(round(mean, digits = 1)))
  
}

sliderInput_ggpredict <- function(id) {
  
  tmp_nums <- var_trans_normal_units(path_data[[id]], id)
  min <- min(tmp_nums) / 5
  max <- max(tmp_nums) * 5
  
  shiny_id <- paste0("ggpredict_vals_", id)
  sliderInput(shiny_id, label = var_to_names(id), 
              min = round(min, digits = 1),
              max = round(max, digits = 1), 
              value = c(round(mean(tmp_nums), digits = 1), round(min(tmp_nums), digits = 1)))
}


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