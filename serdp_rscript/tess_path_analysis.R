## Path analysis


library(tidyverse)
library(sjPlot)
library(patchwork)
library(MuMIn)
library(DHARMa)
library(lme4)
library(piecewiseSEM)
library(effectsize)
library(sjPlot)
library(ggeffects)

sem_data <- read_csv("/projectnb/dietzelab/mccabete/SERDP_shiny/code/www/path_analysis_data.csv")


## Data prep ####
sem_data %>% 
  select(deer:total_clusters1m) %>% 
  colSums( ) %>% 
  knitr::kable()

sem_data <- sem_data %>%
  mutate(biomass_log = log(avg_dry_standing_gm2),
         d_since_fire_log = log(d_since_fire),
         logit_litter = car::logit(avg_pct_litter)
  )


sem_data$tpt <- ceiling(sem_data$ticks_per_trap)


## Tick Models ####
# tpt_pois <- glmer(
#   tpt ~ d_since_fire_log + logit_litter + avg_litter_depth_all + avg_canopy_cover + biomass_log + total_clusters1m + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
#   data = sem_data, na.action = "na.fail",
#   family = poisson(link = "log"),
#   control=glmerControl(optimizer = "Nelder_Mead", optCtrl=list(maxfun=1e6))
# )
# plot(tpt_pois)
# simulateResiduals(tpt_pois) %>% plot()
# summary(tpt_pois)
# effectsize::standardize(tpt_pois) %>% summary()
# coefs(tpt_pois)
# plot_model(tpt_pois, show.values = T)
# plot_model(tpt_pois, type = "pred", show.data = T, grid = T)

tpt_noHosts_pois <- glmer(
  tpt ~ d_since_fire_log + logit_litter + avg_litter_depth_all + avg_canopy_cover + biomass_log + avg_1yr_vp..Pa. + (1|inst_name/plot_id),
  data = sem_data, 
  family = poisson(link = "log"),
  na.action = "na.fail",
  control=glmerControl(optimizer = "Nelder_Mead", optCtrl=list(maxfun=1e6))
)


## Host? ####
host_mod_all <- glmer(
  total_clusters1m ~ d_since_fire_log + avg_canopy_cover + logit_litter + avg_litter_depth_all + (1 | inst_name/plot_id), 
  data = sem_data, family = poisson(link = "log"),
  control = glmerControl(optimizer="Nelder_Mead", optCtrl=list(maxfun=1e6))
)
coefs(host_mod_all)
simulateResiduals(host_mod_all) %>% plot()
car::vif(host_mod_all)

## Litter/understory models ####
litter_cover_mod <- lmer(logit_litter ~ d_since_fire_log + avg_canopy_cover + (1|inst_name/plot_id),
                         data = sem_data)
coefs(litter_cover_mod)

litter_depth_mod <- lmer(avg_litter_depth_all ~ d_since_fire_log + avg_canopy_cover + (1|inst_name/plot_id), 
                         data = sem_data)
coefs(litter_depth_mod)



## Standing Biomass #####
standing_biomass_mod <- lmer(biomass_log ~ d_since_fire_log + avg_canopy_cover + (1|inst_name/plot_id), data = sem_data)
# plot(standing_biomass_mod)
simulateResiduals(standing_biomass_mod) %>% plot()
summary(standing_biomass_mod)


## Tree (overstory) canopy cover ####
# pine_mod <- lmer(logit_pct_pine ~ fri15yr_log + avg_canopy_cover +(1|inst_name/plot_id), 
#                  data = sem_data)
canopy_mod <- lmer(avg_canopy_cover ~ d_since_fire_log + fri15yr + (1|inst_name/plot_id), 
                   data = sem_data)


## Fire regime models ####


time_since_fire_mod <- lmer(d_since_fire_log ~ fri15yr + (1|inst_name/plot_id), data = sem_data)
simulateResiduals(time_since_fire_mod) %>% plot()
plot(time_since_fire_mod)
summary(time_since_fire_mod)
plot_model(time_since_fire_mod, show.values = T)

fri_mod <- lmer(fri15yr ~ cv_30yr_fire_days + (1 | inst_name/plot_id), 
                data = sem_data)
simulateResiduals(fri_mod) %>% plot()
plot(fri_mod)

sem_data$fri15yr_int <- ceiling(sem_data$fri15yr)

# plot_model(fri_mod, type = "pred", show.data = T)
# plot_model(fri_mod_pred, type = "pred", show.data = T)

## Weather ~ Climate model
avg_1yr_vp_mod <- lmer(avg_1yr_vp..Pa. ~ cv_30yr_fire_days + (1|inst_name), data = sem_data)
# simulateResiduals(avg_1yr_vp_mod) %>% plot()
# summary(avg_1yr_vp_mod)



## Plotting Sub models ####

sem_df <- as.data.frame(sem_data)




tpt_mod_pred <- plot_model(tpt_noHosts_pois, type = "pred", show.data = F)
tpt_mod_pred_grid <- plot_model(
  tpt_noHosts_pois, type = "pred", show.data = F, grid = T
)
tpt_mod_pred_grid + xlab(" ") + ylab("Tick count")

tpt_mod_pred_grid$data <- tpt_mod_pred_grid$data %>% 
  filter(!group_col %in% c("total_clusters1m", "avg_canopy_cover","d_since_fire_log")) %>% 
  mutate(group = case_when(
    group == "logit_litter" ~ "Litter cover (logit)",
    group == "avg_litter_depth_all" ~ "Litter depth (cm)",
    group == "biomass_log" ~ "Standing biomass (log)",
    group == "avg_1yr_vp..Pa." ~ "Vapor pressure (Pa)"
  )
  )
tpt_mod_grid_fig <- tpt_mod_pred_grid + 
  xlab(" ") + ylab("Predicted tick count") + 
  theme_bw()
#ggsave("figures/tpt_allhosts_marginal.pdf",tpt_mod_grid_fig, width = 5, height = 5, dpi = 300)

tpt_lcov <- tpt_mod_pred$logit_litter +
  xlab("Litter cover (logit)") + ylab("Predicted tick count") + ggtitle(" ") +
  theme_classic()
tpt_ldep <- tpt_mod_pred$avg_litter_depth_all +
  xlab("Litter depth (cm)") + ylab("Predicted tick count") + ggtitle(" ") +
  theme_classic()
tpt_ccov <- tpt_mod_pred$avg_canopy_cover +
  xlab("Canopy cover (%)") + ylab("Predicted tick count") + ggtitle(" ") +
  theme_classic()
tpt_vprs <- tpt_mod_pred$avg_1yr_vp..Pa. +
  geom_point(data = sem_df, aes(avg_1yr_vp..Pa., tpt)) +
  xlab("Vapor pressure (Pa)") + ylab("Predicted tick count") + ggtitle(" ") +
  theme_classic()
tpt_lcov + tpt_ldep + tpt_ccov + tpt_vprs





lcov_mod_pred_grid <- plot_model(
  litter_cover_mod, type = "pred", show.data = F, grid = T
)
lcov_mod_pred_grid + xlab(" ") + ylab("Litter cover (logit)")

lcov_mod_pred_grid$data <- lcov_mod_pred_grid$data %>% 
  mutate(group = case_when(
    group == "avg_canopy_cover" ~ "Canopy cover (%)",
    group == "d_since_fire_log" ~ "Days since fire (log)"
  )
  )
lcov_mod_pred_grid <- lcov_mod_pred_grid + 
  xlab(" ") + ylab("Predicted litter cover (logit)") + 
  theme_bw()


ldep_mod_pred_grid <- plot_model(
  litter_depth_mod, type = "pred", show.data = F, grid = T
)
ldep_mod_pred_grid$data <- ldep_mod_pred_grid$data %>%
  filter(group == "d_since_fire_log") %>% 
  mutate(group = "Days since fire (log)")
ldep_mod_pred_grid <- ldep_mod_pred_grid + 
  xlab(" ") + ylab("Predicted litter depth (cm)") + 
  theme_bw()


sbio_mod_pred_grid <- plot_model(
  standing_biomass_mod, type = "pred", grid = T
)
sbio_mod_pred_grid$data <- sbio_mod_pred_grid$data %>% 
  filter(group == "avg_canopy_cover") %>% 
  mutate(group = "Canopy cover (%)")
sbio_mod_pred_grid <- sbio_mod_pred_grid +
  xlab(" ") + ylab("Predicted understory biomass (log)") +
  theme_bw()

lcov_mod_pred_grid / (ldep_mod_pred_grid + sbio_mod_pred_grid)
#ggsave("figures/veg_vars_allhosts_marginal.pdf", width = 5, height = 5, dpi = 300)


canp_mod_pred_grid <- plot_model(
  canopy_mod, type = "pred", grid = T
)
canp_mod_pred_grid$data <- canp_mod_pred_grid$data %>% 
  filter(group == "d_since_fire_log") %>% 
  mutate(group = "Days since fire (log)")
canp_mod_pred_grid <- canp_mod_pred_grid +
  xlab(" ") + ylab ("Predicted canopy cover (%)") +
  theme_bw()

canopy_pred_raw <- ggpredict(canopy_mod, type = "re",  )

fire_mod_pred_grid <- plot_model(
  time_since_fire_mod, type = "pred", grid = T
)
fire_mod_pred_grid$data <- fire_mod_pred_grid$data %>% 
  mutate(group = "Fire return interval (years)")
fire_mod_pred_grid <- fire_mod_pred_grid +
  xlab(" ") + ylab("Predicted log-days since fire") +
  theme_bw()

fri_mod_pred_grid <- plot_model(
  fri_mod, type = "pred", grid = T
)
fri_mod_pred_grid$data <- fri_mod_pred_grid$data %>% 
  mutate(group = "30-y Average CV of Fire Days")
fri_mod_pred_grid <- fri_mod_pred_grid +
  xlab(" ") + ylab("Predicted fire return interval (years)") +
  theme_bw()

canp_mod_pred_grid + fire_mod_pred_grid + fri_mod_pred_grid
#ggsave("figures/can_fire_vars_allhosts_marginal.pdf", width = 7, height = 3, dpi = 300)



## No hosts pSEM ####
#m <- effectsize::standardize(tpt_noHosts_pois)
noHost_psem <- psem(
  tpt_noHosts_pois,
  # m,
  litter_cover_mod, litter_depth_mod, canopy_mod,
  standing_biomass_mod,
  time_since_fire_mod, fri_mod,
  avg_1yr_vp_mod,
  
  data = sem_df
)
noHosts_psem_summary <- summary(noHost_psem, .progressBar = FALSE)
noHosts_psem_summary







## Calculate indirect effects on tick abundance ####

## No hosts model
noHosts_psem_summary$coefficients
time_since_fire_i <- (.39*.51) + (.4*.24) + (.17*.4*.51) + (.17*-.37*.27)
canopy_cover_i <- (.4*.51) + (-.37*.27)
FRI_i <- (.58*.17*.4*.51) + (.58*.17*-.37*.27) + (.58*.39*.51) + (.58*.4*.24)
cv_fire_days_i <- (.1*.58*.17*.4*.51) + .1*(.58*.17*-.37*.27) + .1*(.58*.39*.51) + .1*(.58*.4*.24)

tpt_effects_noHosts <- tibble(
  variable = c("CV fire days", "FRI", "time since fire", "canopy cover", 
               "litter cover", "litter depth", "standing biomass", 
               "vapor pressure"),
  direct = c(0, 0, 0, 0, .51, .24, .27, .51),
  indirect = c(cv_fire_days_i, FRI_i, time_since_fire_i, canopy_cover_i, 0, 0, 0, 0),
  total = direct + indirect
)

write_csv(tpt_effects_noHosts, "/projectnb/dietzelab/mccabete/SERDP_shiny/code/www/no_hosts_sem_effects.csv")

tpt_effects_noHosts %>% 
  knitr::kable(digits = 3, caption = "No hosts model effects on tick abundance")




## Tess Messing with stuff ####

vpd_conditionals <- c(10, 50, 100, 1000, 2000, 2500)

biomass_log_level <- c(1,2,3,4)
avg_canopy_cover_level <- c(0.1, 0.2, 0.5, 0.9, 1)
avg_litter_depth_all_level <- c(0.5,2,5,10)
logit_litter_level <- c(1,2,3,10)
d_since_fire_log_level <- c(10, 30, 60, 100)

#test_predict <- ggpredict(tpt_noHosts_pois, type = "re", terms = c("avg_1yr_vp..Pa. [vpd_conditionals]", "biomass_log [biomass_log_level]", "avg_canopy_cover, [avg_canopy_cover_level]","logit_litter [logit_litter_level" ))
test_predict <- ggpredict(tpt_noHosts_pois, type = "re", terms = c("avg_1yr_vp..Pa.", "biomass_log"))
 
small <- sem_data %>% 
  select(avg_dry_standing_gm2, tpt, biomass_log) %>%
  filter(biomass_log == any(as.numeric(as.character(test_predict$group))))

plot(test_predict)
ggplot2(test_predict) %>%
  geom_point(aes(x = x, y = test_predict))

exp_short <- function(x){
  print(paste(class(x)))
 #x <- exp(x)
 x <- exp(x)

  return(values_at(x, "meansd"))
}

tidy_test <- get_model_data(tpt_noHosts_pois, type = "pred",pred.type = "re",  terms = c("avg_1yr_vp..Pa.", "biomass_log [exp]"), show.data = TRUE)

test_plot <- ggpredict(tpt_noHosts_pois, type = "re",  terms = c("avg_1yr_vp..Pa.", "biomass_log"))
vpd_data_frame <- as_tibble(test_plot$avg_1yr_vp..Pa.)



#p <- plot_model(tpt_noHosts_pois, type = "pred",pred.type = "re",  terms = c( "avg_1yr_vp..Pa.","biomass_log"),  show.data = TRUE, back.transform = TRUE)
#p <- plot_model(tpt_noHosts_pois, type = "pred",pred.type = "re", show.data = TRUE)


 #p$mapping$colour[[2]] <- "group_col" # If placed before running the group changes, get Discrete value error. If at end, get a
p <-  ggpredict(tpt_noHosts_pois, type = "re",  terms = c("avg_1yr_vp..Pa.", "biomass_log [40, 200]"))
 rawdata <-  attr(p, "rawdata")
  rawdata$group <- exp(rawdata$group)
  attr(p, "rawdata") <- rawdata
  
  at_list <- attr(p, "at.list")
  at_list$biomass_log <- exp(at_list$biomass_log)
  attr(p, "at.list") <- at_list
  p$group <- as.character(exp(as.numeric(as.character(p$group))))
  #p$group_col <-  as.character(exp(as.numeric(as.character(p$group_col))))
  
  
  
  plt <- plot(p, rawdata = TRUE) + 
  ggtitle("") + 
    xlab(paste("Varibale Pretty Name")) + 
    ylab("Predicted Ticks Per Trap") + 
    labs(color = "variable 2 pretty name") #+ 
    #geom_point(data = rawdata, aes(x = x, y = response)) 
  plt
 # p$data$group <- exp(as.numeric(as.character(p$data$group)))
 # p$data$group_col <-  exp(as.numeric(as.character(p$data$group_col)))
 # p$layers[[1]]$data$group <- exp(as.numeric(as.character(p$layers[[1]]$data$group)))
 # p$layers[[1]]$data$group_col <- exp(as.numeric(as.character(p$layers[[1]]$data$group_col)))
 
 
 #p$layers[[2]]
 #p$mapping$colour[[2]] <- "~group_col"

 #p$labels$group <- "group"
 #p
#rawdata$biomass <- exp(rawdata$group)
#rawdata$group_col <- p$data$group_col

p <- p + 
ggtitle("") + 
  xlab(paste("Varibale Pretty Name")) + 
  ylab("Predicted Ticks Per Trap") + 
  labs(color = "variable 2 pretty name") + 
geom_point(data = rawdata, aes(x = x, y = response)) 


p  


  p$layers[[1]]$data$group <- exp(as.numeric(as.character(p$layers[[1]]$data$group)))
  p$layers[[1]]$data$group_col <-exp(as.numeric(as.character(p$layers[[1]]$data$group_col)))
 
  
 


fire <- ggpredict(fri_mod, type = "re")

test_mean <- ggemmeans(tpt_noHosts_pois, type = "re", terms = c("avg_1yr_vp..Pa.", "d_since_fire_log [6.65]"))
plot(test_mean)
test_mean

test_mean2 <- ggemmeans(tpt_noHosts_pois, type = "re", terms = c("avg_1yr_vp..Pa."))
#plot(test_mean)
test_mean2

## Exporting glmer objects for use in shiny ####
#tick_glmer <- tpt_noHosts_pois
#save(tick_glmer, file = "/projectnb/dietzelab/mccabete/SERDP_shiny/code/www/tick_glmer.Rdata")
#write_csv(sem_data, file = "/projectnb/dietzelab/mccabete/SERDP_shiny/code/www/path_data.csv")

cov_names  <- c("avg_1yr_vp..Pa.", "avg_litter_depth_all")
cov_xlab <- cov_names[1]
cov_group_lable <- cov_names[2]

p <-  ggpredict(tpt_noHosts_pois, type = "re",  terms = c("avg_1yr_vp..Pa.", "avg_litter_depth_all [100]"))
 
new_p <- variable_transform_plot(p, cov_names)
plt <- plot(new_p, rawdata = TRUE) + 
  geom_path(aes(, group = 1)) + 
  ggtitle("") +
  xlab(paste(cov_xlab)) + 
  ylab("Predicted Ticks Per Trap") +
  labs(color = paste(cov_group_lable)) #+ 
plt

### Setting up the cummulative effects plot





  
  paste_var <- function(var, vals = NULL){
    var <- names_to_variables(var)
    
    if( ! rlang::is_empty(vals) ){
      p_var <- paste0( var, " [", paste(vals, collapse = ","), "]")
    }
  }

vals_y <- c(1, 100)
vals_x <- c(1, 3000)
var1 <- "Litter Depth"
var2 <- "% Canopy Cover"



fire_levels <- c(9, 10, 11) ## Need a way of taking the 1 value a user puts in and turning it into three vals. rnorm statement? 
percent_canopy <- ggpredict(canopy_mod, type = "re", terms = "d_since_fire_log[fire_levels]")
percent_canopy_baseline <- ggpredict(canopy_mod, type = "re", terms = c("d_since_fire_log [quart2]")) # Excluding "fri15yr" since not significant 
tmp_canopy <- values_at(percent_canopy$predicted, values = "quart2") #, percent_canopy$conf.low, percent_canopy$conf.high) ## Why include high and low confidence intervals? I think it may throw sampleing off
#tmp_canopy <- quantile(percent_canopy$predicted, seq(from = 0.1, to = 0.99, length = 11)) 

plot_comparison_ggplot(percent_canopy, percent_canopy_baseline, title = "Predicted % Canopy")

biomass <- ggpredict(standing_biomass_mod, type = "re", terms = "avg_canopy_cover[tmp_canopy]") # Excluding "d_since_fire_log" since not significant 
biomass_baseline <- ggpredict(standing_biomass_mod, type = "re", terms = c("avg_canopy_cover  [quart2]"))
tmp_biomass <- values_at(biomass$predicted, values = "quart2")

plot_comparison_ggplot(biomass, biomass_baseline, "Predicted Biomass")

logit_litter <- ggpredict(litter_cover_mod, type = "re", terms = c("avg_canopy_cover [tmp_canopy]", "d_since_fire_log [fire_levels]"))
logit_litter_baseline <- ggpredict(litter_cover_mod, type = "re", terms = c("avg_canopy_cover [quart2]", "d_since_fire_log [quart2]"))
tmp_logit <- values_at(logit_litter$predicted, values = "quart2") 

plot_comparison_ggplot(logit_litter, logit_litter_baseline, "Predicted Litter Cover")

 #c(logit_litter$predicted)#, logit_litter$conf.low, logit_litter$conf.high)
litter_depth <- ggpredict(litter_depth_mod, type = "re", terms = "d_since_fire_log [fire_levels]")
litter_depth_baseline <- ggpredict(litter_depth_mod, type = "re", terms = "d_since_fire_log [quart2]")

plot_comparison_ggplot(litter_depth, litter_depth_baseline, title = "Predicted Litter Depth")

ticks <-  ggpredict(tpt_noHosts_pois, type = "re", terms = c("avg_canopy_cover [tmp_canopy]", "logit_litter[tmp_logit]", "biomass_log [tmp_biomass]", "avg_1yr_vp..Pa. [quart2]") ) ## Cant be more than four values. Excluding d_since_fire becuase wasn't sig anyway
ticks_baseline <- ggpredict(tpt_noHosts_pois, type = "re", terms = c("avg_canopy_cover [quart2]", "avg_1yr_vp..Pa. [quart2]", "logit_litter [quart2]", "biomass_log [quart2]"))

plot_comparison_ggplot(ticks, ticks_baseline, "Ticks Predicted")

plot_comparison_ggplot <- function(simulated, baseline, title){
  
  
  simulated$label <- "Simulated Levels"
  simulated$quant <- "Mean Predictions"
  simulated_conf <- list()
  simulated_conf$confidence_points <- c(simulated$conf.low, simulated$conf.high, simulated$predicted)
  simulated_conf$label <- "Simulated Levels"
  simulated_conf$quant <- "Prediction Interval"
  simulated_conf <- as.data.frame(simulated_conf)
  
  
  baseline$label <- "Baseline"
  baseline$quant <- "Mean Predictions"
  baseline_conf <- list()
  baseline_conf$confidence_points <- c(baseline$conf.low, baseline$conf.high, baseline$predicted)
  baseline_conf$label <- "Baseline"
  baseline_conf$quant <- "Prediction Interval 95%"
  
  baseline_conf <- as.data.frame(baseline_conf)
  
  if(length(baseline$predicted) <= 9){
    ggplot() + 
      theme_classic()+
      geom_pointrange(data = simulated_conf, aes(ymin = min(confidence_points), ymax = max(confidence_points),y = mean(confidence_points), x = label, color = label, linetype = quant)) +
      geom_pointrange(data = baseline_conf, aes(ymin = min(confidence_points), ymax = max(confidence_points),y = mean(confidence_points), x = label, color = label, linetype = quant)) +
      geom_pointrange(data = simulated, aes(ymin = min(predicted), ymax = max(predicted),y = mean(predicted), x = label, color = label, linetype = quant)) +
      geom_pointrange(data = baseline, aes(ymin = min(predicted), ymax = max(predicted),y = mean(predicted), x = label, color = label, linetype = quant)) +
      #geom_point(data = baseline, aes(x = round(mean(baseline$predicted), digits = 3), y = baseline$label[1])) +
      geom_text(aes(y = mean(baseline$predicted), x = baseline$label[1]), label = paste("Mean:", round(mean(baseline$predicted), digits = 3)), nudge_x = 0.3, family = "Ariel", size = 3) + 
      #geom_point(data = simulated, aes(x = round(mean(simulated$predicted), digits = 3), y = simulated$label[1])) +
      geom_text(aes(y = mean(simulated$predicted), x = simulated$label[1]), label = paste("Mean:", round(mean(simulated$predicted), digits = 3)), nudge_x = 0.3, family = "Ariel", size = 3) + 
      #scale_linetype_discrete(c("b", "a")) + 
      ylab(title) +
      xlab("") + 
      scale_color_discrete(guide = "none") + 
      guides(linetype = guide_legend(override.aes = list(shape = c(NA, NA), title = NA)))
      #scale_alpha_discrete(range=c(0.5, 1)) + 
     
    
  }else{
    p <- ggplot() + 
      theme_classic()+
      geom_violin(data = simulated_conf, aes(x = confidence_points, y = label, fill = label, alpha = quant)) +
      geom_violin(data = baseline_conf, aes(x = baseline_conf$confidence_points, y = baseline_conf$label, fill = label, alpha = quant)) +
      geom_violin(data = simulated, aes(x = simulated$predicted, y = simulated$label, fill = label, alpha = quant)) + 
      geom_violin(data = baseline, aes(x = baseline$predicted, y = baseline$label, fill = label, alpha = quant)) +
      geom_point(data = baseline, aes(x = round(mean(baseline$predicted), digits = 3), y = baseline$label[1]), color = "yellow") +
      geom_text(aes(x = mean(baseline$predicted), y = baseline$label[1]), label = paste("Mean:", round(mean(baseline$predicted), digits = 3)), nudge_y = 0.3, family = "Ariel", size = 3) + 
      geom_point(data = simulated, aes(x = round(mean(simulated$predicted), digits = 3), y = simulated$label[1]), color = "yellow") +
      geom_text(aes(x = mean(simulated$predicted), y = simulated$label[1]), label = paste("Mean:", round(mean(simulated$predicted), digits = 3)), nudge_x = round(mean(simulated$predicted), digits = 3), nudge_y = 0.3, family = "Ariel", size = 3) + 
      coord_flip() + 
      xlab(title) +
      ylab("") + 
      scale_fill_discrete(guide = "none")  + 
      scale_alpha_discrete(range=c(1, 0.5)) + 
      guides(alpha = guide_legend(override.aes = list(fill = c("grey50", "grey70"), title = NA)) ) 
    
    return(p)
    
  }
  
  
  
  
  
}







####
ticks$label <- "Simulated Levels"
ticks$quant <- "Mean Predictions"
ticks_conf <- list()
ticks_conf$confidence_points <- c(ticks$conf.low, ticks$conf.high, ticks$predicted)
ticks_conf$label <- ticks$label
ticks_conf$quant <- "95% Prediction Interval"
ticks_conf <- as.data.frame(ticks_conf)


ticks_baseline$label <- "Baseline"
ticks_baseline$quant <- "Mean Predictions"
ticks_b <- list()
ticks_b$confidence_points <- c(ticks_baseline$conf.low, ticks_baseline$conf.high, ticks_baseline$predicted)
ticks_b$label <- ticks_baseline$label
ticks_b$quant <- "95% Prediction Interval"

ticks_b <- as.data.frame(ticks_b)

ggplot() + 
  theme_classic()+
  geom_violin(data = ticks_conf, aes(x = confidence_points, y = label, fill = label, alpha = quant)) +
  geom_violin(data = ticks_b, aes(x = ticks_b$confidence_points, y = ticks_b$label, fill = label, alpha = quant)) +
  geom_violin(data = ticks, aes(x = ticks$predicted, y = ticks$label, fill = label, alpha = quant)) + 
  geom_violin(data = ticks_baseline, aes(x = ticks_baseline$predicted, y = ticks_baseline$label, fill = label, alpha = quant)) +
  geom_point(data = ticks_baseline, aes(x = round(mean(ticks_baseline$predicted), digits = 3), y = ticks_baseline$label[1]), color = "yellow") +
  geom_text(aes(x = mean(ticks_baseline$predicted), y = ticks_baseline$label[1]), label = paste("Mean:", round(mean(ticks_baseline$predicted), digits = 3)), nudge_x = 4, nudge_y = 0.3, family = "Ariel", size = 3) + 
  geom_point(data = ticks, aes(x = round(mean(ticks$predicted), digits = 3), y = ticks$label[1]), color = "yellow") +
  geom_text(aes(x = mean(ticks$predicted), y = ticks$label[1]), label = paste("Mean:", round(mean(ticks$predicted), digits = 3)), nudge_x = 4, nudge_y = 0.3, family = "Ariel", size = 3) + 
  coord_flip() + 
  xlab("Ticks Predicted")+
  ylab("") + 
  scale_fill_discrete(guide = "none")  + 
  scale_alpha_discrete(range=c(0.5, 1)) + 
  guides(alpha = guide_legend(override.aes = list(fill = c("grey70", "grey50"))) ) 

#litter_cover_mod, litter_depth_mod, canopy_mod,
#standing_biomass_mod,
#time_since_fire_mod, fri_mod,
#avg_1yr_vp_mod,



