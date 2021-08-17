plot_comparison_ggplot <- function(simulated, baseline, title){
  
  
  simulated$label <- "Simulated Levels"
  simulated$quant <- "Mean Predictions"
  simulated_conf <- list()
  simulated_conf$confidence_points <- c(simulated$conf.low, simulated$predicted, simulated$conf.high)
  simulated_conf$label <- "Simulated Levels"
  simulated_conf$quant <- "Prediction Interval 95%"
  simulated_conf <- as.data.frame(simulated_conf)
  
  
  baseline$label <- "Baseline"
  baseline$quant <- "Mean Predictions"
  baseline_conf <- list()
  baseline_conf$confidence_points <- c(baseline$conf.low, baseline$predicted, baseline$conf.high)
  baseline_conf$label <- "Baseline"
  baseline_conf$quant <- "Prediction Interval 95%"
  
  baseline_conf <- as.data.frame(baseline_conf)
  
  if(length(baseline$predicted) <= 9){
    p <- ggplot() + 
      theme_classic()+
      geom_pointrange(data = simulated_conf, aes(ymin = min(confidence_points), ymax = max(confidence_points),y = mean(confidence_points), x = label, color = label, linetype = quant)) +
      geom_pointrange(data = baseline_conf, aes(ymin = min(confidence_points), ymax = max(confidence_points),y = mean(confidence_points), x = label, color = label, linetype = quant)) +
      geom_pointrange(data = simulated, aes(ymin = min(predicted), ymax = max(predicted),y = mean(predicted), x = label, color = label, linetype = quant)) +
      geom_pointrange(data = baseline, aes(ymin = min(predicted), ymax = max(predicted),y = mean(predicted), x = label, color = label, linetype = quant)) +
      geom_text(aes(y = mean(baseline$predicted), x = baseline$label[1]), label = paste("Mean:", round(mean(baseline$predicted), digits = 3)), nudge_x = 0.3, family = "Ariel", size = 3) + 
      geom_text(aes(y = mean(simulated$predicted), x = simulated$label[1]), label = paste("Mean:", round(mean(simulated$predicted), digits = 3)), nudge_x = 0.3, family = "Ariel", size = 3) + 
      ylab(title) +
      xlab("") + 
      scale_color_discrete(guide = "none") + 
      guides(linetype = guide_legend(override.aes = list(shape = c(NA, NA)), title = NULL))
    
    return(p)
    
  }else{
    p <- ggplot() + 
      theme_classic()+
      geom_violin(data = simulated_conf, aes(x = confidence_points, y = label, fill = label, alpha = quant)) +
      geom_violin(data = baseline_conf, aes(x = baseline_conf$confidence_points, y = baseline_conf$label, fill = label, alpha = quant)) +
      geom_violin(data = simulated, aes(x = simulated$predicted, y = simulated$label, fill = label, alpha = quant)) + 
      geom_violin(data = baseline, aes(x = baseline$predicted, y = baseline$label, fill = label, alpha = quant)) +
      geom_point(data = baseline, aes(x = round(mean(baseline$predicted), digits = 3), y = baseline$label[1]), color = "yellow") +
      geom_text(aes(x = mean(baseline$predicted), y = baseline$label[1]), label = paste("Mean:", round(mean(baseline$predicted), digits = 3)), nudge_y = 0.3, nudge_x = mean(baseline$predicted) * 2,  family = "Ariel", size = 3) + 
      geom_point(data = simulated, aes(x = round(mean(simulated$predicted), digits = 3), y = simulated$label[1]), color = "yellow") +
      geom_text(aes(x = mean(simulated$predicted), y = simulated$label[1]), label = paste("Mean:", round(mean(simulated$predicted), digits = 3)), nudge_y = 0.3, nudge_x = mean(simulated$predicted) * 2, family = "Ariel", size = 3) + 
      coord_flip() + 
      xlab(title) +
      ylab("") + 
      scale_fill_discrete(guide = "none")  + 
      scale_alpha_discrete(range=c(1, 0.5)) + 
      guides(alpha = guide_legend(override.aes = list(fill = c("grey50", "grey70")), title = NULL) ) 
    
    return(p)
    
  }
  
  
  
}