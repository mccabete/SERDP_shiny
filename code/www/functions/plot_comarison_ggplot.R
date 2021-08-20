plot_comparison_ggplot <- function(simulated, baseline, title){
  
  simulated <- as.data.frame(simulated)
  baseline <- as.data.frame(baseline)
  
  simulated$label <- "Simulated Levels"
  baseline$label <- "Observations"
 
  names(simulated) <- c("predicted", "label")
  names(baseline) <- c("predicted", "label")
  
  if(length(baseline$predicted) <= 9){
    p <- ggplot() + 
      theme_classic()+
      geom_pointrange(data = simulated, aes(ymin = min(predicted), ymax = max(predicted),y = mean(predicted), x = label, color = label)) +
      geom_pointrange(data = baseline, aes(ymin = min(predicted), ymax = max(predicted),y = mean(predicted), x = label, color = label)) +
      geom_text(aes(y = mean(baseline$predicted), x = baseline$label[1]), label = paste("Mean:", round(mean(baseline$predicted), digits = 3)), nudge_x = 0.3, family = "Ariel", size = 3) + 
      geom_text(aes(y = mean(simulated$predicted), x = simulated$label[1]), label = paste("Mean:", round(mean(simulated$predicted), digits = 3)), nudge_x = 0.3, family = "Ariel", size = 3) + 
      ylab(title) +
      xlab("") + 
      scale_color_discrete(guide = "none")
    
    return(p)
    
  }else{
    p <- ggplot() + 
      theme_classic()+
      geom_violin(data = simulated, aes(x = simulated$predicted, y = simulated$label, fill = label)) + 
      geom_violin(data = baseline, aes(x = baseline$predicted, y = baseline$label, fill = label)) +
      geom_point(data = baseline, aes(x = round(mean(baseline$predicted), digits = 3), y = baseline$label[1]), color = "yellow") +
      geom_text(aes(x = 0 , y = baseline$label[1]), label = paste("Mean:", round(mean(baseline$predicted), digits = 3)), nudge_y = 0.3,  nudge_x = -2,  family = "Ariel", size = 3) + 
      geom_point(data = simulated, aes(x = round(mean(simulated$predicted), digits = 3), y = simulated$label[1]), color = "yellow") +
      geom_text(aes(x = 0, y = simulated$label[1]), label = paste("Mean:", round(mean(simulated$predicted), digits = 3)), nudge_y = 0.3, nudge_x = -2, family = "Ariel", size = 3) + 
      coord_flip() + 
      xlab(title) +
      ylab("") + 
      scale_fill_discrete(guide = "none") # + 
      
    
    return(p)
    
  }
  
  
  
}
