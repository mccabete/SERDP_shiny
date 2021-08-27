#hist(path_data$avg_litter_depth_all)
library("stringr")

tmp <- path_data

#tmp <- select(path_data,c(avg_litter_depth_all, plot_id, inst_name, visit_date)  )
tmp$small_name <- NA

for ( i in seq_along(tmp$avg_litter_depth_all)){
  
  tmp$small_name[i] <- stringr::str_sub(tmp$plot_id[i], end = (nchar(tmp$plot_id[i]) -3))
  
}

tmp$installation_name <- data_name_to_formal(tmp$small_name)

write_csv(tmp, "www/path_data.csv")
