################################################################################
# Stacked bar plot for Age-standardised prevalence of idiopathic epilepsy (per 100,000) by severity and GBD super-region 
# Victor Liu (victor.liu@aut.ac.nz)
# Sequela IDs instead of cause IDs: Idiopathic, seizure-free, treated epilepsy sequela ID = 458, 
# Idiopathic, less severe epilepsy sequela ID = 459, Idiopathic, severe epilepsy ID = 460).
################################################################################

library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)

creat_plot <- function(prevalence_df, location_f, region_ls, plot_title) {
  #filter by all region location_id
  prev_data <- filter(prevalence_df, age_group_id == 27, year_id == 2021, sex_id == 3, location_id %in% region_ls)
  
  draw_cols <- grep("draw_", names(prevalence_df), value = TRUE)
  #prev_data <- prevalence_df
  prev_data$mean_draw_2021 <- rowMeans(prev_data[, draw_cols]) * 100000
  
  merged_result <- merge(location_f, prev_data, by = "location_id", all = FALSE) 
  merged_result <- merged_result[c("location_name", "sequela_id", "mean_draw_2021")]
  
  #translate the squela_id to actual name
  merged_result$sequela_id <- ifelse(merged_result$sequela_id == 458, "Idiopathic, seizure-free, treated epilepsy",
                          ifelse(merged_result$sequela_id == 459, "Idiopathic, less severe epilepsy",
                                 ifelse(merged_result$sequela_id == 460, "Idiopathic, severe epilepsy", merged_result$sequela_id)))
  
  # Generate plot for super_region
  plot <- ggplot(merged_result, aes(fill = sequela_id, y = mean_draw_2021, x = location_name)) + 
    geom_bar(position="fill", stat="identity") +  theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7), 
          axis.text.y = element_text(size = 7), legend.position = "bottom") +
    ggtitle(plot_title) +
    labs(y = "Prevalence rate per 100,000", x = "Super-region", fill = "Epilepsy Severity", size = 4)
  return(plot)
}


# Process prevalence data
prevalence_df <- read.csv("../GBD 2021 extracts/draws_sequela_epilepsy_prev_all_age.csv", header = TRUE)
location_f <- read.csv("../column_id/shared_ids_location.csv", header = TRUE) 
location_f <- location_f[c("location_id", "location_name")]

# Set output path - update this!
# (Filepath below used to test script, store sample outputs)
output_path <- "../graphs/"

super_region_id_ls <- c(1, 4, 31, 64, 103, 137, 158, 166)
region_id_ls <- c(1, 32, 42, 56, 70, 65, 100, 96, 73, 120, 104, 124, 134, 138, 159, 5, 21, 9, 167, 174, 192, 199)


super_region_plot <- creat_plot(prevalence_df, location_f, super_region_id_ls, "Age-standardised prevalence of idiopathic epilepsy (per 100,000) by severity and GBD super-region")
region_plot <- creat_plot(prevalence_df, location_f, region_id_ls, "Age-standardised prevalence of idiopathic epilepsy (per 100,000) by severity and region")

# Save
pdf(paste0(output_path, "Figure5.Age-standardised_prevalence_of_idiopathic_epilepsy_(per100,000)_by_severity_and_GBD_super-region.pdf"), width = 9, height = 6)
print(super_region_plot)
dev.off()

pdf(paste0(output_path, "Figure5.Age-standardised_prevalence_of_idiopathic_epilepsy_(per100,000)_by_severity_and_region.pdf"), width = 11, height = 6)
print(region_plot)
dev.off()
