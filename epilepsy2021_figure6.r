################################################################################
# Stacked bar plot for Prevalence rates of idiopathic, secondary and total epilepsy (per 100,000) by age 
# Victor Liu (victor.liu@aut.ac.nz)
################################################################################

library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)

creat_plot <- function(filtered_prevalence, plot_title) {
  
  #translate cause_id 545 to idopathic, all other to secondary
  filtered_prevalence$cause <- ifelse(filtered_prevalence$cause_id == 545, "Idiopathic epilepsy", "Secondary epilepsy")
  
  summarized_data <- filtered_prevalence %>%
    group_by(age_group_name, cause) %>%
    summarize(across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE))) %>% 
    mutate(draw_mean = rowMeans(across(starts_with("draw_")), na.rm = TRUE)*100000)
  
  summarized_result <- select(summarized_data, "age_group_name", "cause", "draw_mean")
  
  # Define the order of categories for reordering rows
  desired_order <- c("Early Neonatal", "Late Neonatal", "1-5 months", "6-11 months", "12 to 23 months", "2 to 4", "5 to 9", 
  "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", 
  "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 94", "95 plus")
  
  # Reorder the levels of the "category" variable
  summarized_result$age_group_name <- factor(summarized_result$age_group_name, levels = desired_order)
  
  # Generate plot for
  plot <- ggplot(summarized_result, aes(fill = cause, y = draw_mean, x = age_group_name)) + 
    geom_bar(position="stack", stat="identity") +  theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7), 
          axis.text.y = element_text(size = 7), legend.position = "bottom") +
    ggtitle(plot_title) +
    labs(y = "Prevalence rate per 100,000", x = "Age ID", fill = "Epilepsy Type", size = 4)
  return(plot)
}


# Process prevalence data
prevalence_df <- read.csv("../GBD 2021 extracts/draws_total_epilepsy_imp_prev_all_age.csv", header = TRUE)
# Set output path - update this!
# (Filepath below used to test script, store sample outputs)
output_path <- "../graphs/"

age_group <- read.csv("../column_id/shared_ids_age_group.csv")
filtered_prevalence <- filter(prevalence_df, location_id == 1, year_id == 2021) %>% merge(age_group, by = "age_group_id")

stack_plot <- creat_plot(filtered_prevalence, "Prevalence rates of idiopathic, secondary and total epilepsy (per 100,000) by age")

                             
# Save
pdf(paste0(output_path, "Figure6.Prevalence_rates_of_idiopathic,secondary_and_total_epilepsy_(per100,000)by_age.pdf"), width = 11, height = 6)
print(stack_plot)
dev.off()
