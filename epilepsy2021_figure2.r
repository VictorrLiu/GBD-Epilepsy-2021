# Load libraries
library(dplyr)
library(ggplot2)
library(ggsci)

# Set output path - update this!
output_path <- "../graphs/"

# Load and preprocess age group data
age_group <- read.csv("../column_id/shared_ids_age_group.csv")
selected_age_rows <- age_group %>%
  filter(!age_group_id %in% c(2, 3)) %>%
  mutate(age_group_name = case_when(
    age_group_id == 235 ~ "95", 
    age_group_id == 238 ~ "1.5",
    age_group_id == 388 ~ "0.3",
    age_group_id == 389 ~ "0.85",
    TRUE ~ as.character(age_group_name)
  ))

# Load main data
data <- read.csv("../GBD 2021 extracts/draws_total_epilepsy_imp_prev_all_age.csv")

# Define a function to create plots
create_plot <- function(filtered_data, plot_title) {
  summarized_data <- filtered_data %>%
    group_by(age_group_id, sex_id) %>%
    summarize(across(starts_with("draw_"), ~ sum(.x, na.rm = TRUE))) %>%
    merge(selected_age_rows, by = "age_group_id", all = FALSE) %>%
    mutate(
      draw_mean = rowMeans(across(starts_with("draw_")), na.rm = TRUE),
      lower_2.5 = apply(across(starts_with("draw_")), 1, function(x) quantile(x, probs = 0.025, na.rm = TRUE)),
      upper_2.5 = apply(across(starts_with("draw_")), 1, function(x) quantile(x, probs = 0.975, na.rm = TRUE)),
      lower = as.numeric(gsub("^(\\d+) to (\\d+)$", "\\1", age_group_name)),
      upper = as.numeric(gsub("^(\\d+) to (\\d+)$", "\\2", age_group_name)),
      middle = ifelse(is.na(lower), as.numeric(gsub("^(\\d+)$", "\\1", age_group_name)), lower + (upper - lower) / 2)
    ) 
  # Rename the age_group_name column with the middle values
  summarized_data$age_group_name <- summarized_data$middle
  summarized_data <- summarized_data[, !(names(summarized_data) %in% c("lower", "upper", "middle"))]
  
  
  summarized_data <- select(summarized_data, age_group_name, sex_id,	draw_mean, lower_2.5, upper_2.5)
  
   # Transform to per 100K
  summarized_data$draw_mean <- summarized_data$draw_mean * 100000
  summarized_data$lower_2.5 <- summarized_data$lower_2.5 * 100000
  summarized_data$upper_2.5 <- summarized_data$upper_2.5 * 100000  
  summarized_data <- bind_rows(summarized_data, data.frame(age_group_name = 0, draw_mean = 0, lower_2.5 = 0, upper_2.5 = 0, sex_id = 1:2))
  

  plot <- ggplot(summarized_data, aes(x = age_group_name, y = draw_mean, fill = factor(sex_id))) + 
    geom_line(aes(color = factor(sex_id)), size = 0.8) + 
    geom_ribbon(aes(ymin = lower_2.5, ymax = upper_2.5), alpha = 0.3) +
    scale_fill_manual(values = c("#3288bd", "#D53E4F"),
                      labels = c("Male", "Female"),
                      guide = "none") +
    scale_colour_manual(values = c("#00468BFF", "#ad002aff"),
                        labels = c("Male", "Female"), 
                        guide = "legend", name = "") +
    scale_x_continuous(breaks = seq(0, max(summarized_data$age_group_name), by = 5), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Age (years)") + 
    ylab("Prevalence (per 100 000 population)") + 
    ggtitle(plot_title) +
    theme_classic() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.15, 0.95),
          strip.text.x = element_text(face = "bold"))
  
  return(plot)
}

# Filter and create plots for different types of epilepsy
filtered_idio_data <- data %>%
  filter(location_id == 1, year_id == 2021, cause_id == 545)
plot_idio <- create_plot(filtered_idio_data, "Global prevalence of idiopathic epilepsy by age and sex, 2021")

filtered_second_data <- data %>%
  filter(location_id == 1, year_id == 2021, cause_id != 545)
plot_second <- create_plot(filtered_second_data, "Global prevalence of secondary epilepsy by age and sex, 2021")

filtered_combined_data <- data %>%
  filter(location_id == 1, year_id == 2021)
plot_combined <- create_plot(filtered_combined_data, "Global prevalence of combined epilepsy by age and sex, 2021")

# Save plots as PDFs
pdf(paste0(output_path, "Global_prevalence_of_idiopathic_epilepsy_by_age_and_sex,2021.pdf"), width = 11, height = 6)
print(plot_idio)
dev.off()

pdf(paste0(output_path, "Global_prevalence_of_secondary_epilepsy_by_age_and_sex,2021.pdf"), width = 11, height = 6)
print(plot_second)
dev.off()

pdf(paste0(output_path, "Global_prevalence_of_combined_epilepsy_by_age_and_sex,2021.pdf"), width = 11, height = 6)
print(plot_combined)
dev.off()
