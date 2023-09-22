################################################################################
# Create simple figure for Global years lived with disability (YLDs) and 
# years of life lost (YLLs) rates due to idiopathic epilepsy by age, 2021 
# Used for GBD 2021 ep paper
# Victor Liu (victor.liu@aut.ac.nz)
################################################################################

library(dplyr)
library(ggplot2)
library(ggsci)

# Set output path - update this!
output_path <- "../graphs/"
age_group <- read.csv("../column_id/shared_ids_age_group.csv")



# Filter the data frame to exclude age_group_id == 2 & 3, because they are the early and late neonatal. 
# and remove 235, 238, 388, 389 so i can add in the correct values for them later
selected_age_rows <- age_group[!(age_group$age_group_id %in% c(2, 3, 22, 27)),]

#updating the age_group 388 from "1-5 months" to 0.3, 389 from "6-11 months" to 0.85... 
selected_age_rows <- selected_age_rows %>%
  mutate(age_group_name = case_when(
    age_group_id == 235 ~ "95", age_group_id == 238 ~ "1.5",
    age_group_id == 388 ~ "0.3", age_group_id == 389 ~ "0.85",
    TRUE ~ age_group_name))

pop_df <- read.csv("../column_id/population.csv", header = TRUE) %>% 
  filter(location_id == 1, year_id == 2021, sex_id == 3) %>% select(location_id, year_id, sex_id, age_group_id, population)
yll_preprocessed_data_f <- read.csv("C:../GBD 2021 extracts/draws_idio_epilepsy_yll_all_age.csv")

yll_data_f <- merge(yll_preprocessed_data_f, pop_df, all = FALSE) %>% 
  mutate(across(starts_with("draw_"), ~ . / population))

yld_data_f <- read.csv("C:../GBD 2021 extracts/draws_idio_epilepsy_yld_all_age.csv")

yld_data <- yld_data_f %>% mutate(type_id = "yld")
yll_data <- yll_data_f %>% mutate(type_id = "yll") %>% filter(metric_id == 1)

# Combine the data frames into a single data frame
combined_data <- bind_rows(yld_data, yll_data)
filtered_idio_data <- subset(combined_data, location_id == 1 & year_id == 2021 & sex_id == 3)
filtered_idio_data <- merge(filtered_idio_data, selected_age_rows, by = "age_group_id", all = FALSE)


filtered_idio_data$draw_mean <- rowMeans(filtered_idio_data[, paste0("draw_", 1:99)], na.rm = TRUE) 
# Calculate mean of the lower & upper 2.5 percentile for each row
filtered_idio_data$lower_2.5 <- apply(filtered_idio_data[, paste0("draw_", 1:99)], 1, function(x) quantile(x, probs = 0.025, na.rm = TRUE))
filtered_idio_data$upper_2.5 <- apply(filtered_idio_data[, paste0("draw_", 1:99)], 1, function(x) quantile(x, probs = 0.975, na.rm = TRUE))

# Extract lower and upper bounds from age_group_name if the format matches
filtered_idio_data$lower <- as.numeric(gsub("^(\\d+) to (\\d+)$", "\\1", filtered_idio_data$age_group_name))
filtered_idio_data$upper <- as.numeric(gsub("^(\\d+) to (\\d+)$", "\\2", filtered_idio_data$age_group_name))

# Calculate the middle value where the format matches
filtered_idio_data$middle <- filtered_idio_data$lower + (filtered_idio_data$upper - filtered_idio_data$lower) / 2

# Replace middle values with existing values where the format doesn't match
filtered_idio_data$middle[is.na(filtered_idio_data$middle)] <- as.numeric(gsub("^(\\d+)$", "\\1", filtered_idio_data$age_group_name[is.na(filtered_idio_data$middle)]))

# Rename the age_group_name column with the middle values
filtered_idio_data$age_group_name <- filtered_idio_data$middle

# Drop the intermediate columns (lower, upper, and middle)
filtered_idio_data <- filtered_idio_data[, !(names(filtered_idio_data) %in% c("lower", "upper", "middle"))]

selected_idio_data <- select(filtered_idio_data, age_group_name, type_id, draw_mean, lower_2.5, upper_2.5)


# Transform to per 100K
# Apply the multiplication operation to "draw_mean', lower_2.5' and 'upper_2.5' only for rows where 'type_id' is "YLDs"
selected_idio_data <- selected_idio_data %>%
  mutate(across(c(draw_mean, lower_2.5, upper_2.5), ~ . * 100000))

#add in the 0,0 data point for plotting
new_row_1 <- data.frame(age_group_name = 0, draw_mean = 0, lower_2.5 = 0, upper_2.5 = 0, type_id = "yld")
new_row_2 <- data.frame(age_group_name = 0, draw_mean = 0, lower_2.5 = 0, upper_2.5 = 0, type_id = "yll")

# Combine new rows with the existing dataframe
selected_idio_data <- rbind(selected_idio_data, new_row_1, new_row_2)


# Create plot!
pdf(paste0(output_path, "Figure3.Global_years_lived_with_disability_(YLDs)_and_years_of_life_lost_(YLLs)_rates_due_to_idiopathic_epilepsy_by_age,2021.pdf"), width = 11, height = 6)
ggplot(selected_idio_data, aes(x = age_group_name, y = draw_mean, fill = factor(type_id))) + 
  geom_line(aes(color = factor(type_id)), size = 0.8) + 
  geom_ribbon(aes(ymin = lower_2.5, ymax = upper_2.5), alpha = 0.3) +
  scale_fill_manual(values = c("#D53E4F", "#3288bd"),
                    labels =c("YLDs", "YLLs"),
                    guide = "none") +
  scale_colour_manual(values = c("#ad002aff", "#00468BFF"),
                      labels =c("YLDs", "YLLs"), 
                      guide = "legend", name = "") +
  scale_x_continuous(breaks = seq(0, max(selected_idio_data$age_group_name), by = 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Age (years)") + 
  ylab("YLL and YLD rate (per 100 000 population)") + 
  ggtitle("Global years lived with disability (YLDs) and years of life lost (YLLs) rates due to idiopathic epilepsy by age, 2021") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.09, 0.99),
        legend.text = element_text(size = 5)) + # Adjust legend position (0, 1) = top-left
  theme(strip.text.x = element_text(face = "bold")) 
dev.off()

