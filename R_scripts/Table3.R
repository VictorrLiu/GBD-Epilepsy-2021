################################################################################
# Table 3. Absolute number (in millions) and age-standardised rates per 100,000 
# people per year, with 95% uncertainty intervals (UI), of prevalent epilepsy, 
# deaths from epilepsy and DALYs due to epilepsy in 2021 and percentage change in 
# the metrics for 1990-2021, by type epilepsy and GBD super-region (same table by 
# World Bank country income groups (High-income countries; Low- to-middle-income countries) 
# 
# Victor Liu (victor.liu@aut.ac.nz)
################################################################################

library(data.table)
library(readxl)
library(stats)
library(dplyr)

# Define a function to calculate percentage differences
calculate_percentage_diff <- function(data_1990, data_2021, draw_cols) {
  modified_rows <- data.frame(matrix(0, nrow = nrow(data_1990), ncol = ncol(data_1990)))
  names(modified_rows) <- names(data_1990)
  
  for (draw_col in draw_cols) {
    draw_diff <- (data_2021[[draw_col]] - data_1990[[draw_col]]) / data_1990[[draw_col]]
    modified_rows[[draw_col]] <- draw_diff
  }
  
  return(modified_rows)
}
# Define a function to process data
process_data <- function(data_df, heading){
  
  #type_val <- ifelse(type == "number", 1000000, 1)
  # Filter rows for 1990 and 2021
  data_1990 <- filter(data_df, year_id == 1990)
  data_2021 <- filter(data_df, year_id == 2021) 
  
  # Calculate percentage differences
  draw_cols <- grep("draw_", names(data_df), value = TRUE)
  modified_rows <- calculate_percentage_diff(data_1990, data_2021, draw_cols)
  
  # Update year_id for the generated rows
  modified_rows$year_id <- "(2021-1990)/1990"
  
  # Copy values from 1990 and 2021 rows for other columns
  other_cols <- setdiff(names(data_df), c("year_id", draw_cols))
  for (col in other_cols) {
    modified_rows[[col]] <- ifelse(modified_rows$year_id == "(2021-1990)/1990", data_1990[[col]], modified_rows[[col]])
  }
  modified_rows$percentage_diff <- rowMeans(modified_rows[, draw_cols])
  data_2021$mean_2021 <- rowMeans(data_2021[, draw_cols])
  metric_2021 <- select(data_2021, location_id, mean_2021)
  merged_rows <- merge(modified_rows, metric_2021, by = "location_id") %>% select(mean_2021, location_id, percentage_diff, type)
  
  result <- merged_rows %>% 
    mutate(
      #mean_2021 = mean_2021/type_val,
      !!paste0(heading,"_Metric_in_2021") := mean_2021,
      !!paste0(heading,"_Percentage change, 1990-2021") := sprintf("%.2f%%", percentage_diff * 100)) %>% 
    select(-mean_2021, -percentage_diff)
  return(result)
}

super_region_id_ls <- c(1, 4, 31, 64, 103, 137, 158, 166)
#loading file needed for the location_name, location_sdi
location_f <- read.csv("../column_id/shared_ids_location.csv", header = TRUE)
location_f <- location_f[c("location_id", "location_name")]


# Process death data
death_df <- read.csv("../GBD 2021 extracts/draws_idio_epilepsy_deaths_all_age_all_metrics.csv", header = TRUE)
death_num <- filter(death_df, metric_id == 1, age_group_id == 22, sex_id == 3, location_id %in% super_region_id_ls) %>% 
  mutate(type = 'Absolute number')
death_rate <- filter(death_df, metric_id == 3, age_group_id == 27, sex_id == 3, location_id %in% super_region_id_ls)%>% 
  mutate(type = 'Age-standardised rate')
death_num_result <- process_data(death_num, "Deaths")
death_rate_result <- process_data(death_rate, "Deaths")
final_death <- bind_rows(death_num_result, death_rate_result)

# Process prevalence data
prevalence_df <- read.csv("../GBD 2021 extracts/draws_idiopathic_epilepsy_prevalence.csv", header = TRUE)
pop_df <- read.csv("../column_id/population.csv", header = TRUE) %>% 
  filter(age_group_id == 22, year_id == 2021 | year_id == 1990) %>% select(location_id, year_id, sex_id, population)
prevalence_rate <- filter(prevalence_df, metric_id == 3, age_group_id == 27, sex_id == 3, location_id %in% super_region_id_ls)%>% 
  mutate(type = 'Age-standardised rate')
#multiply old draw values by population size accordingly
prevalence_num <- merge(prevalence_rate, pop_df, all = FALSE) %>% mutate(across(starts_with("draw_"), ~ . * population))%>% 
  mutate(type = 'Absolute number')
prevalence_num_result <- process_data(prevalence_num, "Prevalence")
prevalence_rate_result <- process_data(prevalence_rate, "Prevalence")
final_prevalence <- bind_rows(prevalence_num_result, prevalence_rate_result)

# Process DALY data
daly_num <- read.csv("../GBD 2021 extracts/draws_idio_epilepsy_dalys_all_age_num.csv", header = TRUE)%>% 
  filter(metric_id == 1, age_group_id == 22, sex_id == 3, year_id == 1990 | year_id == 2021, location_id %in% super_region_id_ls)%>% 
  mutate(type = 'Absolute number')
daly_rate <- read.csv("../GBD 2021 extracts/draws_idio_epilepsy_dalys_all_age_rate.csv", header = TRUE)%>% 
  filter(metric_id == 3, age_group_id == 27, sex_id == 3, year_id == 1990 | year_id == 2021, location_id %in% super_region_id_ls)%>% 
  mutate(type = 'Age-standardised rate')
daly_num_result <- process_data(daly_num, "DALYs")
daly_rate_result <- process_data(daly_rate, "DALYs")
final_daly <- bind_rows(daly_num_result, daly_rate_result)

# Merge the dataframes using the inner_join function
merged_result <- inner_join(final_prevalence, final_death, by = c("location_id", "type")) %>%
  inner_join(final_daly, by = c("location_id", "type"))
merged_result <- merge(location_f, merged_result, by = "location_id", all = TRUE)

final_result <- na.omit(merged_result)
# Define the custom order for the "type" column
custom_order <- c("Absolute number", "Age-standardised rate")
location_order <- c(1,64,103,166,137,4,158,31)

# Order the dataframe by "type" and "location_id"
final_result <- final_result %>%
  arrange(factor(type, levels = custom_order), match(location_id, location_order))

# Extract the reults to a csv file
write.csv(final_result, "../excel_tables/Table3.csv", row.names = FALSE)
