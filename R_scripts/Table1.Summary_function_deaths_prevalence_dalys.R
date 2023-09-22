library(data.table)
library(readxl)
library(stats)
library(dplyr)

# Define a function to calculate percentage differences
calculate_percentage_diff <- function(rate_1990, rate_2021, draw_cols) {
  modified_rows <- data.frame(matrix(0, nrow = nrow(rate_1990), ncol = ncol(rate_1990)))
  names(modified_rows) <- names(rate_1990)
  
  for (draw_col in draw_cols) {
    draw_diff <- (rate_2021[[draw_col]] - rate_1990[[draw_col]]) / rate_1990[[draw_col]]
    modified_rows[[draw_col]] <- draw_diff
  }
  
  return(modified_rows)
}

# Define a function to calculate mean, min, and max draws for 2021
calculate_draw_stats_2021 <- function(num_2021, draw_cols, modified_rows) {
  # Reorder num_2021 based on the order of location_id in modified_rows, so the rows match in the later calculation for mean_draw_2021 etc
  unique_location_ids <- unique(modified_rows$location_id)
  num_2021 <- num_2021[match(unique_location_ids, num_2021$location_id), ]
  
  mean_draw_2021 <- rowMeans(num_2021[, draw_cols])
  min_draw_2021 <- apply(num_2021[, draw_cols], 1, min)
  max_draw_2021 <- apply(num_2021[, draw_cols], 1, max)
  
  return(data.frame(mean_draw_2021, min_draw_2021, max_draw_2021))
}

# Define a function to calculate mean, lower percentile, and upper percentile
calculate_statistics <- function(modified_rows, draw_cols) {
  mean_diff <- rowMeans(modified_rows[, draw_cols])
  lower_percentile <- apply(modified_rows[, draw_cols], 1, FUN = function(x) mean(quantile(x, probs = 0.025)))
  upper_percentile <- apply(modified_rows[, draw_cols], 1, FUN = function(x) mean(quantile(x, probs = 0.975)))
  
  return(data.frame(mean_diff, lower_percentile, upper_percentile))
}

# Define a function to process data
process_data <- function(source_num, source_rate, heading, metric_id_num, metric_id_rate) {
  # Filter rows for 1990 and 2021
  rate_1990 <- source_rate[source_rate$year_id == 1990, ]
  rate_2021 <- source_rate[source_rate$year_id == 2021, ]
  num_2021 <- source_num[source_num$year_id == 2021, ]
  
  # Calculate percentage differences
  draw_cols <- grep("draw_", names(source_rate), value = TRUE)
  modified_rows <- calculate_percentage_diff(rate_1990, rate_2021, draw_cols)
  
  # Update year_id for the generated rows
  modified_rows$year_id <- "(2021-1990)/1990"
  
  # Copy values from 1990 and 2021 rows for other columns
  other_cols <- setdiff(names(source_rate), c("year_id", draw_cols))
  for (col in other_cols) {
    modified_rows[[col]] <- ifelse(modified_rows$year_id == "(2021-1990)/1990", rate_1990[[col]], modified_rows[[col]])
  }
  
  # Calculate draw statistics for 2021
  draw_stats_2021 <- calculate_draw_stats_2021(num_2021, draw_cols, modified_rows)
  
  # Calculate statistics (mean, lower percentile, upper percentile)
  statistics <- calculate_statistics(modified_rows, draw_cols)
  
  # Extract the desired columns
  result <- cbind(modified_rows[c("location_id")], draw_stats_2021, statistics)
  
  # Rename columns with proper headings
  result <- result %>% 
    mutate(
      !!paste0(heading,"_mean_draw_2021") := sprintf("%.0f", mean_draw_2021),
      !!paste0(heading,"_min_draw_2021") := sprintf("%.0f", min_draw_2021),
      !!paste0(heading,"_max_draw_2021") := sprintf("%.0f", max_draw_2021),
      !!paste0(heading,"_mean_diff") := sprintf("%.2f%%", mean_diff * 100),
      !!paste0(heading,"_lower_percentile") := sprintf("%.2f%%", lower_percentile * 100),
      !!paste0(heading,"_upper_percentile") := sprintf("%.2f%%", upper_percentile * 100)
    ) %>%
    select(-mean_diff, -lower_percentile, -upper_percentile, -mean_draw_2021, -min_draw_2021, -max_draw_2021)
  
  return(result)
}

#loading file needed for the location_name, location_sdi
location_f <- read.csv("../column_id/shared_ids_location.csv", header = TRUE)
location_f <- location_f[c("location_id", "location_name")]
sdi_f <- read.csv("../column_id/locations_sdi.csv", header = TRUE)
sdi_f <- sdi_f[c("region_name", "location_id")]

# Process death data
death_df <- read.csv("../GBD 2021 extracts/draws_idio_epilepsy_deaths_all_age_all_metrics.csv", header = TRUE)
death_num <- filter(death_df, metric_id == 1, age_group_id == 22, sex_id == 3)
death_rate <- filter(death_df, metric_id == 3, age_group_id == 27, sex_id == 3)
death_result <- process_data(death_num, death_rate, "death", 1, 3)

# Process prevalence data
prevalence_df <- read.csv("../GBD 2021 extracts/draws_idiopathic_epilepsy_prevalence.csv", header = TRUE)
pop_df <- read.csv("../column_id/population.csv", header = TRUE) %>% 
  filter(age_group_id == 22, year_id == 2021) %>% select(location_id, year_id, sex_id, population)
prevalence_rate <- filter(prevalence_df, metric_id == 3, age_group_id == 27, sex_id == 3)
#multiply old draw values by population size accordingly
prevalence_num <- merge(prevalence_rate, pop_df, all = FALSE) %>% mutate(across(starts_with("draw_"), ~ . * population))
prevalence_result <- process_data(prevalence_num, prevalence_rate, "prevalence", 1, 3)

# Process DALY data
daly_num <- read.csv("../GBD 2021 extracts/draws_idio_epilepsy_dalys_all_age_num.csv", header = TRUE)%>% 
  filter(metric_id == 1, age_group_id == 22, sex_id == 3, year_id == 1990 | year_id == 2021)
daly_rate <- read.csv("../GBD 2021 extracts/draws_idio_epilepsy_dalys_all_age_rate.csv", header = TRUE)%>% 
  filter(metric_id == 3, age_group_id == 27, sex_id == 3, year_id == 1990 | year_id == 2021)
daly_result <- process_data(daly_num, daly_rate,"daly", 1, 3)

# Merge the dataframes using the inner_join function
merged_result <- inner_join(death_result, prevalence_result, by = "location_id") %>%
  inner_join(daly_result, by = "location_id")
merged_result <- merge(location_f, merged_result, by = "location_id", all = TRUE)

#Merge the sdi locations, then avg (the differences) and sum (the draws) according to their categorization
merged_sdi <- inner_join(merged_result, sdi_f, by = "location_id") %>% 
  select(-c("location_name", "location_id"))

#summarize by sdi categorizations
grouped_sdi <- merged_sdi %>%
  group_by(region_name) %>%
  summarize(
    across(ends_with("_diff"), ~ sprintf("%.2f%%", mean(as.numeric(gsub("%", "", .))))),
    across(ends_with("_percentile"), ~ sprintf("%.2f%%", mean(as.numeric(gsub("%", "", .))))),
    across(ends_with("_2021"), ~ sum(as.numeric(gsub("[^0-9.]", "", .)))),
    .groups = "drop"
  )
# Rename the 'region_name' column to 'location_name' in 'grouped_sdi'
# Ensure that the columns in 'grouped_sdi' and 'merged_result' have the same data types
grouped_sdi <- grouped_sdi %>%
  rename(location_name = region_name) 

# Define the desired order of location names
desired_order <- c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI")

# Reorder the rows in 'grouped_sdi' based on the desired order and align the columns of both DataFrames
grouped_sdi <- grouped_sdi %>%
  arrange(match(location_name, desired_order)) %>%
  mutate(across(everything(), as.character)) %>% 
  mutate(location_id = 0)
grouped_sdi <-  grouped_sdi[, names(merged_result)]

# Load region file and reorder the DataFrame according to the regions
#region_f <- read.csv("../column_id/shared_ids_location_region.csv", header = TRUE)
#ordered_result <- inner_join(region_f, merged_result, by = "location_id") %>% 
#  select(c(-region_name, -location_name.x)) %>% rename(location_name = location_name.y)

# Bind 'grouped_sdi' on top of 'ordered_result'
#final_result <- bind_rows(grouped_sdi, ordered_result)
final_result <- bind_rows(grouped_sdi, merged_result)
final_result <- na.omit(final_result)

# Extract the global row
#global_row <- final_result[final_result$location_name == "Global", ]

# Remove the global row from the dataframe
#final_result <- final_result[final_result$location_name != "Global", ]

# Bind the global row at the top of the dataframe
#final_result <- rbind(global_row, final_result)

# Extract the reults to a csv file
write.csv(final_result, "../excel_tables/Table1_Deaths,prevalence,and_DALYs_for_idiopathic_epilepsy_in_2021,and_percentage_change_to_1990_in_age-standardised_rates_by_location.csv", row.names = FALSE)
