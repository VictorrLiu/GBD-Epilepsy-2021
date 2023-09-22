################################################################################
# Bar charts for The impact of idiopathic epilepsy (YLLs, YLDs, YLLs+YLLs=DALYs) by sex and GBD super-region
# Template/plot style developed by Zichen Liu
# Updated for GBD 2021 epilepsy paper by Victor Liu (victor.liu@aut.ac.nz)
################################################################################
library(dplyr)
library(data.table)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(stringr)
library(scales)
library(RColorBrewer)
library(ggsci)

# Set output path - update this!
output_path <- "../graphs/"

super_region_id_ls <- c(4, 31, 64, 103, 137, 158, 166)
location_f <- read.csv("../column_id/shared_ids_location.csv", header = TRUE) 
location_f <- location_f[c("location_id", "location_name")]

pop_df <- read.csv("../column_id/population.csv", header = TRUE) %>% 
  filter(age_group_id == 22, year_id == 2021, sex_id %in% c(1,2) ) %>% select(location_id, year_id, sex_id, age_group_id, population)

yll_data_f <- read.csv("C:../GBD 2021 extracts/draws_idio_epilepsy_yll_all_age.csv") %>% filter(year_id == 2021, age_group_id == 22, sex_id %in% c(1,2) )

yld_preprocessed_data_f <- read.csv("C:../GBD 2021 extracts/draws_idio_epilepsy_yld_all_age.csv")

yld_data_f <- merge(yld_preprocessed_data_f, pop_df, all = FALSE) %>% 
  mutate(across(starts_with("draw_"), ~ . * population))

yld_data <- yld_data_f %>% mutate(type_id = "yld") %>% filter(location_id %in% super_region_id_ls)
yll_data <- yll_data_f %>% mutate(type_id = "yll") %>% filter(metric_id == 1, location_id %in% super_region_id_ls)
combined_data <- bind_rows(yld_data, yll_data) %>% merge(location_f, by = 'location_id')

combined_data$draw_mean <- rowMeans(combined_data[, paste0("draw_", 1:99)], na.rm = TRUE)
combined_data <- combined_data %>% select(location_name, sex_id, draw_mean, type_id)

# Separate male and female dfs
male_df <- combined_data[combined_data$sex_id == 1, ]
female_df <- combined_data[combined_data$sex_id == 2, ]

# Generate female panel
male_plot <- ggplot(male_df, aes(fill = type_id, y = draw_mean, x = location_name)) + 
  geom_bar(position="stack", stat="identity") +  theme_classic() +
  coord_flip() + scale_y_continuous(limits = c(0, 25000000)) +  # Set y-axis limits
  ggtitle("Male") +
  scale_x_discrete(position = "top") +
  scale_y_reverse() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7), 
        axis.text.y = element_blank(), legend.position = "none") + xlab(element_blank()) + ylab(element_blank()) 

# Generate male panel
female_plot <- ggplot(female_df, aes(fill = type_id, y = draw_mean, x = location_name)) + 
  geom_bar(position="stack", stat="identity") +  
  theme_classic() + coord_flip() + ggtitle("Female") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7), 
        axis.text.y = element_blank(), legend.position = "right", legend.margin = margin(0,70,0,0)) + 
  xlab(element_blank()) + ylab(element_blank()) 

# Generate center axis panel
axis <- ggplot(female_df, x = location_name, y = 1) +
  geom_text(aes(label = location_name, x = location_name, y = 1), position = "identity", size = 3.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  ggtitle("") +
  xlab(NULL) +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = NA),
        axis.ticks.x = element_line(color = NA),
        plot.margin = unit(c(0.2,-1, 1,-1), "cm"))
        
# Create output
pdf(paste0(output_path, "Figure7.The_impact_of_idiopathic_epilepsy_YLLs,YLDs,by_sex_and_GBD_super-region.pdf"), width=15, height=8.5)
grid.arrange(male_plot, axis, female_plot, ncol = 3, widths = c(5.2/13,2.6/13,5.2/13),
             top = text_grob("The impact of idiopathic epilepsy (YLLs, YLDs, YLLs+YLLs=DALYs) by sex and GBD super-region, 2021", size = 12, x = 0.5))
dev.off()
