### Graphs of agricultural sensors data - Temperature measure at leaves  ### 

Sys.setlocale("LC_TIME", "English")
# Based on real data, after anonymization and randomization.

library(dplyr)
library(ggplot2)
library(lubridate)


##### Importing ####

df_leaf <- readr::read_csv(
  "./data/leaf_temp.csv",
  col_select = -1)

df_leaf <- df_leaf %>% 
  mutate(
    sensor = factor(sensor, ordered = TRUE))

##### Wrangling

# Keeping only interval of interest

df_leaf_day <- df_leaf %>% 
  filter(date_time %within% interval("2021-05-25", "2021-09-14"))

# averaging daily temperatures #

df_leaf_day <- df_leaf_day %>% 
  mutate(
    date = as.Date(date_time)) %>% 
  group_by(sensor, date) %>% 
  summarise(
    leaf_temp_daily = mean(leaf_temp)) %>% 
  ungroup()

# Checking if all days are represented in the dataset

date_range <- min(df_leaf_day$date):max(df_leaf_day$date)
date_range <- as.Date(date_range)

if(all(unique(df_leaf_day$date) %in% date_range) == FALSE){
  stop("There are days with missing data")
}

remove(date_range)

# Adding all days for all sensors
# useful for moving averages

df_leaf_day <- df_leaf_day %>% 
  tidyr::complete(
    sensor, date) %>% 
  arrange(sensor,date)


##### Graph  #### 

# Color blind-friendly palette

palette_plot <- c("#000000", "#E69F00","#56B4E9", "#009E73")


# Plot

plot_leaf_temp <- ggplot(
  df_leaf_day,
  mapping = aes(
    x = date,
    y = leaf_temp_daily)) +

  geom_smooth(
    aes(colour = sensor),
    se = FALSE,
    method = "loess",
    span = 0.2,
    linewidth = 0.5)

# Scales

plot_leaf_temp <- plot_leaf_temp +
  scale_x_date(
    date_breaks = "14 days",
    date_labels = "%b-%d") +

scale_color_manual(values = palette_plot)


# Labels

plot_leaf_temp <- plot_leaf_temp +
  xlab("Date") +
  ylab("Leaf temperature (°C)")

# Formatting


plot_leaf_temp <- plot_leaf_temp +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "black", size = 12, vjust = 2.5),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y =  element_text(color = "black", size = 8),
    axis.text.x = element_text(color = "black", size = 8, angle = 45, hjust = 1)
  )


# Showing plot

plot_leaf_temp

