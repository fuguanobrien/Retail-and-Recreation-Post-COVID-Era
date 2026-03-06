# downloading necessary packages and data
library(tidyverse)
library(ggplot2)
options(repr.plot.width = 16, repr.plot.height = 8) 
library(dplyr)
library(lubridate)
library(gridExtra)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
library(scales)
library(stringr)
library(grid)
library(cowplot)

data <- read_csv('joined_df.csv')


plot_data_map <- data %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  group_by(state, month) %>%
  summarize(
    mean_retail_and_recreation = mean(retailAndRecreation, na.rm = TRUE),
    .groups = "drop"
  )

# create "Other States" monthly averages
other_states_data <- plot_data_map %>%
  filter(state != "California") %>%
  group_by(month) %>%
  summarize(
    mean_retail_and_recreation = mean(mean_retail_and_recreation, na.rm = TRUE),
    state = "Other States",
    .groups = "drop"
  )

# Combine California + Other States
combined_data <- bind_rows(
  plot_data_map %>% filter(state == "California"),
  other_states_data
)

#percentile
other_states_percentiles <- plot_data_map %>%
  filter(state != "California") %>%   # all non-CA states, many rows per month
  group_by(month) %>%
  summarize(
    median_val = quantile(mean_retail_and_recreation, 0.5, na.rm = TRUE),
    lower_band = quantile(mean_retail_and_recreation, 0.25, na.rm = TRUE),
    upper_band = quantile(mean_retail_and_recreation, 0.75, na.rm = TRUE),
    state = "Other States",
    .groups = "drop"
  )

# Plot
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_line(
    data = combined_data %>% filter(state == "Other States"),
    aes(x = month, y = mean_retail_and_recreation),
    color = "#e5ba72", alpha = 0.6, size = 2
  ) +
  geom_ribbon(
    data = other_states_percentiles,
    aes(x = month, ymin = lower_band, ymax = upper_band),
    fill = "#e5ba72",
    alpha = 0.25
  ) +
  geom_line(
    data = combined_data %>% filter(state == "California"),
    aes(x = month, y = mean_retail_and_recreation),
    color = "#466362", size = 2
  ) +
  labs(
    x = NULL,
    y = "Mean Retail & Recreation Value (%)",
    title = "California's Recovery Trails U.S. Trend Throughout the Pandemic"
  )+
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
  )

  # annotations added in after in presentation 

state_df <- data %>%
  mutate(month = floor_date(date, unit = "month")) %>%
  group_by(state, month) %>%
  summarize(mean_retail_and_recreation = mean(retailAndRecreation, na.rm = TRUE), .groups = "drop")

# earliest and latest month in the dataset
early_month  <- min(state_df$month)
latest_month <- max(state_df$month)

# statewide change table
state_change_table <- state_df %>%
  filter(month %in% c(early_month, latest_month)) %>%
  mutate(month_label = ifelse(month == early_month, "early_value", "late_value")) %>%
  select(state, month_label, mean_retail_and_recreation) %>%
  tidyr::pivot_wider(
    names_from  = month_label,
    values_from = mean_retail_and_recreation
  ) %>%
  mutate(change = late_value - early_value) %>%
  arrange(change)

state_plot <- state_change_table %>%
  mutate(
    # create a factor ordered descending by change
    state_factor = factor(state, levels = state[order(-change)]),
    
    color_flag = case_when(
      state == "California" ~ "California",
      state == "Maine"      ~ "Other",
      TRUE                  ~ "Other"
    ),
    
    label_color = case_when(
      state == "California" ~ "#76a5a4", 
      state == "Maine"      ~ "white", 
      TRUE                  ~ "grey50"
    ),
    label_pos  = change + ifelse(change >= 0, 0.5, -0.5),
    label_text = paste0(round(change, 0), "%")
  ) %>%
  ggplot(aes(x = change, y = state_factor, fill = color_flag)) +
  geom_col() +
  # bar labels
  geom_text(
    aes(
      label = label_text,
      x = label_pos,
      color = label_color,
      hjust = ifelse(change >= 0, 0, 1)
    ),
    size = 3,
    show.legend = FALSE
  ) +
  geom_vline(xintercept = 0, color = "darkgrey", linewidth = 0.5) +
  geom_text(
    aes(
      x = 0.1, 
      y = state_factor,
      label = state,
      color = case_when(
        state == "California" ~ "#466362",
        TRUE                  ~ "black"
      )
    ),
    hjust = 0,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("California" = "#76a5a4", "Other" = "grey80")) +
  scale_color_identity() +
  labs(
    title = "California Shows One of the Largest Post-Pandemic Declines in Retail & Recreation",
    x = "Net Percent Change over 2020-2022",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

state_plot

data_summary <- data %>%
  mutate(date = as.Date(date)) %>%
  group_by(county, date) %>%
  summarise(
    mean_retail_and_recreation = mean(retailAndRecreation, na.rm = TRUE),
    .groups = "drop"
  )

# earliest and latest dates
latest_date <- max(data_summary$date, na.rm = TRUE)
early_date  <- min(data_summary$date, na.rm = TRUE)

county_change_table <- data_summary %>%
  filter(date %in% c(early_date, latest_date)) %>%
  mutate(date_label = ifelse(date == early_date, "early_value", "late_value")) %>%
  select(county, date_label, mean_retail_and_recreation) %>%
  tidyr::pivot_wider(
    names_from = date_label,
    values_from = mean_retail_and_recreation
  ) %>%
  mutate(change = late_value - early_value) %>%
  arrange(change)

mean_county_change <- mean(county_change_table$change, na.rm = TRUE)
# mean_county_change -- average for bay area is 23%

county_change_table <- county_change_table %>%
  mutate(
    county_lower = county |> 
      tolower() |> 
      str_replace(" county$", "") |> 
      str_trim(),
    change = as.numeric(change)
  )

ca_map <- map_data("county") %>%
  filter(region == "california") %>%
  mutate(subregion = str_trim(subregion))

ca_map_joined <- ca_map %>%
  left_join(county_change_table, by = c("subregion" = "county_lower"))

bay_area_counties <- c(
  "san francisco", "san mateo", "santa clara", "alameda",
  "contra costa", "solano", "napa", "sonoma", "marin"
)

bay_area_map <- ca_map_joined %>%
  filter(subregion %in% bay_area_counties)

# Compute centroids for Bay Area labels, skipping NAs
bay_area_labels <- bay_area_map %>%
  group_by(subregion) %>%
  summarise(
    lon = mean(long),
    lat = mean(lat),
    change_value = mean(change, na.rm = TRUE)  # ignore NA
  ) %>%
  filter(!is.na(change_value)) %>%             # remove counties with NA
  mutate(
    change_label = paste0(round(change_value, 0), "%")  # format as percent
  )

# color scale
fill_scale <- scale_fill_gradient2(
  low = "#466362",
  mid = "#f7f7f7",
  high = "#466362",
  midpoint = 0,
  name = "Net Change",
  na.value = "gray90"
)
# US map
plot_full_state <- ggplot() +
  geom_polygon(
    data = ca_map_joined,
    aes(x = long, y = lat, group = group, fill = change),
    color = "white", linewidth = 0.25
  ) +
  coord_map("albers", lat0 = 30, lat1 = 45) +
  fill_scale +
  labs(title = "California", subtitle = "Net Change Across Counties") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA), # transparent bg
    panel.background = element_rect(fill = "transparent", color = NA) # transparent bg
  )

options(repr.plot.width = 12, repr.plot.height = 8) 

# bay area labels with San Francisco in white text
plot_bay_area <- ggplot() +
  geom_polygon(
    data = bay_area_map,
    aes(x = long, y = lat, group = group, fill = change),
    color = "white", linewidth = 0.25
  ) +
  geom_text(
    data = bay_area_labels,
    aes(x = lon, y = lat, label = change_label),
    color = ifelse(bay_area_labels$subregion == "san francisco", "white", "black"),
    size = 3
  ) +
  coord_map("albers", lat0 = 30, lat1 = 45) +
  fill_scale +
  labs(title = "Bay Area Counties", subtitle = "Net Change (Zoomed In)") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    plot.background = element_rect(fill = "transparent", color = NA), # transparent bg
    panel.background = element_rect(fill = "transparent", color = NA) # transparent bg
  )

# legend
legend_plot <- ggplot() +
  geom_polygon(
    data = ca_map_joined,
    aes(x = long, y = lat, group = group, fill = change)
  ) +
  fill_scale +
  theme_void() +
  theme(legend.position = "right",
        plot.background = element_rect(fill = "transparent", color = NA), # transparent bg
        panel.background = element_rect(fill = "transparent", color = NA) # transparent bg
  )

legend <- cowplot::get_legend(legend_plot)

# side by side charts
map_panel <- cowplot::plot_grid(
  plot_full_state, plot_bay_area,
  ncol = 2,
  align = "h"
)

combined_with_legend <- cowplot::plot_grid(
  map_panel, legend,
  ncol = 2,
  rel_widths = c(1, 0.12)
)

final_plot <- cowplot::plot_grid(
  cowplot::ggdraw() + cowplot::draw_label(
    "Bay Area Experiences 10% Greater Average Decline Than Other California Counties",
    fontface = "bold",
    x = 0.5,
    hjust = 0.5,
    size = 16
  ),
  combined_with_legend,
  ncol = 1,
  rel_heights = c(0.08, 1)  # title takes 8% of height
) +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA)
  )

final_plot

data <- data %>%
  mutate(date = as.Date(date)) %>%
  arrange(state, date)

daily <- data %>%
  group_by(state) %>%
  mutate(daily_change = retailAndRecreation - lag(retailAndRecreation)) %>%
  summarise(perDay = mean(daily_change, na.rm = TRUE))

monthly <- data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(state, month) %>%
  summarise(month_avg = mean(retailAndRecreation), .groups = "drop") %>%
  group_by(state) %>%
  mutate(month_change = month_avg - lag(month_avg)) %>%
  summarise(perMonth = mean(month_change, na.rm = TRUE))

yearly <- data %>%
  mutate(year = floor_date(date, "year")) %>%
  group_by(state, year) %>%
  summarise(year_avg = mean(retailAndRecreation), .groups = "drop") %>%
  group_by(state) %>%
  mutate(year_change = year_avg - lag(year_avg)) %>%
  summarise(perYear = mean(year_change, na.rm = TRUE))

RateOfChange <- daily %>%
  left_join(monthly, by = "state") %>%
  left_join(yearly, by = "state")


RateOfChange <- RateOfChange %>%
  mutate(
    perDay = as.numeric(perDay), 
    perMonth = as.numeric(perMonth),
    perYear = as.numeric(perYear)
  )

# Sort by perYear in ascending order
state_variation <- RateOfChange %>%
  arrange(perYear) %>%
  mutate(
    state = factor(state, levels = state),
    highlight = ifelse(state == "California", "California", "Other")
  )

# lollipop chart
ggplot(state_variation, aes(x = state, y = perYear, color = highlight)) +
  geom_segment(aes(x = state, xend = state, y = 0, yend = perYear), linewidth = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("California" = "#76a5a4", "Other" = "lightgrey")) +
  labs(
    title = "Rate of Recovery per Year by State",
    subtitle = "Average year-over-year change in retail and recreation activity. California highlighted in teal.",
    x = NULL,
    y = "Percentage Points per Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      color = ifelse(levels(state_variation$state) == "California", "#76a5a4", "black")
    ),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
