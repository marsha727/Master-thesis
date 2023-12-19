library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(ggquiver)
library(dtwclust)

AFPS_int_TS <- readRDS("App/AFPS_int_TS.rds")

# Assuming AFPS_int_TS is your dataframe and datetime is in POSIXct format

# Extract month names and create a new variable
AFPS_int_TS$month_name <- month(AFPS_int_TS$datetime)

# Plot the graph
p <- ggplot(AFPS_int_TS) +
  geom_path(aes(x = TENSIO3, y = SENTEK3, color = month_name), size = 1.5) +
  labs(
    title = "Hysteresis drying and wetting cycles",
    x = "TENSIO AFPS (mm)",
    y = "SENTEK AFPS (mm)"
  ) +
  #scale_color_viridis_d(option = "inferno")
  scale_color_distiller(palette = "RdYlGn", direction = 1, guide = guide_legend(title = "Month"), labels = month.abb[unique(AFPS_int_TS$month_name)]) +
  theme_minimal()

# Create a data frame for arrows
arrow_data <- data.frame(
  x = head(AFPS_int_TS$TENSIO3, n = -1),
  y = head(AFPS_int_TS$SENTEK3, n = -1),
  xend = tail(AFPS_int_TS$TENSIO3, n = -1),
  yend = tail(AFPS_int_TS$SENTEK3, n = -1)
)

# Add arrows
arrow_size <- 0.05  # Adjust the arrow size as needed
arrow_color <- "black"

p + geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(type = "closed", length = unit(arrow_size, "inches")), 
                 color = arrow_color, size = 0, lineend = "butt")




#manual segmentation
sensors <- c("SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3")

result_segment <- data.frame(datetime = AFPS_int_TS$datetime,
                             cycle_type = "None")

rate_threshold <- 0.5

for(sensor in sensors){
  
  AFPS <- AFPS_int_TS[[sensor]]
  
  rate_of_change <- c(NA, diff(AFPS))
  
  drying_indices <- which(rate_of_change < -rate_threshold)
  wetting_indices <- which(rate_of_change > rate_threshold)
  
  result_segment[[paste(sensor, "cycle_type", sep = "_")]] <- "None"
  result_segment[[paste(sensor, "cycle_type", sep = "_")]][drying_indices] <- "Drying"
  result_segment[[paste(sensor, "cycle_type", sep = "_")]][wetting_indices] <- "Wetting"
}

# Optionally, you can combine the results into a single column for easier analysis
result_segment$combined_cycle_type <- rowwise(result_segment) %>%
  select(starts_with("cycle_type")) %>%
  apply(1, function(x) ifelse("Drying" %in% x, "Drying", ifelse("Wetting" %in% x, "Wetting", "None")))
















