library(tidyverse)

#already aggregated for day
AFPS_int_TS <- readRDS("App/AFPS_int_TS.rds") #File just containing AFPS 

# Extract month names and create a new variable
AFPS_int_TS$month_name <- month(AFPS_int_TS$datetime)

# Plot the graph
p <- ggplot(AFPS_int_TS) +
  geom_path(aes(x = TENSIO3, y = SENTEK1, color = month_name), linewidth = 1.5) +
  labs(
    title = "Hysteresis drying and wetting cycles",
    x = "TENSIO AFPS (mm)",
    y = "SENTEK AFPS (mm)"
  ) +
  #scale_color_viridis_d(option = "inferno")
  scale_color_distiller(palette = "RdYlGn", direction = 1, guide = guide_legend(title = "Month"), labels = month.abb[unique(AFPS_int_TS$month_name)]) +
  theme_minimal()

# Create a data frame for arrows
#It is calculated for each point, so a new arrow between each point
arrow_data <- data.frame(
  x = head(AFPS_int_TS$TENSIO3, n = -1), #start of tensio point
  y = head(AFPS_int_TS$SENTEK1, n = -1), #start of sentek point
  xend = tail(AFPS_int_TS$TENSIO3, n = -1), #end of tensio point
  yend = tail(AFPS_int_TS$SENTEK1, n = -1) #end of sentek point
)

# Add arrows
arrow_size <- 0.05  # Adjust the arrow size as needed
arrow_color <- "black"

p + geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(type = "closed", length = unit(arrow_size, "inches")), 
                 color = arrow_color, size = 0, lineend = "butt")
