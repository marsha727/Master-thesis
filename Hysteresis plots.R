library(tidyverse)
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
  geom_path(aes(x = TENSIO3, y = SENTEK3, color = month_name), linewidth = 1.5) +
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







