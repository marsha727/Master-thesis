library(pracma)
library(tidyverse)
library(astsa)
library(ggpubr)
library(ggpmisc)

AFPS_int_TS <- read_rds("App/AFPS_int_TS.rds")


columns <- c("SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3")

peak_T3 <- findpeaks(AFPS_int_TS$TENSIO3, minpeakheight = 17, minpeakdistance = 4)
print(peak_T3)

extracted_peaks <- lapply(1:nrow(peak_T3), function(i){
  start_index <- peak_T3[i, 3]
  end_index <- peak_T3[i, 4]
  peaks_in_range <- AFPS_int_TS[start_index:end_index, ]
  print(peaks_in_range)
  
  # Convert datetime to numeric for time differences
  time_diff <- as.numeric(diff(peaks_in_range$datetime))
  
  # Calculate the slope for each column
  slopes <- lapply(peaks_in_range[, -1], function(column) {
    diff_column <- diff(column)
    slope_column <- c(NA, diff_column / time_diff)
    return(slope_column)
  })
  
  for (j in seq_along(slopes)) {
    col_name <- paste(names(peaks_in_range)[j + 1], "_slope", sep = "")
    peaks_in_range[[col_name]] <- slopes[[j]]
  }
  
  peaks_in_range$cycle_number <- i
  return(peaks_in_range)
})

extracted_peaks_TS <- do.call(rbind, extracted_peaks)

drying_cycle <- lapply(1:nrow(peak_T3), function(i){
  drying <- peak_T3[i, 3]
  max <- peak_T3[i, 2]
  peaks_in_range <- AFPS_int_TS[drying:max, ]
  return(peaks_in_range)
})

drying_TS <- do.call(rbind, drying_cycle)

wetting_cycle <- lapply(1:nrow(peak_T3), function(i){
  wetting <- peak_T3[i, 4]
  max <- peak_T3[i, 2]
  peaks_in_range <- AFPS_int_TS[wetting:max, ]
  return(peaks_in_range)
})

wetting_TS <- do.call(rbind, wetting_cycle)

ggplot(extracted_peaks_TS, aes(x = SENTEK1, y = TENSIO3 )) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() +
  labs(title = "Correlation plot") +
  stat_correlation(mapping = use_label(c("R", "P")), 
                   size = 4, 
                   label.x = "left", 
                   label.y = "top"
  )
 
ggplot(wetting_TS, aes(x = SENTEK1, y = TENSIO3)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() +
  labs(title = "Correlation plot") +
  stat_correlation(mapping = use_label(c("R", "P")), 
                   size = 4, 
                   label.x = "left", 
                   label.y = "top"
  ) 
  

# Calculate correlation separately for each cycle
correlation_data <- extracted_peaks_TS %>%
  group_by(cycle_number) %>%
  summarise(correlation = cor(SENTEK1, TENSIO2, method = "pearson"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_peaks_TS, aes(x = SENTEK1, y = TENSIO2)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() +
  labs(title = "Correlation per cycle") +
  facet_wrap(~cycle_number, scales = "free") +
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")), 
                                    size = 4, 
                                    label.x = "left", 
                                    label.y = "top"
                                    )

ggplot(extracted_peaks_TS) +
  geom_point(aes(x = datetime, y = SENTEK1, color = factor(cycle_number))) +
  geom_point(aes(x = datetime, y = TENSIO2, color = factor(cycle_number))) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "orange", "5" = "purple")) +
  # You can customize colors as needed
  labs(color = "Cycle Number") +
  theme_minimal()


