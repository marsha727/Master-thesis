library(pracma)
library(tidyverse)
library(astsa)
library(ggpubr)
library(ggpmisc)

AFPS_int_TS <- read_rds("App/AFPS_int_TS.rds")


columns <- c("SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3")

#peaks_list <- lapply(columns, function(col){
  #peak_indices <- findpeaks(AFPS_int_TS[[col]], minpeakheight = 17)
  #peaks_dates <- AFPS_int_TS$datetime[peak_indices]
  #return(data.frame(datetime = peaks_dates, peak = peak_indices))
#})

peak_T3 <- findpeaks(AFPS_int_TS$TENSIO3, minpeakheight = 17)
print(peak_T3)

extracted_peaks <- lapply(1:nrow(peak_T3), function(i){
  start_index <- peak_T3[i, 3]
  end_index <- peak_T3[i, 4]
  peaks_in_range <- AFPS_int_TS[start_index:end_index, ]
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


extracted_peaks_TS <- extracted_peaks_TS %>%
  group_by(cycle_number) %>%
  mutate(correlation = cor(SENTEK3, TENSIO3, method = "pearson"))


correlation_plots <- extracted_peaks_TS %>% 
  ggplot(aes(x = SENTEK3, y = TENSIO3)) +
  geom_point() +
  facet_wrap(~cycle_number, scales = "free") + 
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Correlation per month") +
  geom_text(aes(label = sprintf("R = %.2f", correlation)))
  
print(correlation_plots)


# Calculate correlation separately for each cycle
correlation_data <- extracted_peaks_TS %>%
  group_by(cycle_number) %>%
  summarise(correlation = cor(SENTEK3, TENSIO3, method = "pearson"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_peaks_TS, aes(x = SENTEK3, y = TENSIO3)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Correlation per cycle") +
  facet_wrap(~cycle_number, scales = "free") +
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)


correlation_plot + stat_correlation(mapping = use_label(c("R", "P")), 
                                    size = 4, 
                                    label.x = "left", 
                                    label.y = "top"
                                    )

