library(pracma)
library(tidyverse)
library(ggpmisc)
library(reshape2)

AFPS_int_TS <- readRDS("App/AFPS_int_TS.rds")

columns <- c("SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3")

#I extracted the data for only the peaks using findpeaks
#created data set exclusively for peaks: extracted_peaks_TS
#Also have seperated wet and dry cycled in drying_TS and wetting_TS

peak_T3 <- findpeaks(AFPS_int_TS$TENSIO3, minpeakheight = 17, minpeakdistance = 4)
print(peak_T3)

extracted_peaks <- lapply(1:nrow(peak_T3), function(i){
  start_index <- peak_T3[i, 3] #the 3th row is the start index number
  end_index <- peak_T3[i, 4] #the 4th is the end index number
  peaks_in_range <- AFPS_int_TS[start_index:end_index, ]
  print(peaks_in_range)
  
  # Convert datetime to numeric for time differences
  time_diff <- as.numeric(diff(peaks_in_range$datetime))
  
  peaks_in_range$cycle_number <- i
  return(peaks_in_range)
})

extracted_peaks_TS <- do.call(rbind, extracted_peaks)

#Same style but this time row 2-3 = drying cycle
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

#continue working on extracted peaks dataframe

#Because the numbers are not chronological and peak 4 and 5 are the same
extracted_peaks_TS <- extracted_peaks_TS %>%
  mutate(cycle = case_when(
    cycle_number == 1 ~ 3,
    cycle_number == 3 ~ 1,
    cycle_number == 4 ~ 4,
    cycle_number == 5 ~ 4,
    TRUE ~ cycle_number  # Keep the original value if none of the conditions are met
  ))

#Analysis of the data using ggplot and later on the correlaiton map

#all data together still hysteresis or bad correlation
ggplot(extracted_peaks_TS, aes(x = SENTEK1, y = TENSIO3 )) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() +
  labs(title = "Correlation plot") +
  stat_correlation(mapping = use_label(c("R", "P")), 
                   size = 4, 
                   label.x = "left", 
                   label.y = "top"
  )
 
#Only the wetting cycles still not great
ggplot(wetting_TS, aes(x = SENTEK1, y = TENSIO3)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() +
  labs(title = "Correlation plot") +
  stat_correlation(mapping = use_label(c("R", "P")), 
                   size = 4, 
                   label.x = "left", 
                   label.y = "top"
  ) 
  

#correlation here
# Calculate correlation separately for each cycle
correlation_data <- extracted_peaks_TS %>%
  group_by(cycle) %>%
  summarise(correlation = cor(SENTEK1, TENSIO2, method = "pearson"))

# Create a scatter plot with a single correlation coefficient for each cycle
correlation_plot <- ggplot(extracted_peaks_TS, aes(x = SENTEK1, y = TENSIO2)) +
  stat_smooth(method = "lm", col = "red") +
  geom_point() +
  labs(title = "Correlation per cycle") +
  facet_wrap(~cycle, scales = "free") +
  geom_text(data = correlation_data, aes(x = Inf, y = -Inf, label = sprintf("R = %.2f", correlation)), vjust = 1, hjust = 1, size = 3)

#stat correlation for plotting and computing stats
correlation_plot + stat_correlation(mapping = use_label(c("R", "P")), 
                                    size = 4, 
                                    label.x = "left", 
                                    label.y = "top"
                                    )

ggplot(extracted_peaks_TS) +
  geom_point(aes(x = datetime, y = SENTEK1, color = factor(cycle))) +
  geom_point(aes(x = datetime, y = TENSIO2, color = factor(cycle))) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "orange", "5" = "purple")) +
  # You can customize colors as needed
  labs(color = "Cycle Number") +
  theme_minimal()


#Create correlation plots per cycle

subset_list <- extracted_peaks_TS %>%
  group_split(cycle)

# Calculate correlation matrices for each subset
cor_matrices <- lapply(subset_list, function(subset_data) {
  cor_matrix <- cor(subset_data[, 2:6], method = "spearman", use = "pairwise.complete.obs")
  M <- cor_matrix
  M[upper.tri(M)] <- NA
  diag(M) <- NA
  melt(M, na.rm = TRUE)
})

# Create separate heatmaps for each cycle
heatmap_plots <- lapply(seq_along(cor_matrices), function(i) {
  ggplot(cor_matrices[[i]], aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))), vjust = 1) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
    theme_minimal() +
    labs(title = paste("Cycle", as.numeric(names(cor_matrices)[i])))
})

library(gridExtra)
grid.arrange(grobs = heatmap_plots, ncol = 3)
