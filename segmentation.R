library(pracma)
library(tidyverse)

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