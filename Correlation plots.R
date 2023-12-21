library(tidyverse)
library(astsa)

AFPS_int_TS <- readRDS("App/AFPS_int_TS.rds")

print(ccf(AFPS_int_TS$SENTEK3, AFPS_int_TS$TENSIO2))

lag2.plot(na.omit(AFPS_int_TS$OWASIS), na.omit(AFPS_int_TS$SENTEK1), max.lag = 4)

hist(AFPS_int_TS$TENSIO3)

d <- decompose(AFPS_int_TS$SENTEK3)

start_date1 <- as.POSIXct("2022-04-01")
end_date1 <- as.POSIXct("2022-07-01")

start_date2 <- as.POSIXct("2022-07-01")
end_date2 <- as.POSIXct("2022-09-25")

AFPS_int_TS1 <- AFPS_int_TS %>% 
  filter(datetime <= end_date1 | datetime >= end_date2)

AFPS_int_TS2 <- AFPS_int_TS %>% 
  filter(datetime >= start_date2 & datetime <= end_date2)

print(ccf(AFPS_int_TS1$SENTEK1, AFPS_int_TS1$TENSIO3))
print(ccf(AFPS_int_TS$SENTEK1, AFPS_int_TS$TENSIO3))

lag2.plot(AFPS_int_TS1$TENSIO3, AFPS_int_TS1$SENTEK1, max.lag = 5)
lag2.plot(AFPS_int_TS2$TENSIO2, AFPS_int_TS2$SENTEK1, max.lag = 5)

#correlaiton heat map

M <- cor(AFPS_int_TS[, 2:6], method = "spearman", use = "pairwise.complete.obs")

# Set both diagonal and upper triangular part to NA
M[upper.tri(M)] <- NA
diag(M) <- NA

library(reshape2)
melted_M <- melt(M, na.rm = TRUE)

library(ggplot2)
ggplot(melted_M, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))), vjust = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Spearman Rank Correlation Heatmap")



M <- cor.test(AFPS_int_TS$OWASIS, AFPS_int_TS$SENTEK3, method = "spearman")$p.value



#normalization
columns_to_normalize <- c("SENTEK1", "SENTEK3", "TENSIO2", "TENSIO3", "OWASIS")

# Custom Min-Max normalization function
min_max_normalize <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

# Apply normalization to the specified columns
normalized_columns <- apply(AFPS_int_TS[, columns_to_normalize], 2, min_max_normalize)

AFPS_int_TS_n <- AFPS_int_TS

AFPS_int_TS_n[, columns_to_normalize] <- normalized_columns


#TENSIO AND SENTEK WITH VOLUME = 60 CM
ggplot(AFPS_int_TS_n) +
  geom_line(aes(x = datetime, y = SENTEK1, color = "SENTEK1"), linewidth = 0.3) +
  geom_line(aes(x = datetime, y = TENSIO2, color = "TENSIO2"), linewidth = 0.3) +
  labs(
    title = "AFPS int (SENTEK  VS TENSIO VS OWASIS) depth: 20-60 and unknown OWASIS",
    x = "Date",
    y = "AFPS int (mm)"
  ) +
  scale_color_manual(
    values = c("SENTEK1" = 'red', "SENTEK3" = "tomato", "TENSIO2" = "blue", "TENSIO3" = "skyblue", "OWASIS" = "darkgreen"),
    name = "Device"
  ) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.width = unit(0.5, "cm")
  )



