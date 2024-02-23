library(tidyverse)
library(astsa)

AFPS_int_TS <- readRDS("App/AFPS_int_TS.rds")

#some cross-correlation testing
print(ccf(AFPS_int_TS$SENTEK3, AFPS_int_TS$TENSIO2))

lag2.plot(na.omit(AFPS_int_TS$OWASIS), na.omit(AFPS_int_TS$SENTEK1), max.lag = 4)

hist(AFPS_int_TS$TENSIO3)

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




