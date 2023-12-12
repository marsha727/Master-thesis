library(tidyverse)
library(astsa)

AFPS_int_TS <- readRDS("App/AFPS_int_TS.rds")

print(ccf(AFPS_int_TS$SENTEK3, AFPS_int_TS$TENSIO3))

hist(AFPS_int_TS$TENSIO3)

