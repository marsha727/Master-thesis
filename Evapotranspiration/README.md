# Evapotranspiration

In this script the evapotranspiration data from the LAW_MS_ICOS file are further filtered:
  -  Filter for the LE flags of 2 (already included in dataset)
  -  Outlier filtering with 99th percentile
  -  Filter for negative daily ET values when no hazy conditions occur

The evaporative fraction is calculated
  -  Evaporative fraction is calculated from the ratio of latent heat flux to the sum of the latent and sensible heat flux
