#Total porosity test
Subset_Bodem_fysische_metingen <- read.csv2("Datasets/MvG_Bodem_fysische_metingen.csv")

Subset_Bodem_fysische_metingen$a <- as.numeric(Subset_Bodem_fysische_metingen$a)
Subset_Bodem_fysische_metingen$begindiepte <- as.numeric(Subset_Bodem_fysische_metingen$begindiepte)
Subset_Bodem_fysische_metingen$einddiepte <- as.numeric(Subset_Bodem_fysische_metingen$einddiepte)

depth_to_interpolate <- 50

#makes a dataframe so the y value is attached to end and begin depth for all MvG parameters
#WCS
interpolated_WCS_b <- spline(Subset_Bodem_fysische_metingen$begindiepte[1:4], Subset_Bodem_fysische_metingen$WCS[1:4], xout = depth_to_interpolate)$y
interpolated_WCS_e <- spline(Subset_Bodem_fysische_metingen$einddiepte[1:4], Subset_Bodem_fysische_metingen$WCS[1:4], xout = depth_to_interpolate)$y
interpolated_WCS = mean(c(interpolated_WCS_b, interpolated_WCS_e))
#a
interpolated_a_b <- spline(Subset_Bodem_fysische_metingen$begindiepte[1:4], Subset_Bodem_fysische_metingen$a[1:4], xout = depth_to_interpolate)$y
interpolated_a_e <- spline(Subset_Bodem_fysische_metingen$einddiepte[1:4], Subset_Bodem_fysische_metingen$a[1:4], xout = depth_to_interpolate)$y
interpolated_a = mean(c(interpolated_a_b, interpolated_a_e))
#n
interpolated_n_b <- spline(Subset_Bodem_fysische_metingen$begindiepte[1:4], Subset_Bodem_fysische_metingen$n[1:4], xout = depth_to_interpolate)$y
interpolated_n_e <- spline(Subset_Bodem_fysische_metingen$einddiepte[1:4], Subset_Bodem_fysische_metingen$n[1:4], xout = depth_to_interpolate)$y
interpolated_n = mean(c(interpolated_n_b, interpolated_n_e))
#m
interpolated_m_b <- spline(Subset_Bodem_fysische_metingen$begindiepte[1:4], Subset_Bodem_fysische_metingen$m[1:4], xout = depth_to_interpolate)$y
interpolated_m_e <- spline(Subset_Bodem_fysische_metingen$einddiepte[1:4], Subset_Bodem_fysische_metingen$m[1:4], xout = depth_to_interpolate)$y
interpolated_m = mean(c(interpolated_m_b, interpolated_m_e))

new_row <- Subset_Bodem_fysische_metingen %>% 
  summarize(
    Locatie = "INT",
    begindiepte = depth_to_interpolate,
    einddiepte = depth_to_interpolate + 5,
    WCS = interpolated_WCS,
    WCR = 0,
    a = interpolated_a,
    n = interpolated_n,
    m = interpolated_m,
    across(.cols = -c(Locatie, begindiepte, einddiepte, WCS, WCR, a, n, m), .fns = ~NA)
  )

Subset_Bodem_fysische_metingen_int <- rbind(Subset_Bodem_fysische_metingen, new_row)
Subset_Bodem_fysische_metingen_int <- Subset_Bodem_fysische_metingen_int[-4,]

TPS_av = sum((Subset_Bodem_fysische_metingen$WCS * (Subset_Bodem_fysische_metingen$einddiepte - Subset_Bodem_fysische_metingen$begindiepte))/sum(Subset_Bodem_fysische_metingen$einddiepte - Subset_Bodem_fysische_metingen$begindiepte))

