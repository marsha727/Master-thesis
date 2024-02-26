# AFPS scripts

The subfolders include the scripts that include calculations for each measurement:
  -  Direct soil moisture sensors (SENTEK)
  -  Tensiometers
  -  OWASIS

# Overview
  -  SENTEK: the registered values are transformed back into scaled frequency (raw), and a linear correction is applied to calculate the water-filled pore space. The water-filled pore space is used to calculate the air-filled pore space, using the max water-filled pore space as the porosity.
  -  TENSIOMETER: the soil matric potential is intropolated to create a continous dataset for each 1 cm of soil. The Mualum van Genuchten equation is applied to the interpolated values to retrieve the soil moisture content. The air-filled pore space is calculated from the soil moisture content using the saturated water content.
  -  OWASIS: includes the a script that extracts the air-filled pore space values from the TIF files for the polder footprint, by calcuting the mean of a 3x3 pixel window.

# Extra
  -  OWASIS original script: Includes an additional OWASIS script that does not take a 3x3 window but a large polder
  -  AFPS profile: includes a script that creates a dataframe of the air-filled pore space per depth for more in depth analysis of the changes occuring per depth.
