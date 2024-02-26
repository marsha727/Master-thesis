# Non linear regression model

The script in this folder was used to fit different equation to describe the driving factors of night-time NEE. This includes three different equations:
  1.  The groundwater sigmoid dependency
  2.  The AFPS sigmoid dependency
  3.  The AFPS limited growth dependency
  4.  The AFPS bellcurve dependency

The file has the following structure:
  1.  NLS with grid search to fit different starting files to search for global minimum
  2.  Create subplots to visualize the fitted models: modelled night-time NEE against GWL/AFPS
  3.  AICc to compare the best model to predict night-time NEE
  4.  Goodness of fit of individual models: modeled NEE vs Observed NEE, and residual of models
