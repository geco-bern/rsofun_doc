# FROM THE REVISION PLAN:
# Setup 1: global, reduced parameter set (as in initial manuscript version), only GPP as target
# Setup 2: global, full parameter set, only GPP as target
# Setup 3: global, full parameter set, GPP and traits as target
# We expect Setup 2 to yield wider posteriors than from Setup 1, and that posterior distributions will be narrowed again by Setup 3. This experimental design will allow us to demonstrate the robustness (or absence thereof) of the MCMC and the usefulness of using traits for simultaneously calibrating with fluxes.


# TODO: what will we do differently in revised?
#       Will we redo sensitivity of combined likelihood (i.e. gpp and traits)?
#       Will we keep the sensitivity of the gpp-likelihood only?

      # From 02_sensitivity_analysis.R:
      # # best parameter values (initial values)
      # par_cal_best <- c(
      #   kphio              = list(0.03,    0.09423773, 0.15),
      #   kphio_par_a        = list(-0.004,  0.0025,     0.001),
      #   kphio_par_b        = list(10,      20,         30),
      #   soilm_thetastar    = list(0,       0.6*240,    240),
      #   soilm_betao        = list(0,       0.2,        1),
      #   beta_unitcostratio = list(50.0,    146.0,      200.0),
      #   rd_to_vcmax        = list(0.01,    0.014,      0.1),
      #   tau_acclim         = list(7.0,     30.0,       60.0),
      #   kc_jmax            = list(0.2,     0.41,       0.8),
      #   error_gpp          = list(0.01     1           4),
      # )
