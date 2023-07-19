  library(tidyverse)
  library(tictoc)
  library(rstan)
  library(cmdstanr)
  library(sf)
  library(sdmTMB)
  library(INLA)
  library(rnaturalearth)
  library(VAST)
  library(cmdstanr)
  check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
  library(posterior)
  library(bayesplot)
  library(here)
  library(glue)
  color_scheme_set("brightblue")
  
  start_time <- Sys.time()
  set.seed(42)
  # bayesian test -----------------------------------------------------------
  
  # Generate some fake data
  n <- 200000
  k <- 20
  X <- matrix(rnorm(n * k), ncol = k)
  y <- rbinom(n, size = 1, prob = plogis(3 * X[,1] - 2 * X[,2] + 1))
  mdata <- list(k = k, n = n, y = y, X = X)
  
  mod_cl <- cmdstan_model("logistic.stan")
  
  fit_cl <- mod_cl$sample(data = mdata, chains = 1, parallel_chains = 1, refresh = 100)
  
  fit_cl$time()
  
  b = Sys.time()
  model <- stan_demo(model = 516, chains = 4, cores = 4)
  gp_time = Sys.time() - b
  
  gp_time
  # mixed effects test ------------------------------------------------------
  
  # Load package
  
  # load data set
  # see `?load_example` for list of stocks with example data 
  # that are installed automatically with `FishStatsUtils`. 
  example = load_example( data_set="EBS_pollock" )
  
  # Make settings (turning off bias.correct to save time for example)
  settings = make_settings( n_x = 100, 
                            Region = example$Region, 
                            purpose = "index2", 
                            bias.correct = FALSE )
  
  # Run model
  fit = fit_model( settings = settings, 
                   Lat_i = example$sampling_data[,'Lat'], 
                   Lon_i = example$sampling_data[,'Lon'], 
                   t_i = example$sampling_data[,'Year'], 
                   b_i = example$sampling_data[,'Catch_KG'], 
                   a_i = example$sampling_data[,'AreaSwept_km2'] )
  
  # Plot results
  # plot( fit )
  
  mesh <- sdmTMB::make_mesh(pcod, xy_cols = c("X", "Y"), cutoff = 10)
  # 
  d <- subset(pcod, year == 2017)
  pcod_spde <- sdmTMB::make_mesh(d, c("X", "Y"), cutoff = 30)
  m <- sdmTMB(density ~ 0 + depth_scaled + depth_scaled2,
              data = d, mesh = pcod_spde, family = tweedie(link = "log"))
  p <- predict(m, newdata = qcs_grid)
  
  
  spt_time <- Sys.time()
  fit_spatiotemporal <- sdmTMB(
    density ~ s(depth, k = 5), 
    data = pcod, 
    mesh = mesh,
    time = "year",
    family = tweedie(link = "log"), 
    spatial = "off", 
    spatiotemporal = "ar1"
  )
  sdmtmb_time <- Sys.time() - spt_time
  
  
  tmb_timer <- Sys.time()
  
  TMB::runExample(all = TRUE, clean = TRUE)
  
  tmb_time <- Sys.time() - tmb_timer
  
  # mapping test ------------------------------------------------------------
  
  map_start <- Sys.time()
  
  eezs <-
    sf::read_sf(here("World_EEZ_v11_20191118", "eez_v11.shp")) 
  
  eez_map <- eezs |> 
    ggplot() + 
    geom_sf()
  
  ggsave("eez_test.pdf", plot = eez_map)
  
  map_end <- Sys.time() - map_start
  
  map_end
  
  total_time <- Sys.time() - start_time
  
  total_time
