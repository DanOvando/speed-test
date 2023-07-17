library(tidyverse)
library(tictoc)
library(rstan)
library(cmdstanr)
library(sf)
# library(TMB)
library(sdmTMB)
library(INLA)
library(rnaturalearth)
# library(VAST)
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")
tic()

# bayesian test -----------------------------------------------------------

# Generate some fake data
n <- 250000
k <- 20
X <- matrix(rnorm(n * k), ncol = k)
y <- rbinom(n, size = 1, prob = plogis(3 * X[,1] - 2 * X[,2] + 1))
mdata <- list(k = k, n = n, y = y, X = X)

mod_cl <- cmdstan_model("logistic.stan")

fit_cl <- mod_cl$sample(data = mdata, chains = 4, parallel_chains = 4, refresh = 100)

# mixed effects test ------------------------------------------------------

# Load package
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
example = load_example( data_set="EBS_pollock" )

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 100, 
                          Region = example$Region, 
                          purpose = "index2", 
                          bias.correct = TRUE )

# Run model
fit = fit_model( settings = settings, 
                 Lat_i = example$sampling_data[,'Lat'], 
                 Lon_i = example$sampling_data[,'Lon'], 
                 t_i = example$sampling_data[,'Year'], 
                 b_i = example$sampling_data[,'Catch_KG'], 
                 a_i = example$sampling_data[,'AreaSwept_km2'] )

# Plot results
plot( fit )

# mapping test ------------------------------------------------------------
mesh <- make_mesh(pcod, xy_cols = c("X", "Y"), cutoff = 10)
pcod$year_scaled <- as.numeric(scale(pcod$year))
fit <- sdmTMB(
  density ~ s(depth, k = 5) + year_scaled,
  spatial_varying = ~ year_scaled, 
  data = pcod, mesh = mesh, 
  time = "year",
  family = tweedie(link = "log"),
  spatiotemporal = "off"
)

toc()