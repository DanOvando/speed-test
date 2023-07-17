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
color_scheme_set("brightblue")
tic()

# bayesian test -----------------------------------------------------------

# Generate some fake data
n <- 25000
k <- 20
X <- matrix(rnorm(n * k), ncol = k)
y <- rbinom(n, size = 1, prob = plogis(3 * X[,1] - 2 * X[,2] + 1))
mdata <- list(k = k, n = n, y = y, X = X)

mod_cl <- cmdstan_model("logistic.stan")

fit_cl <- mod_cl$sample(data = mdata, chains = 4, parallel_chains = 4, refresh = 100)

model <- stan_demo(model = 516, chains = 4, cores = 4)



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
plot( fit )

mesh <- make_mesh(pcod, xy_cols = c("X", "Y"), cutoff = 10)

d <- subset(pcod, year == 2017)
pcod_spde <- make_mesh(d, c("X", "Y"), cutoff = 30)
m <- sdmTMB(density ~ 0 + depth_scaled + depth_scaled2,
            data = d, mesh = pcod_spde, family = tweedie(link = "log"))
p <- predict(m, newdata = qcs_grid)


# mapping test ------------------------------------------------------------


eezs <-
  sf::read_sf(here("World_EEZ_v11_20191118_LR", "eez_v11_lowres.shp")) 

eezs |> 
  ggplot() + 
  geom_sf()

toc()

