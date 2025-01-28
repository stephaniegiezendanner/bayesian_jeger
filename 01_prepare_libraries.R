# Additionally, make sure to have the following R libraries installed:
#   
# The Rstan package (warning, there are 2 steps to the installation: Configuring C++ toolchains, and then installation of Rstan)
# Rmarkdown
# Shiny
# tidyverse
# BRMS

# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")


# Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)


# example(stan_model, package = "rstan", run.dontrun = TRUE)

library("rstan") # observe startup messages
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# install.packages("rmarkdown")
library("rmarkdown")

# install.packages("shiny")
library("shiny")

library(tidyverse)

# install.packages("brms")
library("brms")
