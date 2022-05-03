# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("dplyr", "tidyverse", "readr", "tsibble", "ggplot2", "fpp3", "lubridate", "fable", "feasts", "fabletools", "slider", "imputeTS", "shiny", "anytime")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))