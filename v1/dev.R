#' @name dev.R
#' @title Package Development & Testing Workflow
#' @author Tim Fraser, PhD
#' @description R script for building our R package. Produces a `.tar.gz` file for our package!
#'

# Assume working directory is the project/package directory.

# Unload package if present
unloadNamespace(ns = "policyoptimizer"); rm(list = ls()); remove.packages("policyoptimizer")

# Build the package. 
# (Adjust path. My R files are all located within an /rstudio/ folder. Probably different on your computer.)
setwd(paste0(rstudioapi::getActiveProject(), "/v1"))

# Document the package
devtools::document()

# Build the package!
devtools::build(path = getwd(), vignettes = FALSE)

# Unload it if present
unloadNamespace(ns = "policyoptimizer"); rm(list = ls()); remove.packages("policyoptimizer")

# Install package from source
install.packages("policyoptimizer_0.1.0.tar.gz", type = "source")

# Restart R
.rs.restartR() # Only works in RStudio
