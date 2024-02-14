if (!require("devtools", quietly = TRUE)) install.packages("devtools")

try(detach("package:goFlux", unload = TRUE), silent = TRUE)
devtools::install_github("Qepanna/goFlux")
