##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check modelsirt
# R CMD Rd2pdf modelsirt
# R CMD build modelsirt --resave-data
library(devtools)
library(roxygen2)
setwd("/mnt/WDRED_REMOTE/repositories/modelsirt/")
setwd("/mnt/WD500/public_rstatistics/")
# usethis::create_package("modelsirt")
document()
install()

