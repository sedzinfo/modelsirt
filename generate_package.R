##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check modelsirt
# R CMD Rd2pdf modelsirt
# R CMD build modelsirt --resave-data
library(devtools)
library(roxygen2)
rm(list = ls())
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)
# usethis::create_package("modelsirt")
document()
if ("modelsirt" %in% rownames(installed.packages())) remove.packages("modelsirt")
install()
library(modelsirt)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=modelsirt.pdf")
if (.Platform$OS.type == "windows") shell.exec("modelsirt.pdf") else system("evince modelsirt.pdf")
system("R CMD build . --resave-data")

modelsirt() # starts the app: the console stays busy until you close it
