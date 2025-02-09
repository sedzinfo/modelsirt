##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check modelsirt
# R CMD Rd2pdf modelsirt
# R CMD build modelsirt --resave-data
library(devtools)
library(roxygen2)
directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
setwd(directory)
# usethis::create_package("modelsirt")
rm(list=c("modelsirt"))
document()
install()
library(modelsirt)
modelsirt()


