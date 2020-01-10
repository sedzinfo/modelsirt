# Models IRT

A function that displays IRT model curves for various methods, using shiny apps.
This function was originally written by Wes Bonifay and Metin Bulus and it is here modified to include several models in one function call.
The original package can be found in CRAN under the name "irtDemo".
This function now uses plotly.
Thurstonian IRT curves were added with the help of Kostas Maistrelis.

# Installation Instructions

install.packages("devtools")

library(devtools)

install_github("sedzinfo/modelsirt")

# Usage

library(modelsirt)

modelsirt()
