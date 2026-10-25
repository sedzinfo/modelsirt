# Interactive IRT Model Visualization

## Overview

**modelsirt** is an R package that provides an interactive Shiny-based application for visualizing Item Response Theory (IRT) model curves. This package is designed for researchers, educators, and practitioners working with psychometric models, offering an intuitive way to explore and understand how different IRT models behave under various parameter configurations.

The package allows users to interactively manipulate model parameters and instantly see how these changes affect item characteristic curves (ICCs), category response curves (CRCs), and item information functions. This makes it an excellent educational tool for teaching IRT concepts and a practical tool for model exploration and parameter sensitivity analysis.

## Features

**modelsirt** provides interactive visualizations for multiple IRT models, including:

- **Rasch Model**: The fundamental dichotomous IRT model with equal discrimination parameters
- **1PL, 2PL, 3PL, and 4PL Models**: Progressively more complex logistic models for binary items
  - 1PL: One-parameter logistic model (difficulty only)
  - 2PL: Two-parameter logistic model (difficulty and discrimination)
  - 3PL: Three-parameter logistic model (adds lower asymptote/guessing parameter)
  - 4PL: Four-parameter logistic model (adds upper asymptote/carelessness parameter)
- **Graded Response Model (GRM)**: For ordinal polytomous items with ordered categories
- **Partial Credit Model (PCM)**: For ordinal items where categories represent partial credit
- **Rating Scale Model (RSM)**: A constrained version of PCM with common threshold spacings across items
- **Generalized Partial Credit Model (GPCM)**: Extends PCM to include item discrimination parameters
- **Nominal Response Model (NRM)**: For unordered categorical responses
- **Thurstonian IRT Model**: For forced-choice items and comparative judgments

### Key Capabilities

- **Interactive Parameter Adjustment**: Use sliders to modify item parameters in real-time
- **Multiple Visualization Types**: View item characteristic curves, category response curves, and information functions
- **Modern Plotting**: Built with Plotly for interactive, publication-quality graphics
- **Educational Tool**: Perfect for teaching IRT concepts and exploring model behavior
- **Research Application**: Useful for parameter sensitivity analysis and model comparison

## Background and Attribution

This package is a fork and significant enhancement of the original **irtDemo** package by **Wes Bonifay** and **Metin Bulus**, which is available on CRAN. The current version has been substantially modified and expanded by **Dimitrios Zacharatos** to:

- Consolidate multiple IRT models into a single unified interface
- Modernize visualizations using Plotly for enhanced interactivity
- Add support for additional IRT models
- Improve the user interface and experience

Special thanks to [Kostas Maistrelis](https://github.com/maisk) for contributing the implementation of Thurstonian IRT curves.

**Original Authors**: Wes Bonifay and Metin Bulus  
**Current Maintainer**: Dimitrios Zacharatos  
**Original CRAN Package**: [irtDemo](https://cran.r-project.org/package=irtDemo)

## Installation

### Prerequisites

1. **Install R** (version 4.0 or higher recommended)  
   Download from: https://cran.r-project.org/

2. **Install RStudio** (optional but highly recommended)  
   Download from: https://posit.co/downloads/

### Package Installation

Open RStudio (or R console) and run the following commands:

```r
# Install devtools if not already installed
if (!require("devtools")) {
  install.packages("devtools")
}

# Install modelsirt from GitHub
devtools::install_github("sedzinfo/modelsirt")
```

The package will automatically install its dependencies:
- `shiny`: For the interactive web application
- `plotly`: For interactive plotting
- `magrittr`: For pipe operators

## Usage

Using **modelsirt** is straightforward:

```r
# Load the package
library(modelsirt)

# Launch the interactive Shiny application
modelsirt()
```

This will open an interactive web application in your default browser where you can:

1. Select the IRT model you want to explore from the navigation menu
2. Adjust item parameters using the interactive sliders
3. Observe how the item characteristic curves change in real-time
4. Explore different scenarios and parameter combinations
5. Use the visualizations for teaching or presentations

### Example Workflow

```r
library(modelsirt)

# Launch the app
modelsirt()

# In the app:
# 1. Navigate to the "2PL Model" tab
# 2. Adjust the discrimination parameter (α) to see how it affects curve steepness
# 3. Adjust the difficulty parameter (β) to see how it shifts the curve
# 4. Switch to "3PL Model" to explore the guessing parameter
# 5. Compare different models by switching between tabs
```

## Screenshots

### Rasch 1PL 2PL 3PL 4PL Model
![Rasch Model](https://raw.githubusercontent.com/sedzinfo/modelsirt/refs/heads/master/images/modelsirt1.png)

### Partial Credit Model Generalized Partial Credit Model
![2PL Model](https://raw.githubusercontent.com/sedzinfo/modelsirt/refs/heads/master/images/modelsirt2.png)

### Graded Response Model
![3PL Model](https://raw.githubusercontent.com/sedzinfo/modelsirt/refs/heads/master/images/modelsirt3.png)

### Rating Scale Model Generalized Rating Scale Model
![Graded Response Model](https://raw.githubusercontent.com/sedzinfo/modelsirt/refs/heads/master/images/modelsirt4.png)

### Nominal Response Model
![Partial Credit Model](https://raw.githubusercontent.com/sedzinfo/modelsirt/refs/heads/master/images/modelsirt5.png)

### TIRT Model
![Rating Scale Model](https://raw.githubusercontent.com/sedzinfo/modelsirt/refs/heads/master/images/modelsirt6.png)

### Multidimensional Models
![Nominal Response Model](https://raw.githubusercontent.com/sedzinfo/modelsirt/refs/heads/master/images/modelsirt7.png)

## Use Cases

**modelsirt** is valuable for:

- **Education**: Teaching IRT theory in psychometrics courses
- **Research**: Exploring model behavior and parameter sensitivity
- **Test Development**: Understanding item characteristics during test construction
- **Model Selection**: Comparing different IRT models for your data
- **Presentations**: Creating interactive demonstrations of IRT concepts
- **Learning**: Self-study and exploration of psychometric models

## Contributing

Contributions are welcome! If you'd like to contribute to **modelsirt**, please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/YourFeature`)
3. Commit your changes (`git commit -m 'Add some feature'`)
4. Push to the branch (`git push origin feature/YourFeature`)
5. Open a Pull Request

## Issues and Support

If you encounter any issues or have questions:

- Open an issue on [GitHub Issues](https://github.com/sedzinfo/modelsirt/issues)
- Provide a reproducible example when reporting bugs
- Check existing issues before creating a new one

## License

This package is licensed under the GNU General Public License (GPL) version 2 or later. See the [LICENSE](LICENSE) file for details.

## Citation

If you use **modelsirt** in your research or teaching, please cite both the original work and this package:

```
Zacharatos, D. (2026). modelsirt: Interactive IRT Model Visualization. 
R package version 0.0.0.9000. https://github.com/sedzinfo/modelsirt

Original work by:
Bonifay, W., & Bulus, M. (2021). irtDemo: Item Response Theory Demo Collection. 
R package. https://CRAN.R-project.org/package=irtDemo
```

## Acknowledgments

- **Wes Bonifay** and **Metin Bulus** for creating the original irtDemo package
- **Kostas Maistrelis** ([@maisk](https://github.com/maisk)) for implementing Thurstonian IRT curves
- The R community for the excellent packages that make this work possible

## References

For those interested in learning more about IRT:

- Embretson, S. E., & Reise, S. P. (2000). *Item Response Theory for Psychologists*. Lawrence Erlbaum Associates.
- Baker, F. B., & Kim, S.-H. (2017). *The Basics of Item Response Theory Using R*. Springer.
- de Ayala, R. J. (2009). *The Theory and Practice of Item Response Theory*. Guilford Press.

---

![Stars](https://img.shields.io/github/stars/sedzinfo/modelsirt)
![Watchers](https://img.shields.io/github/watchers/sedzinfo/modelsirt)
![Repo Size](https://img.shields.io/github/repo-size/sedzinfo/modelsirt)
![Open Issues](https://img.shields.io/github/issues/sedzinfo/modelsirt)
![Forks](https://img.shields.io/github/forks/sedzinfo/modelsirt)
![Last Commit](https://img.shields.io/github/last-commit/sedzinfo/modelsirt)
![Contributors](https://img.shields.io/github/contributors/sedzinfo/modelsirt)
![License](https://img.shields.io/github/license/sedzinfo/modelsirt)
![Workflow Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/modelsirt/build-tarball.yml)

