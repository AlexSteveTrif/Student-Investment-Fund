
# Okanagan College Student Investment Fund Analysis

## Overview
This repository contains the financial analysis and risk assessment models I developed as a Junior Analyst for the Okanagan College Student Investment Fund. The project showcases my work in applying Principal Component Analysis (PCA) for stock price predictions, automating data collection, and implementing a custom Value at Risk (VaR) and Expected Shortfall (ES) model for portfolio risk evaluation. The stocks I choose are placeholder stocks and do not represent any stock descions made my the Okanagan College Student Investment Fund. 

## Contents
- `weeklypullV3.0.R`: Script for automating the extraction and processing of weekly stock data.
- `VaR.engineV3.1.R`: Custom R function for calculating the Value at Risk and Expected Shortfall.
- `Project_Report.pdf`: Detailed project report including methodology, results, and conclusion.
- 'Portfolio Risk Assessment.Rmd': MArkdown file used to generate pdf file

## Installation
To run these scripts, you will need R and the following R packages: `tidyverse`, `quantmod`, `tidyquant`, `PerformanceAnalytics`, `ggplot2`, and `dplyr`. Install R from [CRAN](https://cran.r-project.org/) and the packages using the following commands in your R console:

```{r}
install.packages("tidyverse")
install.packages("quantmod")
install.packages("tidyquant")
install.packages("PerformanceAnalytics")
install.packages("ggplot2")
install.packages("dplyr")
```

## Usage
Each script can be run independently in an R environment. Ensure you set the correct working directory before running the scripts to process the stock data correctly.

```R
setwd("path_to_your_data_directory")
source("weeklypullV3.0.R")
source("VaR.engineV3.1.R")
```

Replace `path_to_your_data_directory` with the path to the directory containing your CSV data files.

## Contribution
This project is part of my professional portfolio and not open for public contribution. However, feedback and suggestions are welcome and can be submitted as issues within this repository.

## License
The content of this project itself is licensed under the [Creative Commons Attribution 4.0 license](https://creativecommons.org/licenses/by/4.0/), and the underlying source code used to format and display that content is licensed under the MIT license.

## Contact
For any further questions, you can reach out to me at [alextrif25@gmail.com](mailto:alextrif25@gmail.com).



