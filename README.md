# Self-compassion and Demographic Data Analysis

This repository contains an analysis of a dataset designed to explore the relationship between self-compassion and demographic data.

## Project Overview

The analysis focuses on various demographic factors and their relationship to self-compassion. The data is processed, cleaned, and analyzed using R. Various statistical methods and visualizations are employed to understand the data better.

## Installation

To run the analysis, ensure you have the following R packages installed:

```r
install.packages(c("haven", "dplyr", "tidyr", "devtools", "sjlabelled", "stringr", "ggplot2", "car", "corrplot", "e1071", "caTools", "class", "fastDummies", "party", "magrittr", "rpart", "rpart.plot", "lsr"))
devtools::install_github("strengejacke/sjlabelled")
```

## Data Import and Cleaning

The dataset is imported from an Excel file and cleaned by removing missing values and duplicates. Key steps include:

- Importing the dataset using `read_excel()`.
- Removing missing values and duplicates with `drop_na()` and `distinct()`.
- Processing specific columns and handling categorical data.

## Data Processing

The data processing involves:

- Subsetting and combining relevant columns.
- Reversing the scales of specific columns.
- Creating new factors based on the processed data.

## Analysis

The analysis includes:

- Grouping and summarizing data by various demographic factors.
- Performing statistical tests like ANOVA and t-tests.
- Visualizing data with box plots and other plots using `ggplot2`.

## Machine Learning Models

The project also explores machine learning models like KNN and decision trees to predict self-compassion based on demographic factors.

## Usage

To run the analysis, open the `data_analysis.R` script and execute the code in an R environment. The script includes detailed comments and steps to guide you through the analysis.

## Contributing

Contributions are welcome. Please fork the repository and submit a pull request with your changes.

## License

This project is licensed under the MIT License.

For more details, refer to the [data_analysis.R](https://github.com/amirmsk/self_compation_demographicas/blob/main/data_analysis.R) script.
