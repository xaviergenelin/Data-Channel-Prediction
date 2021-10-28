# Data Channel Prediction

## Purpose

The purpose of this project is to analyze the [Online News Popularity Data Set](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity) from the UCI Machine Learning Repository using predictive models in R. We'll automate the process of analyzing 6 different data channels within the data set that involves exporatory data analysis and predictive modeling for the number of shares. 

## Required Packages

The following libraries are used in this project:

-   `tidyverse`: read in the data, general data manipulation and plotting  
-   `caret`: splitting the data, preproccess and resampling methods, testing and creating predictive models   
-   `corrplot`: correlation plot for variables  
-   `doParellel`: parallel computing to speed up code  
-   `rmarkdown`: rendering the .Rmd files

## Data Channel Files

Each of the data channels has their own analysis and results.

- [Bus Channel Analysis](Reports/Bus.md)
- [Entertainment Channel Analysis](Reports/Entertainment.md)
- [Lifestyle Channel Analysis](Reports/Lifestyle.md)
- [Socmed Channel Analysis](Reports/Socmed.md)
- [Tech Channel Analysis](Reports/Tech.md)
- [World Channel Analysis](Reports/World.md)

