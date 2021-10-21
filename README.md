# Stat628Module2_Group9

## Introduction
This project aims to come up with a model to estimate percentage of BodyFat using clinically avaliable measurements. 252 observations and 17 variables are included in the dataset.

## Contributions

Zhengyuan Wen built the GAM model and wrote R code for this model and 10-fold CV. He also
wrote the Final Model Selection part, Analysis and Strengths and Weakness part of the summary. And
he was in charge of Shiny code.

Hengrui Qu wrote the Introduction and Data Cleaning part of the summary and R code for data
cleaning, and was responsible for the whole PowerPoint.

Jiaying Jia built the stepwise model and lasso model, and wrote R code for both models and R code
for calculating RMSE. She also wrote Candidate Models and Motivations part of the summary and was
responsible for editing and polishing the article.

We met 5 times and spent 15 hours in discussion. And we each spent around 10 hours after the
discussion


## Files
`Code` has all the codes used in the project.

`Code/Shiny` has all the files for Shiny App.

`Data` includes both original `csv` files and `cleardata.csv`.

`Summary` contains our summary report.

`Presentation` contains the PDF file for our presentation.

`Image` includes the images used in this project.


## Shiny
You could directly click on the following link,
https://wennroy.shinyapps.io/shiny/

If the above link doesn't work, open the following file in `/Code/Shiny/shiny.R`.

Set the working directory as root directory in the second lines in `shiny.R`, which should look like this,

```R
rm(list=ls())
setwd("xxx/Stat628Module2_Group9")
```

Then type the following command,

```R
runApp('xxx/Stat628Module2_Group9/Code/Shiny/shiny.R')
```

in Rstudio. Then it would work. 

The opening UI interface will look like this,

<img src="https://i.imgur.com/qmdCH0V.png" alt="image" style="zoom: 67%;" />

Input the value in each column, (*represents not necessary information) then click the `Calculate you BodyFat` bottom. It will give you the result. And remember to  click `Clear Output` bottom before your next calculation.



The result will look like this,

<img src="https://i.imgur.com/H1FdLoo.png" style="zoom: 67%;" />

