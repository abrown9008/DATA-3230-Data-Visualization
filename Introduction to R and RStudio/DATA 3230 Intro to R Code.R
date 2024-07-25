## DATA 3230 - Introduction to R Code ##

## R as a Calculator ##

## Add ##

2 + 2

## Subtract ##

2 - 2

## Multiply ##

2*2 

## Divide ##

2/2 

## Raise to a Power ##

2^2

## or ##

2**2

## I can save 2 + 2 as "a" ##

a <- 2 + 2

a

## R is case sensitive so big A isn't
## the same as little a ##

A

b <- "DATA 3230"

b

## Importing a CSV File ##

## First, install the readr package ##

install.packages('readr')

## What are the arguments for read_csv? ##

?readr::read_csv

## Go Ahead and Read it in ##

library(readr)
heart <- read_csv("Introduction to R and RStudio/HEART.csv")

## Importing an XLSX File ##

## Installing the 'readxl' package ##

install.packages('readxl')
library(readxl)

## Importing the 'esoph' dataset ##

esoph <- read_xlsx("Introduction to R and RStudio/esoph.xlsx")

## We can explore some characteristics of our datasets
## using the dplyr::glimpse function! ##

## We have to first install the dplyr package ##

install.packages('dplyr')
library(dplyr)

heart |>
  glimpse()

## Find the mean of AgeAtDeath ##

mean(heart$AgeAtDeath)

## Check the documentation for the mean function ##

?mean

## Try Again Omitting the NA Values ##

mean(heart$AgeAtDeath,na.rm=TRUE)

## Subset last four column of Heart ##

heart_status <- heart |>
  select(Chol_Status,BP_Status,
         Weight_Status,Smoking_Status) 

heart_status |>
  glimpse()

## Subset Overweight Participants ##

heart_status_ow <- heart_status |>
  filter(Weight_Status == 'Overweight')

