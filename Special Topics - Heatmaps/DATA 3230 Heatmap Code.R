## Heatmapping Using ggplot2 ##

library(tidyverse)

## Suppose we want to see how certain MLB teams have performed
## in terms of wins over the previous 30 seasons (1992 - 2022).

## Before we start thinking about visualizations, let's first
## get the data into the right format: ##

library(Lahman)

nleast <- Teams |>
  filter(between(yearID,1992,2022) &
           teamID %in% c("ATL","PHI","NYN")) |>
  select(yearID,teamID,W)

nleast |>
  glimpse()

## We could use a stacked our grouped bar chart to communicate
## this information. Let's try a vertical grouped bar chart: ##

nleast |>
  ggplot(aes(x = yearID, y = W, fill = teamID)) +
  geom_bar(stat = 'identity', 
           position = position_dodge()) +
  theme_classic()

## This is way too busy to be effective! Let's instead try 
## a heatmap. ##

## A heatmap can be used to visualize a data matrix 
## by representing each matrix entry by a color corresponding 
## to its magnitude, enabling the user to visually process 
## large datasets with thousands of rows and/or columns. ##

## Here, we will use geom_tile: ##

nleast |>
  ggplot(aes(x = yearID,y = teamID)) +
  geom_tile(color='black') +
  geom_text(aes(label=W),color='white') +
  theme_classic()

## This is better but we can make it even better! ##

nleast |>
  ggplot(aes(x = yearID,y = teamID)) +
  geom_tile(aes(fill=W)) +
  theme_classic()

## We can change the color palette. For example, if we 
## want to use option A in viridis: ##

library(viridis)

nleast |>
  ggplot(aes(x = yearID,y = teamID)) +
  geom_tile(aes(fill=W)) +
  theme_classic() +
  scale_fill_viridis(option="A")