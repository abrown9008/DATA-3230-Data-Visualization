## Adding a Background Image to a Graph ##

## Let's add the MLB logo to the background of
## our scatterplot: ##

library(tidyverse)
library(ggimage)

## First, we have to get the data into the right
## format!! ##

mlb <- Lahman::Batting |>
  select(yearID,teamID,R,HR) |>
  filter(yearID == 2022) |>
  group_by(teamID) |>
  summarize(R = sum(R,na.rm=T),
            HR = sum(HR,na.rm=T))

## Save File Path of MLB Logo ##

pics <- list.files(path = "Multiple Quantities/",pattern='.jpg')

pics

mlb_logo <- paste("Multiple Quantities/",pics[1],sep="")

## Generate a Scatterplot ###

library(cowplot)
library(magick)
library(rsvg)

mlb |>
  ggplot(aes(x=R,y=HR)) +
  geom_point() +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit\n and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic() +
  theme_cowplot() -> mlb_plot

## Read image from source ##

mlb_image <- image_read_svg("https://upload.wikimedia.org/wikipedia/commons/a/a6/Major_League_Baseball_logo.svg")

## Add to image ##

ggdraw() +
  draw_image(mlb_image) +
  draw_plot(mlb_plot)

## We can change the size ##

ggdraw() +
  draw_image(mlb_image,scale=0.25) +
  draw_plot(mlb_plot)

## We can change the position ##

ggdraw() +
  draw_image(mlb_image,scale=0.25,
             halign = 0,
             valign = 0) +
  draw_plot(mlb_plot)

## Let's say for analytics day, I want to add the KSU logo 
## to my plot: ##

ksu_image <- image_read_svg("https://upload.wikimedia.org/wikipedia/commons/6/63/Kennesaw_State_Owls_logo.svg")

ggdraw() +
  draw_image(ksu_image,scale=0.25) +
  draw_plot(mlb_plot)
