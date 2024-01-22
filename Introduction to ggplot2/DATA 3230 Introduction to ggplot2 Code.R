## DATA 3230: Introduction to ggplot2 ##

library(tidyverse)

## Plotting Basic ggplot ##

install.packages('palmerpenguins')

penguins <- palmerpenguins::penguins

## Summary of Penguins dataset ##

penguins |>
  glimpse()

## Building the ggplot canvas ##

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm,
                     y = body_mass_g))

## Add a geom ##

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point()

## MPG Example ##

mpg <- ggplot2::mpg

p <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy))

p + labs(x = "Engine Displacement",
         y = "Highway MPG")

## Scatterplot by Group (Using the Color aesthetic) ##

## Examine MPG dataset ##

mpg |>
  glimpse()

## Differentiate Class by Color of Points! ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,color=class))

ggplot(data = mpg,mapping = aes(x = displ,y = hwy,color=class)) + 
  geom_point()

mpg |>
  ggplot(mapping = aes(x = displ,y = hwy,color=class)) + 
  geom_point()

## Differentiate Species also by Color of Points ##

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           color=species))

## Scatterplot by Group (Using the Size aesthetic) ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,size=class))

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           size=species))

## Scatterplot by Group (Using the alpha aesthetic) ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,alpha=class))

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           alpha=species))

## Scatterplot by Group (Using the Shape aesthetic) ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,shape=class))

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           shape=species))

## Change the color of the points ##

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),
             color='blue')

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm),
             color='#CE1141')

## Let's check out all the colors available in R ##

colors()

"http://sape.inf.usi.ch/quick-reference/ggplot2/colour"

## Have point shape be uniform ##

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm),
             shape = 11)

## Now, let's try a new visualization this time 
## comparing the frequency of vehicle class in the mpg
## data and species in the penguins data ##

## First, we have to get the data into the right format ##

new_mpg <- mpg |>
  dplyr::group_by(class) |>
  dplyr::count()

new_mpg

new_penguins <- penguins |>
  dplyr::group_by(species) |>
  dplyr::count() 

new_penguins

## Now, we can visualize the data using a different geom called
## geom_bar ##

new_penguins |>
  ggplot(aes(x=species,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Penguin Species",
       y = "Frequency",
       title = "Frequency of Penguin Species Observed")

new_mpg |>
  ggplot(aes(x=class,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Vehicle Class",
       y = "Frequency",
       title = "Frequency of Vehicle Class Observed")

## We can center justify the titles by: ##

new_penguins |>
  ggplot(aes(x=species,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Penguin Species",
       y = "Frequency",
       title = "Frequency of Penguin Species Observed") +
  theme(plot.title = element_text(hjust=0.50))

new_mpg |>
  ggplot(aes(x=class,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Vehicle Class",
       y = "Frequency",
       title = "Frequency of Vehicle Class Observed") +
  theme(plot.title = element_text(hjust=0.50))

## We can also change the overall theme of the plot
## by using a theme_ function. For instance, theme_minimal() ##

new_penguins |>
  ggplot(aes(x=species,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Penguin Species",
       y = "Frequency",
       title = "Frequency of Penguin Species Observed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.50)) 

## theme_classic() ##

new_penguins |>
  ggplot(aes(x=species,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Penguin Species",
       y = "Frequency",
       title = "Frequency of Penguin Species Observed") +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.50))

## theme_bw() ##

new_penguins |>
  ggplot(aes(x=species,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Penguin Species",
       y = "Frequency",
       title = "Frequency of Penguin Species Observed") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.50))

## There are also custom themes in other packages 
## we can make use of, too ##

library(ggthemes)

## theme_economist_white() ##

new_penguins |>
  ggplot(aes(x=species,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Penguin Species",
       y = "Frequency",
       title = "Frequency of Penguin Species Observed") +
  theme_economist_white() +
  theme(plot.title = element_text(hjust=0.50))

## Google docs theme ##

new_penguins |>
  ggplot(aes(x=species,y=n)) +
  geom_bar(stat='identity') +
  labs(x = "Penguin Species",
       y = "Frequency",
       title = "Frequency of Penguin Species Observed") +
  theme_gdocs() +
  theme(plot.title = element_text(hjust=0.50))
