## Heatmapping Using ggplot2 ##

library(tidyverse)

## A heatmap can be used to visualize a data matrix 
## by representing each matrix entry by a color corresponding 
## to its magnitude, enabling the user to visually process 
## large datasets with thousands of rows and/or columns. ##

## Let's consider an example. Suppose we want to use 
## the NYC Flights data to determine the mean flight departure
## delay per month by carrier. ##

## First, let's get the data into the right format:

flights <- nycflights13::flights

flights2 <- flights |>
  select(carrier,dep_delay,month) |>
  group_by(month,carrier) |>
  summarize(MD = mean(dep_delay,na.rm=T))

## Now, let's visualize :) ##

flights2 |>
  ggplot(aes(x = month, y = carrier, fill = MD)) +
  geom_tile()

## What do we notice right away? First, OO (SkyWest Airlines) doesn't 
## have observations for some of the months. Let's omit: ##

flights2 |>
  filter(carrier != "OO") |>
  ggplot(aes(x = month, y = carrier, fill = MD)) +
  geom_tile()

## Nice! Now what else should change? Maybe we add the actual 
## airline name and manipulate the axes? ##

flights2 |>
  filter(carrier != "OO") |>
  left_join(nycflights13::airlines,
            by = join_by(carrier)) |>
  ggplot(aes(x = month, y = name, fill = MD)) +
  geom_tile() +
  labs(x = "Month",
       y = "Airline Name",
       fill = "Mean Departure\nDelay") +
  theme_minimal()

## Looks better! How about fixing the month to where it
## actually shows the name of the month? ##

library(lubridate)

flights2 |>
  filter(carrier != "OO") |>
  left_join(nycflights13::airlines,
            by = join_by(carrier)) |>
  mutate(Date = as_date(paste("2013-",month,"-1",sep="")),
         M1 = month(Date,label=T)) |>
  ggplot(aes(x = M1, y = name, fill = MD)) +
  geom_tile() +
  labs(x = "Month",
       y = "Airline Name",
       fill = "Mean Departure\nDelay") +
  theme_minimal()

## Without Abbrevations: ##

flights2 |>
  filter(carrier != "OO") |>
  left_join(nycflights13::airlines,
            by = join_by(carrier)) |>
  mutate(Date = as_date(paste("2013-",month,"-1",sep="")),
         M1 = month(Date,label=T,abbr=F)) |>
  ggplot(aes(x = M1, y = name, fill = MD)) +
  geom_tile() +
  labs(x = "Month",
       y = "Airline Name",
       fill = "Mean Departure\nDelay") +
  theme_minimal()

## What if we want to change the palatte? Remember, we learned how
## to do this already!! Let's see how it applies here: ##

library(viridis)

flights2 |>
  filter(carrier != "OO") |>
  left_join(nycflights13::airlines,
            by = join_by(carrier)) |>
  mutate(Date = as_date(paste("2013-",month,"-1",sep="")),
         M1 = month(Date,label=T,abbr=F)) |>
  ggplot(aes(x = M1, y = name, fill = MD)) +
  geom_tile() +
  labs(x = "Month",
       y = "Airline Name",
       fill = "Mean Departure\nDelay") +
  theme_minimal() +
  scale_fill_viridis(option="A")

flights2 |>
  filter(carrier != "OO") |>
  left_join(nycflights13::airlines,
            by = join_by(carrier)) |>
  mutate(Date = as_date(paste("2013-",month,"-1",sep="")),
         M1 = month(Date,label=T,abbr=F)) |>
  ggplot(aes(x = M1, y = name, fill = MD)) +
  geom_tile() +
  labs(x = "Month",
       y = "Airline Name",
       fill = "Mean Departure\nDelay") +
  theme_minimal() +
  scale_fill_viridis(option="D")

## We can add our own scale, too! ##

flights2 |>
  filter(carrier != "OO") |>
  left_join(nycflights13::airlines,
            by = join_by(carrier)) |>
  mutate(Date = as_date(paste("2013-",month,"-1",sep="")),
         M1 = month(Date,label=T,abbr=F)) |>
  ggplot(aes(x = M1, y = name, fill = MD)) +
  geom_tile() +
  labs(x = "Month",
       y = "Airline Name",
       fill = "Mean Departure\nDelay") +
  theme_minimal() +
  scale_fill_gradient(low="red",high='green')

## Now, I want to use theoffice dataset to visually evaluate the
## average number of lines per episode per season of the main characters
## in the Office (Michael, Jim, Pam, Dwight, Andy, Angela, Kevin, Ocsar,
## Creed, Kelly, Ryan, Phyllis, Meredith, Stanley, & Toby) ##