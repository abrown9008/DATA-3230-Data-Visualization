## DATA 3230 Communicating Association Between Quantitative
## Variables Code ##

library(tidyverse)

## What if we want to visualize the association between
## homeruns hit and runs scored by team for the 2022
## Major League Baseball regular season? ##

## First, we have to get the data into the right
## format!! ##

mlb <- Lahman::Batting |>
  select(yearID,teamID,R,HR) |>
  filter(yearID == 2022) |>
  group_by(teamID) |>
  summarize(R = sum(R,na.rm=T),
            HR = sum(HR,na.rm=T))

## Generate a Scatterplot ###

mlb |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## How do we interpret this? In general, I have students answer the following questions:
## What is the direction of the relationship (Positive/Negative)? ##
## What is the form of the relationship (Linear/Non-Linear)? ##
## What is the strength of the form of the relationship (weak, moderate, strong)? ##
## What unusual characteristics are exhibited (clusters, outliers, etc.)? ##

## We can add an arrow indicating the direction of the relationship using the
## annotate function! ##

mlb |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  annotate("segment",x = 575, xend = 715,
           y = 190, yend = 240, color = 'red',
           linewidth = 5, arrow = arrow()) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## What if we wanted to include the line of best fit
## (i.e., the simple linear regression line) on our plot? ##

## Adding the Line ##

mlb |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  geom_smooth(method='lm',se=F) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## Adding the Equation of the Line ##

library(ggpubr)

mlb |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  geom_smooth(method='lm',se=F) +
  stat_regline_equation(label.x = 625, label.y = 225) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## We can control the size ##

mlb |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  geom_smooth(method='lm',se=F) +
  stat_regline_equation(label.x = 625, label.y = 225,
                        size = 10) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## and color, too ##

mlb |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  geom_smooth(method='lm',se=F) +
  stat_regline_equation(label.x = 625, label.y = 225,
                        size = 10,color='#1E4D2B') +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## We can also identify individual teams using geom_label_repel like we did 
## with our time series plots! ##

library(ggrepel)

mlb |>
  left_join(Lahman::Teams |>
              filter(yearID == 2022) |>
              select(teamID,name) |>
              distinct()) |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  geom_smooth(method='lm',se=F) +
  geom_label_repel(aes(label=name)) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## Maybe we don't want all of these. Maybe just the top 3 HR and bottom 3 HR.
## How could we do that? Data structure!! ##

top3 <- mlb |>
  left_join(Lahman::Teams |>
              filter(yearID == 2022) |>
              select(teamID,name) |>
              distinct()) |>
  arrange(desc(HR)) |>
  head(3)

bottom3 <- mlb |>
  left_join(Lahman::Teams |>
              filter(yearID == 2022) |>
              select(teamID,name) |>
              distinct()) |>
  arrange(HR) |>
  head(3)

topbottom <- bind_rows(top3,bottom3)

mlb |>
  left_join(Lahman::Teams |>
              filter(yearID == 2022) |>
              select(teamID,name) |>
              distinct()) |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  geom_smooth(method='lm',se=F) +
  geom_label_repel(aes(label=name),data=topbottom) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2022 Regular Season") +
  theme_classic()

## What if we wanted to see how this relationship changed from 2019 - 2022?
## We can make use of facets to do this!! ##

## First, let's get our data in the right format: ##

mlb2 <- Lahman::Batting |>
  select(yearID,teamID,R,HR) |>
  filter(between(yearID,2019,2022)) |>
  group_by(teamID,yearID) |>
  summarize(R = sum(R,na.rm=T),
            HR = sum(HR,na.rm=T))

## Now, we can use our same base code for a scatterplot, but add
## a faceting function to it! ##

## Facet Grid ##

mlb2 |>
  ggplot(aes(x=R,y=HR)) + geom_point() +
  facet_grid(~yearID) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2019 - 2022 Regular Seasons") +
  theme_bw()

## We can also add regression lines to our facets as well :) ##

mlb2 |>
  ggplot(aes(x=R,y=HR,color=factor(yearID))) + geom_point() +
  geom_smooth(method = 'lm',se = F) +
  facet_grid(~yearID) +
  stat_regline_equation(label.y=300) +
  labs(x="Team Runs Scored",
       y="Team Homeruns Hit",
       title="Association Between Homeruns Hit and Runs Scored by MLB Team",
       subtitle="2019 - 2022 Regular Seasons") +
  theme_bw() +
  theme(legend.position = 'none')

## Your turn! Using the schrute::theoffice dataset, I want you to assess
## the association between the number of lines the character Jim has per episode
## with the number of lines the character Pam has per episode across seasons 1 - 5 ##

install.packages('schrute')

schrute::theoffice |>
  glimpse()
