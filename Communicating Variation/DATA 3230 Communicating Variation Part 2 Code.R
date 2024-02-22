## Communicating Variation Part 2 Code ##

library(tidyverse)
library(rstatix)

## What if we wanted to observe not only change
## over time, but also the variation within each
## individual year? To do this, we can plot not
## only the means, but also their standard errors.
## So we have to reaggregate our data to make this
## happen: ##

so91 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9) |>
  group_by(yearID) |>
  get_summary_stats(SO9,type='full')

so91 |>
  ggplot(aes(x=yearID,y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se)) +
  geom_line() +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()

## The problem with this though is that the standard errors
## are relatively small given the large sample sizes each year.
## Resultingly, a boxplot may be a better option. But with
## boxplots, we need the data to be in its raw format: ##

so92 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9)

so92 |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_boxplot(fill='white',color='black') +
  geom_point(data=so91,
             aes(x=factor(yearID),
                 y=mean),shape='x') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()

## But the text labels are all over the place! Let's see
## if changing the angle fixes this: ##

so92 |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_boxplot(fill='white',color='black') +
  geom_point(data=so91,
             aes(x=factor(yearID),
                 y=mean),shape='x') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90))

## Better, but still too much. We need to reduce the number
## of tick marks: ##

so92 |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_boxplot(fill='white',color='black') +
  geom_point(data=so91,
             aes(x=factor(yearID),
                 y=mean),shape='x') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_x_discrete(breaks=seq(1925,2020,by=25))

## Notice, tick marks don't necessarily have to be 
## evenly spaced: ##

so92 |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_boxplot(fill='white',color='black') +
  geom_point(data=so91,
             aes(x=factor(yearID),
                 y=mean),shape='x') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_x_discrete(breaks=c(1910,1947,1976,1990,2020))

## We can also use boxplots in non-time series ways. 
## For instance, let's examine the palmerpenguins
## dataset ##

penguins <- palmerpenguins::penguins |>
  na.omit()

## What if we wanted to compare mean body mass 
## between the species? We would use a one-way ANOVA
## model, right? But this time, let's use rstatix ##

mod <- penguins |>
  anova_test(body_mass_g~species)

## Determine Results: ##

mod

## Okay, so since p < 0.05, this means at least two
## groups may differ from each other. We can use
## Tukey's HSD to determine which pairs: ##

hsd <- penguins |>
  tukey_hsd(body_mass_g~species)

hsd

## Now, we can visualize all of this statistical information
## on a boxplot using ggpubr ##

library(ggpubr)

hsd <- hsd |>
  add_xy_position(x = "species")

penguins |>
  ggboxplot(x = "species", y = "body_mass_g") +
  stat_pvalue_manual(hsd, hide.ns = T) +
  labs(subtitle = get_test_label(mod, detailed = TRUE),
       caption = get_pwc_label(hsd),
       x = "Species",
       y = "Body Mass (in grams)",
       title = "A Comparison of Penguin Species Body Masses"
  )

