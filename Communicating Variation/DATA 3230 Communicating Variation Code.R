## DATA 3230 - Communicating Variation Part 1 ##

library(tidyverse)

## Suppose I want to see how the average number 
## of strikeouts per nine innings pitched (SO/9) MLB 
## pitchers have thrown has changed, if at all, from 
## the 1910 season to the 2023 season for pitchers
## who have thrown more than 15 innings: ##

so9 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9) |>
  group_by(yearID) |>
  summarize(Mean_SO9 = mean(SO9,na.rm=T))

so9 |>
  glimpse()

## Now we can make the visualization. But instead of 
## geom_bar, we are going to use geom_point and geom_line: ##

so9 |>
  ggplot(aes(x=yearID,y=Mean_SO9)) +
  geom_point() +
  geom_line() +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2020 Regular Seasons") +
  theme_classic()

## This plot is well and good, but what if we wanted to 
## highlight the seasons in which we saw the greatest and 
## least amount of S09? We can do this with data annotations! ##

## The main thing to remember with annotations and ggplots in 
## general is that anything we want rendered on a graph which
## comes from data must be in a dataframe. So this means
## we need the x & y coordinates of the minimum and maximum
## SO9 in its own dataframe to make the annotations: ##

minmax <- so9 |>
  filter(Mean_SO9 == min(Mean_SO9) | Mean_SO9 == max(Mean_SO9))

minmax |>
  glimpse()

## So now we can plot! ##

so9 |>
  ggplot(aes(x=yearID,y=Mean_SO9)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=Mean_SO9),data=minmax) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()

## Okay this is problematic for a couple of reasons:
## (1) Too many digits; we need to round
## (2) We need to adjust the position of the labels
## to not be directly over the points. We can fix both 
## problems with relative ease: ##

so9 |>
  ggplot(aes(x=yearID,y=Mean_SO9)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=round(Mean_SO9,2)),
            vjust = -1,data=minmax) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_y_continuous(limits=c(2,11))

## Okay, so that problem is resolved. But what's another problem?
## The main one is: What in the world do these values represent and
## what years are they associated with? Let's fix the first problem.
## We can write out a sentence we want rendered for both the minimum
## and maximum values: ##

minmax <- minmax |>
  mutate(label = case_when(Mean_SO9 == min(Mean_SO9) ~ paste(yearID,"Lowest SO/9\nin 1910-2023"),
                           Mean_SO9 == max(Mean_SO9) ~ paste(yearID,"Highest SO/9\nin 1910-2023"))
  )

minmax |>
  glimpse()

## Cool, so now we can plot this text on the graph! ##

so9 |>
  ggplot(aes(x=yearID,y=Mean_SO9)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=label),
            vjust = -1,data=minmax) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()  +
  scale_y_continuous(limits=c(2,12),
                     breaks = seq(1,13)) +
  scale_x_continuous(limits = c(1910,2027))

## Better but still problems! Here, let's try to use a new 
## function within a package called ggrepel ##

library(ggrepel)

so9 |>
  ggplot(aes(x=yearID,y=Mean_SO9)) +
  geom_point() +
  geom_line() +
  geom_label_repel(aes(label=label),
                   family='serif',
                   color='turquoise',
                   fontface='bold',
                   size=3,
                   nudge_y = 1,
                   alpha = 0.8,
                   data=minmax) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()  +
  scale_y_continuous(limits=c(2,12),
                     breaks = seq(1,13)) +
  scale_x_continuous(limits = c(1910,2027))

## We can play around with the font in geom_label_repel
## like we did previously: ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_label_repel(aes(label=Text),
                   family = 'serif',
                   color = 'turquoise',
                   fontface = 'bold',
                   nudge_y = 1.5,
                   size = 3,
                   data=minmax2)+
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_y_continuous(limits = c(1,12),
                     breaks = seq(1,13))

## Time Series Plot with Standard Errors ##

## We can also visualize the within-year variation by
## adding standard error bars to our points! ##

## To do this we will reaggregate our data using rstatix ##

library(rstatix)

so9 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9) |>
  group_by(yearID) |>
  get_summary_stats(SO9,type='full') |>
  select(yearID,mean,se)

so9 |>
  glimpse()

## Now we can plot using geom_errorbar ##

so9 |>
  ggplot(aes(x=yearID,y=mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se)) +
  geom_line() +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()

## Boxplots ##

## For as wide of an x-axis as we have, it is hard to get 
## a sense of within year variation using the above method.
## Instead, it may be easier to achieve this goal using a boxplot ##

## However, to visualize a boxplot, we need the data to be in
## its raw form: ##

so9 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9)

so9 |>
  glimpse()

## Now, we can use geom_boxplot to visualize ##

so9 |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_boxplot(fill='white',color='black') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()

## Formatting Tick Marks ##

## Changing tick label angle ##

so9 |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_boxplot(fill='white',color='black') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45))

## Changing number of tick labels ##

so9 |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_boxplot(fill='white',color='black') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_x_discrete(breaks=seq(1910,2023,10))

## Half-Violin Plots ##

## With a half-violin plot, we can see the distribution of 
## SO9 for each season. We can also include the boxplot to
## help us visually identify the median and quartiles ##

library(see)

so9 |>
  filter(between(yearID,2015,2022)) |>
  ggplot(aes(x=factor(yearID),y=SO9)) +
  geom_violinhalf(fill='green') +
  geom_boxplot(width = 0.1, fill = 'white') +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 2015 & 2022 Regular Seasons") +
  theme_classic()