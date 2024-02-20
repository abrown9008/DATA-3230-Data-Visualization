## DATA 3230 - Communicating Variation Part 1 ##

library(tidyverse)

## Suppose I want to see how the average number 
## of strikeouts per nine innings pitched (SO/9) MLB 
## pitchers have thrown has changed, if at all, from 
## the 1910 season to the 2023 season for pitchers
## who have thrown more than 15 innings: ##

so9 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15)

## Then, we need to create a new variable called "SO9" 
## to calculate the number of strikeouts a given pitcher 
## had during a given season. 
## Note, the variable IPouts is the number of innings 
## pitched times 3: ##

so9 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9)

## Now we have to decide how we're going to visualize these
## data. A straightforward way may be by plotting the mean
## values for each year and then connecting them with a line
## in what's called a 'time series plot' ##

## First, we have to get the mean SO9 for each year: ##

so9 <- Lahman::Pitching |>
  filter(between(yearID,1910,2023) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9) |>
  group_by(yearID) |>
  summarize(SO9 = mean(SO9,na.rm=T))

## Now we can make the visualization. But instead of 
## geom_bar, we are going to use geom_point and geom_line: ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
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
  filter(SO9 == min(SO9) | SO9 == max(SO9))

## So now we can plot! ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=SO9),data=minmax) +
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
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=round(SO9,2)),
            vjust = -1,data=minmax) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()

## Okay, so that problem is resolved. But what's another problem?
## The main one is: What in the world do these values represent and
## what years are they associated with? Let's fix the first problem.
## We can write out a sentence we want rendered for both the minimum
## and maximum values: ##

minmax2 <- minmax |>
  mutate(Text = paste(round(SO9,2),", which was observed in ",
  yearID, ",\n was the",if_else(SO9 == min(SO9)," minimum", " maximum"), 
  " SO/9",
  sep = ""))

minmax2

## Cool, so now we can plot this text on the graph! ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=Text),
            vjust = -1,data=minmax2) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic()

## Well now we have big problems with how the text is rendering!! ##
## Let's change the y-axis length and see if that helps ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label=Text),
            vjust = -1,data=minmax2) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_y_continuous(limits = c(1,12),
                     breaks = seq(1,13))

## Better but still problems! Here, let's try to use a new 
## function within a package called ggrepel ##

library(ggrepel)

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_label_repel(aes(label=Text),data=minmax2) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_y_continuous(limits = c(1,12),
                     breaks = seq(1,13))

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

## Baseball was famously segregated prior to the 1947
## season when Jackie Robinson was the first Black player
## for the Brooklyn Dodgers. If we look on the graph,
## we can see that right about at the end of segregation,
## SO/9 started to go up. The segregated and integrated
## eras of MLB are worth highlighting. We can do this 
## with a couple of cool tools. First, is geom_area: ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_area(aes(y=SO9)) +
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
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(1,13))

## Notice I had to change the minimum of the yaxis to 0 ##

## Okay the color is completely opaque!! We can change this
## with alpha where 0 is totally transparent and 1 is
## totally opaque. Let's try 0.25 ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_area(aes(y=SO9),
            fill='blue',
            alpha = 0.25) +
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
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(1,13))

## Well, we need to draw a line to separate the two eras,
## right?? We can do this with annotate ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_area(aes(y=SO9),
            fill='blue',
            alpha = 0.25) +
  annotate("segment",x=1947,xend=1947,
           y = 0, 
           yend = so9 |>
                  filter(yearID == 1947) |>
                  select(SO9) |>
                  as.numeric()
           ) +
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
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(1,13))

## We can play around with the linetype too! ##
## We have twodash, solid, longdash, dotted, dotdash,
## and dashed: ##

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_area(aes(y=SO9),
            fill='blue',
            alpha = 0.25) +
  annotate("segment",x=1947,xend=1947,
           y = 0, 
           yend = so9 |>
             filter(yearID == 1947) |>
             select(SO9) |>
             as.numeric(),
           linetype = 'longdash'
  ) +
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
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(1,13))

## Now, we want to add another annotation mentioning what the 
## line is partitioning! Let's create another dataframe
## to do so ##

df2 <- tibble(yearID = c(1930,1987),
              SO9 = c(1.75,1.75),
              Text = c("Segregated Era",
                       "Integrated Era"))

so9 |>
  ggplot(aes(x=yearID,y=SO9)) +
  geom_point() +
  geom_line() +
  geom_area(aes(y=SO9),
            fill='blue',
            alpha = 0.25) +
  annotate("segment",x=1947,xend=1947,
           y = 0, 
           yend = so9 |>
             filter(yearID == 1947) |>
             select(SO9) |>
             as.numeric(),
           linetype = 'longdash'
  ) +
  geom_label_repel(aes(label=Text),
                   family = 'serif',
                   color = 'turquoise',
                   fontface = 'bold',
                   nudge_y = 1.5,
                   size = 3,
                   data=minmax2) +
  geom_label(aes(label = Text),
            fontface = 'bold',
                   data = df2) +
  labs(x="Season",
       y="Average Number of Strikeouts per 9 Innings Pitched",
       title="Changes in Strikeouts per 9 Innings Pitched",
       subtitle="Between the 1910 & 2023 Regular Seasons") +
  theme_classic() +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(1,13))
