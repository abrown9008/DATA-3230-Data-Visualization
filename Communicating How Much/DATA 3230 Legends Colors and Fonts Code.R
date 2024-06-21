## DATA 3230 How Much Code Part 2 ##

library(tidyverse)

## Let's start with our NYC Trash Horizontal Bar Chart ##

nyc <- readxl::read_xlsx("NYC Trash Data.xlsx")

## Subset to September 2011 ##

nyc_sept11 <- nyc |>
  dplyr::filter(MONTH == 9 & YEAR == 2011)

## Sum up REFUSETONSCOLLECTED variable by Borough ##

trash_tot <- nyc_sept11 |>
  dplyr::group_by(BOROUGH) |>
  dplyr::summarize(Sum_Trash = sum(REFUSETONSCOLLECTED))

## Visualization we left off with: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## If we want to add labels to this visualization to show how much
## trash was produced by each borough, we can use a new geom called
## geom_text: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=Sum_Trash)) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## Okay cool! But what are some problems? For starters,
## the label is centered at the end of the bar, making
## the text difficult to read. We can change the justification
## of the text by using the hjust argument: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=Sum_Trash),hjust=1) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## Rounding to one decimal point is also a bit unusual. We 
## can adjust this by using the round function within
## geom_text: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## What if we wanted to control the font in terms of size,
## color, and font type? Well, what options are available to us? 
## Let's see! ##

## R mono, bold-faced, & red ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='mono',
            fontface='bold',color='red',hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## serif, italicized, & NY Knicks Orange ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='italic',color='#F58426',hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## serif, bold.italic, & Braves red ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## We can add extra fonts by using the extrafont package: ##
## DON'T RUN THIS UNTIL AFTER CLASS...TAKES A LONG TIME 
## TO RUN!!! ##

install.packages('extrafont')

library(extrafont)

font_import()
loadfonts(device = "win")

## Now we can use something like, Old English Text MT: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='Old English Text MT',
            fontface='bold.italic',color='#CE1141',hjust=1.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## Using this last visualization, what if we wanted to move
## the labels to the outside of bars? ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic()

## Whoops! Now I can't see Brooklyn's label! This means I now 
## need to change the viewing window. One way is by increasing the 
## x-axis length. We can do this by using scale_x_continuous function: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  scale_x_continuous(limits = c(0,75000))

## Okay cool! That solved the problem! What if we want to 
## increase the number of tick marks to be in increments of 
## 10K? We can again use scale_x_continuous: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  scale_x_continuous(limits = c(0,75000),
                     breaks = seq(0,80000,by=10000))

## Now, to the point that was made in last class, we don't really
## need the three extra zeros indicating that we are working in the
## 10s of thousands. We could get by with 10K, 20K, and so on. We once
## again can use the scales package & 
## scale_x_continuous function to do so: ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  scale_x_continuous(limits = c(0,75000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

## We can also change the font of the axis titles to match
## our data labels using the theme function! ##

trash_tot |>
  ggplot(aes(x=Sum_Trash,y=reorder(BOROUGH,Sum_Trash))) +
  geom_bar(stat='identity',color='black',fill='white') +
  geom_text(aes(label=round(Sum_Trash)),family='serif',
            fontface='bold.italic',color='#CE1141',hjust=-0.25) +
  labs(y = "NYC Borough",
       x = "Total Refuse Collected (in tons)",
       title = "Trash Collected in NYC by Borough",
       subtitle = "September 2011") +
  theme_classic() +
  theme(axis.text.x = element_text(family = 'serif',
                                   face = 'bold.italic',
                                   color = '#CE1141'),
        axis.title.x = element_text(family = 'serif',
                                    face = 'bold.italic',
                                    color = '#CE1141'),
        axis.text.y = element_text(family = 'serif',
                                   face = 'bold.italic',
                                   color = '#CE1141'),
        axis.title.y = element_text(family = 'serif',
                                    face = 'bold.italic',
                                    color = '#CE1141')) +
  scale_x_continuous(limits = c(0,75000),
                     breaks = seq(0,80000,by=10000),
                     labels = scales::label_number(suffix = "K",
                                                   scale = 1e-3))

## Now it's your turn! Using everything we've learned so far, I want you 
## to use the nycflights13::flights dataset, which contains all of the data
## regarding flights departing from the 3 NYC airports for all of 2013. 
## The question I want answered using a visualization is:

## For flights departing in December, who were the top 5 carriers in
## terms of average departure delay? ##

nycflights13::flights |>
  glimpse()

## HINT: To get the carrier name rather than their FAA code, use:

df <- nycflights13::flights |>
  left_join(nycflights13::airlines, by = 'carrier')

df |>
  glimpse()
