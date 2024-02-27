## DATA 3230 - Introduction to Control Charts Code ##

library(tidyverse)

## Read in Data ##

library(readxl)

jeans <- read_xlsx("Blue Jeans.xlsx")

jeans |>
  glimpse()

## Get means for each time point and plot this 
## data in a time series! ##

jeans2 <- bind_cols(
  
  jeans |>
    select(Hour),
  
  apply(jeans[,-1],1,mean)
  
)

names(jeans2)[2] <- "X-Bar"         

jeans2 |>
  glimpse()

jeans2 |>
  ggplot(aes(x=Hour,y=`X-Bar`)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 34,color='red',linetype='dashed') +
  labs(x = "Hour",
       y = "Sample Mean Inseam Length (in inches)",
       title = "Mean Jeans Inseam Length") +
  theme_classic()

## Okay, if we want to obtain control limits, we first have
## to calculate our mean standard deviation: ##

sbar <- mean(apply(jeans[,-1],1,sd))

## Since n = 10, c4 = 0.9727 ##

c4 <- 0.9727

## If L = 3, then we can calculate the Upper and Lower control limits: ##

L <- 3

UCL <- 34 + L*sbar/(c4*sqrt(10))
LCL <- 34 - L*sbar/(c4*sqrt(10))

## Now adding this info to our plot: ##

jeans2 |>
  ggplot(aes(x=Hour,y=`X-Bar`)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 34,color='red',linetype='dashed') +
  geom_hline(yintercept = UCL,color='black',linetype='longdash') +
  geom_hline(yintercept = LCL,color='black',linetype='longdash') +
  labs(x = "Hour",
       y = "Sample Mean Inseam Length (in inches)",
       title = "Mean Jeans Inseam Length") +
  theme_classic()

## How to obtain the same graph using qcc: ##

install.packages('qcc')

library(qcc)

qcc(jeans[,-1],type="xbar",center=34,std.dev=sbar/c4)

## Now, using the NYC Flights 13 data, suppose I use average daily flight departure
## delay as my variable of interest for Delta Airlines ##

## Next, use these values to calculate the average delay per week for Jan - June ##

## Hint: ##

library(lubridate)

nycflights13::flights |>
  mutate(Week = week(as_date(paste(year,month,day,sep="-"))))

## Now estimate the overall mean to use as the target value and also estimate 
## the process standard deviation (use n = 7) ##

## Finally, calculate the control limits and plot. Do we have visual evidence
## to suggest an in-control process? ##