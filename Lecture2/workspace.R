library(tidyverse)

ggplot(mpg) +
   geom_point(aes(x=displ, y=hwy), alpha=0.2)

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, alpha=0.2))

library(nycflights13)

filter(flights, day == 1, month == 1)
filter(flights, day == 1 & month == 1)
filter(flights, day == 1 | month == 1)
filter(flights, day != 1 , month != 1)
filter(flights, !(day == 1 & month == 1))

filter(flights, day = 1)

by_month <- group_by(flights, month)
summarise(by_month, delay = mean(dep_delay, na.rm = TRUE))

          