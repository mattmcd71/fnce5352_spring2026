library(tidyverse)

ggplot(mpg) +
   geom_point(aes(x=displ, y=hwy), alpha=0.2)

ggplot(mpg) +
  geom_point(aes(x=displ, y=hwy, alpha=0.2))