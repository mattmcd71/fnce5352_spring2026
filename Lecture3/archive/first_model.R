library(modelr)

ggplot(diamonds) + geom_density(aes(x=log(price)))
ggplot(diamonds) + geom_density(aes(x=carat))


mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))
