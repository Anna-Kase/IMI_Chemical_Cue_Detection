

# Censored Data Model - https://bookdown.org/content/3686/tools-in-the-trunk.html

library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)

all_tbl <- read.csv("data/master_all_turtle_data.csv")

all_tbl$treatment <- as.factor(all_tbl$treatment)


swim_time <- all_tbl  %>% 
  group_by(Turtle, Trial, treatment, Species, experiment_group, Side, Behavior) %>%
  filter(Behavior == "Swimming") %>% 
  summarise(
    time = sum(total_seconds)
  ) 


# Setting up the censoring

# set censor threshold
t1 <- 300

# create data set where we have a column of values that are <= the threshold and a column
# that indicates if the value was censored or not
d <-
  swim_time %>% 
  mutate(time = as.double(time)) %>% 
  mutate(y1   = if_else(time > t1, t1, time),
         cen1 = if_else(time > t1, "right", "none"))

d

# create another column in the dataset that indicates NA for any censored values
d <-
  d %>% 
  mutate(y_na = ifelse(cen1 == "none", time, NA))

d

# censored model
mean_y <- mean(d$y_na, na.rm = T)
sd_y   <- sd(d$y_na, na.rm = T)

stanvars <- 
  stanvar(mean_y, name = "mean_y") + 
  stanvar(sd_y,   name = "sd_y")

inits <- list(Intercept = mean_y)

inits_list <- list(inits, inits, inits, inits)

# so this runs but I have no idea what it ran...
fit1 <-
  brm(data = d,
      family = hurdle_gamma(),
      y1 | cens(cen1) ~ 1 + ,
      prior = c(prior(normal(log(mean_y), 1), class = Intercept),
                prior(exponential(0.1), class = sigma)),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      # here we insert our start values for the intercept
      inits = inits_list)




d %>% 
  filter(Side == "Stimulus") %>%
  ggplot(aes(x = treatment, y = y1, color = Trial)) +
  geom_point(position = position_dodge(width = 0.2)) +
  facet_grid(Side~Species)



hist(d$y1)

side_only <- brm(time ~ 1 + Side + (Side|Turtle),
                 family = Gamma(link = "log"),
                 data = swim_time,
                 prior = c(prior(normal(4,2), class = "Intercept"),
                           prior(normal(0,1), class = "b"),
                           prior(exponential(0.1), class = "shape"),
                           prior(exponential(1), class = "sd")),
                 #sample_prior = "only",
                 cores = 4, chains = 1, iter = 1000)
