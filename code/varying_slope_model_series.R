
# load in packages and the dataset

library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)

all_tbl <- read.csv("data/master_all_turtle_data.csv")


# make treatment a factor rather than an integer
all_tbl$treatment <- as.factor(all_tbl$treatment)

# sort the dataset so we have time data for just the swimming behavior on just the stimulus side
# of the arena
swim_time <- all_tbl  %>% 
  filter(Behavior == "Swimming") %>% 
  filter(Side == "Stimulus") %>% 
  group_by(Turtle, Trial, treatment, Species, experiment_group, Side, Behavior) %>%
  summarise(
    time = sum(total_seconds)
  ) 


# set up the censored data

# set censor threshold
t1 <- 300

# create data set (d) where we have a column of values that are =< the threshold (y1) and a 
# column that indicates if the value was censored or not (cen1)
d <-
  swim_time %>% 
  mutate(time = as.double(time)) %>% 
  mutate(y1   = if_else(time > t1, t1, time),
         cen1 = if_else(time > t1, "right", "none"))

d

# create another column in the dataset that indicates NA for any censored values (y_na)
d <-
  d %>% 
  mutate(y_na = ifelse(cen1 == "none", time, NA))

d

# setting up the list of values (means and standard deviations from the censored data) to
# be used as the sampling start points for the model (in the original example these values
# were also used as the priors for a very basic (not great) model)
mean_y <- mean(d$y_na, na.rm = T)
sd_y   <- sd(d$y_na, na.rm = T)

stanvars <- 
  stanvar(mean_y, name = "mean_y") + 
  stanvar(sd_y,   name = "sd_y")

inits <- list(Intercept = mean_y)

inits_list <- list(inits, inits, inits, inits)

#censored model with a three way interaction and no varying slopes or intercepts
fit2 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ 1 + Species*treatment*Trial,
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway.RDS",
      file_refit = "on_change")

saveRDS(fit2, file="models/cens_threeway.RDS")


#plotting the posterior of fit2 model
fit2$data %>% 
  distinct() %>% 
  add_epred_draws(fit2) %>% 
  ggplot(aes(.epred))+
  geom_histogram()

fit2$data %>% 
  distinct() %>% 
  add_epred_draws(fit2) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)


# censored model with a three way interaction and just a varying intercept
fit3 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ Species*treatment*Trial + (1 | Turtle),
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway_varI.RDS",
      file_refit = "on_change")

saveRDS(fit3, file="models/cens_threeway_varI.RDS")


fit3$data %>% 
  distinct() %>% 
  add_epred_draws(fit3) %>% 
  ggplot(aes(.epred))+
  geom_histogram()

fit3$data %>% 
  distinct() %>% 
  add_epred_draws(fit3) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)


# censored model with no interactions and just a singular varying slope (by species)
# This model runs, and it gives very interesting predictions
fit4 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ 1 + Species+treatment+Trial + (Species | Turtle),
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape"),
                prior(exponential(0.1), class = "sd")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway_varS.RDS",
      file_refit = "on_change")

saveRDS(fit4, file="models/cens_threeway_varS.RDS")

fit4$data %>% 
  distinct() %>% 
  add_epred_draws(fit4) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)


# censored model with three way interactions and just a singular varying slope (by species)
# This model runs, and it gives very interesting predictions
fit5 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ 1 + Species*treatment*Trial + (Species | Turtle),
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape"),
                prior(exponential(0.1), class = "sd")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway_1varS.RDS",
      file_refit = "on_change")

saveRDS(fit5, file="models/cens_threeway_1varS.RDS")

fit5$data %>% 
  distinct() %>% 
  add_epred_draws(fit5) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)



# censored model with three way interactions and just a non-interaction varying 
#slope (by species+treatment)
# this one also runs
fit6 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ 1 + Species*treatment*Trial + (Species+treatment | Turtle),
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape"),
                prior(exponential(0.1), class = "sd")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway_no_interaction_varS.RDS",
      file_refit = "on_change")

saveRDS(fit6, file="models/cens_threewayno_interaction_varS.RDS")

fit6$data %>% 
  distinct() %>% 
  add_epred_draws(fit6) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)


# censored model with three way interactions and just a non-interaction varying 
#slope (by species+treatment+Trial)
# this one also runs
fit7 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ 1 + Species*treatment*Trial + (Species+treatment+Trial | Turtle),
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape"),
                prior(exponential(0.1), class = "sd")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway_no_interaction_3varS.RDS",
      file_refit = "on_change")

saveRDS(fit7, file="models/cens_threewayno_interaction_3varS.RDS")

fit7$data %>% 
  distinct() %>% 
  add_epred_draws(fit7) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)


# censored model with three way interactions and just a non-interaction varying 
#slope (by species+treatment+Trial) and a varying intercept
# this one also runs
fit8 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ 1 + Species*treatment*Trial + (1 + Species+treatment+Trial | Turtle),
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape"),
                prior(exponential(0.1), class = "sd")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway_no_interaction_3varS_varI.RDS",
      file_refit = "on_change")

saveRDS(fit8, file="models/cens_threewayno_interaction_3varS_varI.RDS")

fit8$data %>% 
  distinct() %>% 
  add_epred_draws(fit8) %>% 
  ggplot(aes(treatment, .epred, fill=Trial))+
  geom_boxplot()+
  facet_wrap(~Species)



# censored model with three way interactions and an interaction varying 
#slope  and a varying intercept
# this one does not run - it just says the model does not contain samples
fit9 <-
  brm(data = d,
      family = hurdle_gamma(link = "log"),
      y1 | cens(cen1) ~ 1 + Species*treatment*Trial + (1 + Species*treatment*Trial | Turtle),
      prior = c(prior(normal(4,2), class = "Intercept"),
                prior(normal(0,1), class = "b"),
                prior(exponential(0.1), class = "shape"),
                prior(exponential(0.1), class = "sd")),
      chains = 4, cores = 4,
      stanvars = stanvars, 
      init = inits_list,
      file = "models/cens_threeway_interaction_3varS_varI.RDS",
      file_refit = "on_change")

saveRDS(fit9, file="models/cens_threeway_interaction_3varS_varI.RDS")
